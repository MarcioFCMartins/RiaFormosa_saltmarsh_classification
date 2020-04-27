# Ria Formosa saltmarsh mapping:
# Estimates the position for each sampling quadrat along the transects
# Marcio Martins - July 2019
# marciomartinsred@gmail.com

# Settings ----------------------------------------------------------------
library(readxl) # Read xlsx
library(openxlsx) # Write and format xlsx
library(dplyr) # Data wrangling
library(tidyr) # Data wrangling
library(sf) # Spatial feature manipulation
library(purrr) # Functional programming tools

options(stringsAsFactors = FALSE)

source("./code/helper_functions.R")
# Load data ---------------------------------------------------------------
# Proj4 string for CRS PT-TM06/ETRS89
pt_crs <- "+proj=tmerc +lat_0=39.66825833333333 +lon_0=-8.133108333333334 +k=1 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs "
# Proj4 string for CRS WGS84
wgs84_crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "

# Start and end point coordinates of each transect
points <- read_xlsx("./data/monipor_clean_data.xlsx",
  sheet = "saltmarsh_data"
) %>%
  dplyr::filter(water_system == "Ria Formosa") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = wgs84_crs) %>%
  st_transform(crs = pt_crs)

# Transect lengths
saltmarsh_lengths <- read_xlsx("./data/monipor_clean_data.xlsx",
  sheet = "saltmarsh_summary"
) %>%
  dplyr::filter(water_system == "Ria Formosa") %>%
  dplyr::select(transect_id, name, length)

# Identifying information for individual trasects
transect_info <- read_xlsx("./data/monipor_clean_data.xlsx",
  sheet = "saltmarsh_data"
) %>%
  dplyr::filter(water_system == "Ria Formosa", start_end == "Start") %>%
  dplyr::select(1:6)

# Get transect orientation ------------------------------------------------


# Reshape data so the x and y of start and end points are individual columns
orientations <- points %>%
  select(-notes) %>%
  cbind(st_coordinates(points)) %>%
  st_drop_geometry() %>%
  gather(variable, value, X, Y) %>%
  unite(temp, start_end, variable) %>%
  spread(temp, value)

# Get orientations relative to north (vector c(1,0))
orientations <- orientations %>%
  mutate(
    delta_x = End_X - Start_X,
    delta_y = End_Y - Start_Y
  ) %>%
  mutate(theta = map2_dbl(delta_x, delta_y, calc_orientation)) %>%
  select(transect_id, theta)

# Estimate quadrat starting point -----------------------------------------
# Every quadrat was placed so that it began at the recorded meter
#   i.e. quadrat 5 started at meter 5
# The last quadrat also extended beyond the recorded GPS point
#   (this was accounted for when calculating transect length by adding 1 meter
#   to recorded value)

# This also means that we can get the quadrat starting points by creating
# k - 2 points, where k = transect length - this does not include the first
# and last quadrat

# Convert to a line format
lines <- points %>%
  group_by(name) %>%
  summarize(
    water_system = unique(water_system),
    water_body = unique(water_body),
    site_nr = unique(site_nr),
    transect_nr = unique(transect_nr),
    transect_id = unique(transect_id)
  ) %>%
  st_cast(to = "LINESTRING") %>%
  left_join(saltmarsh_lengths)

# Get information needed for segments (number and length of segments)
transect_geometry <- lines %>%
  # Get length of segment, so that transect is equally divided into k segments
  mutate(
    gps_length = map_dbl(geometry, st_length),
    segment_number = (length - 1),
    segment_length = gps_length / segment_number
  ) %>%
  st_drop_geometry() %>%
  select(transect_id, gps_length, segment_number, segment_length) %>%
  left_join(orientations)

# Add the starting point to our segment info
starting_points <- points %>%
  filter(start_end == "Start")

transect_geometry <- transect_geometry %>%
  left_join(starting_points %>% select(transect_id)) %>%
  st_as_sf(crs = pt_crs)

#  Move the coordinates to columns
transect_geometry <- cbind(
  transect_geometry,
  st_coordinates(transect_geometry)
) %>%
  st_drop_geometry()

# Break transects into segments (see helper function)
quadrat_points <- lapply(split(transect_geometry, transect_geometry$transect_id),
  FUN = function(x) {
    break_vector(
      name = x$transect_id,
      start_x = x$X,
      start_y = x$Y,
      theta = x$theta,
      length = x$gps_length,
      n_segments = x$segment_number
    )
  }
)

# Reoorganize all the points
quadrat_points <- do.call(rbind, quadrat_points) %>%
  rename(transect_id = name) %>%
  st_as_sf(coords = c("x", "y"), crs = pt_crs) %>%
  arrange(transect_id, quad_index)


# Create polygons for quadrats --------------------------------------------
quadrats <- quadrat_points %>%
  cbind(st_coordinates(quadrat_points)) %>%
  st_drop_geometry() %>%
  left_join(orientations) %>%
  # Draw quadrat points, with theta, edge size = 1, X and Y
  mutate(geometry = pmap(
    list(theta, 1, X, Y),
    draw_quadrat
  )) %>%
  st_as_sf(crs = pt_crs) %>%
  left_join(transect_info) %>%
  select(
    water_system, water_body, site_nr, transect_nr, transect_id, name,
    quad_index, geometry
  )


# Save the quadrats polygons as GeoPackage - like a modern shapefile 
# In my case, it does not limit number of characters for a feature name
st_write(
  quadrats,
  "./outputs/quadrats_raw/quadrats_raw.gpkg")
