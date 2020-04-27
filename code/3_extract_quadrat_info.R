# Mapping Blue Carbon - RIAVALUE
# Extract information from raster files for each quadrat that will be used for model training
# Márcio Martins - November 2019
# marciomartinsred@gmail.com

# Settings ----------------------------------------------------------------
library(raster)
library(sf)
library(dplyr)

# Load data ---------------------------------------------------------------
quadrats <- st_read("./outputs/quadrats_species/quadrats_species.gpkg") %>%
  # Remove quadrats without vegetation
  filter(cluster != "unvegetated")

raster_data <- raster::brick("./data/raster_data/merged_rasters.tif") %>%
  raster::subset(subset = c(1, 2, 5:9)) # Remove aspect and flow direction
names(raster_data) <- c("elev", "slope", "ndvi", "nir", "r", "g", "b")

# Extract quadrat information ---------------------------------------------
# Extract the elevation value at the centroid of each quadrat, 
# Uses bilinear interpolation to downsample 
quadrat_info <- raster::extract(raster_data,
  st_centroid(quadrats),
  method = "bilinear"
)

# Add dem and slope information to dataset
quadrats <- quadrats %>%
  cbind(quadrat_info) %>%
  select(
    water_body, 
    site_nr, 
    transect_nr, 
    transect_id, 
    quad_index, 
    cluster,
    elev,
    slope,
    ndvi,
    nir,
    r,
    g,
    b)


# Save data ---------------------------------------------------------------
st_write(
  quadrats,
  "outputs/quadrats_train_features/quadrats_train_features.gpkg"
)
