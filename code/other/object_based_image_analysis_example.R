# Mapping saltmarsh in the Ria Formosa
# An example of how an object oriented workflow might work
# Márcio Martins - November 2019
# marciomartinsred@gmail.com


# Settings ----------------------------------------------------------------

library(raster)     # Raster data methods
library(sf)         # Spatial vector data
library(OpenImageR) # Image manipulation
source("./code/contrast_stretch.R")

# Data --------------------------------------------------------------------

raster <- raster::brick("./data/raster_data/raster_satellite/sat_clip.tif")
names(raster) <- c("r", "g", "b", "nir")

# Tiling ------------------------------------------------------------------
# Segmenting the whole raster in one go is not possible due to memory limitations
# The option is segmenting tiles individually and rebuild the raster
# Load raster with data

tiles <- sf::st_make_grid(
  x = sf::st_as_sfc(sf::st_bbox(raster)),
  cellsize = 1000,
  what = "polygons"
)

# Plot the tiles and raster
# plotRGB(raster)
# plot(tiles, add = TRUE)
# text(
#   x = st_coordinates(st_centroid(tiles))[, 1],
#   y = st_coordinates(st_centroid(tiles))[, 2],
#   labels = 1:length(tiles),
#   cex = 0.5
# )

# Create super-pixels -----------------------------------------------------
# In the future, pre-processing such as contrast enhancment or
# converting image to false color might produce better results

# Test tile - with values in 0-255 range for openimageR::superpixels
test_tile <- crop(
  raster,
  sf::as_Spatial(tiles[13])) %>%
  subset(c(1,2,3)) %>%
  contrast_stretch()

# Currently, contrast_stretch converts data to 0-255 range
# test_tile <- scale(
#   test_tile,
#   center = FALSE,
#   scale = raster::cellStats(test_tile, max))
# 
# test_tile <- test_tile * 255

# Set all nas to zero
test_tile[is.na(test_tile)] <- 0

test_tile <- as.array(test_tile)

# https://r-posts.com/analyzing-remote-sensing-data-using-image-segmentation/
superpixels <- superpixels(
  input_image = test_tile,
  method = "slic",
  superpixel = 100,
  compactness = 20,
  return_slic_data = TRUE,
  return_labels = TRUE,
  write_slic = "",
  verbose = FALSE
)

plot_spx <- grDevices::as.raster(NormalizeObject(superpixels$slic_data))
par(mfrow = c(1, 1))
plot(plot_spx)

# Add the superpixel labels to original raster - for grouping cells
labels <- raster::raster(tile_falsecolor) %>%
  setValues(values = superpixels$labels)
