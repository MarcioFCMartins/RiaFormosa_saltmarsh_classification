# Mapping Blue Carbon - RIAVALUE
# Create saltmarsh mask 
# Márcio Martins - November 2019
# marciomartinsred@gmail.com

library(ClusterR)       # KMeans_rcpp
library(raster)         # Raster data methods
library(tidyverse) 
library(sf)
library(OpenImageR)

# Tiling ------------------------------------------------------------------

# Load raster with data
raster <- raster::brick("./data/raster_data/raster_satellite/sat_clip.tif")
names(raster) <- c("r", "g", "b", "nir")

tiles <- sf::st_make_grid(
    x = sf::st_as_sfc(sf::st_bbox(raster)),
    cellsize = 1000,
    what = "polygons") 

# Plot the tiles and raster
plotRGB(raster)
plot(tiles, add = TRUE)
text(
    x = st_coordinates(st_centroid(tiles))[,1],
    y = st_coordinates(st_centroid(tiles))[,2],
    labels = 1:length(tiles),
    cex = 0.5)

# This tile will be used for testing purposes
test_tile <- crop(
    raster,
    sf::as_Spatial(tiles[13])) 


# Process tiles -----------------------------------------------------------
# Select tile 13 to use for testing, change to false color
# In the future, this section will a function which is mapped to the whole image
# And will have to extract the parameters for stretching from the whole image
# NIR = Red; Red = Green; Green = Blue -
# This makes vegetation stand out

test_tile <- crop(
    raster,
    sf::as_Spatial(tiles[13])) %>%
    subset(subset = c(1,2,3))

# Create super-pixels -----------------------------------------------------
# https://r-posts.com/analyzing-remote-sensing-data-using-image-segmentation/
superpixels <- superpixels(
    input_image = rgb_stretch_array,
    method = "slic", 
    superpixel = 100,
    compactness = 20,
    return_slic_data = TRUE,
    return_labels = TRUE, 
    write_slic = "",
    verbose = FALSE)

plot_spx <- grDevices::as.raster(NormalizeObject(superpixels$slic_data))
par(mfrow = c(1,1))
plot(plot_spx)

labels <- raster::raster(tile_falsecolor) %>%
    setValues(values = superpixels$labels)

