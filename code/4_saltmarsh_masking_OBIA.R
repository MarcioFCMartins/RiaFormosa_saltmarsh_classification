# Mapping Blue Carbon - RIAVALUE
# Create saltmarsh mask 
# Márcio Martins - November 2019
# marciomartinsred@gmail.com

library(ClusterR)       # KMeans_rcpp
library(raster)         # Raster data methods
library(tidyverse) 
library(raster)
library(sf)
library(OpenImageR)
library(GSIF)
library(rgdal)
library(rgeos)


# Tiling ------------------------------------------------------------------

# Load raster with data
raster <- raster::brick("./data/raster_data/raster_satellite/sat_clip.tif")
names(raster) <- c("r", "g", "b", "nir")

tiles <- GSIF::getSpatialTiles(
    rgdal::GDALinfo("./data/raster_data/raster_satellite/sat_clip.tif"),
    block.x = 1000,
    return.SpatialPolygons = TRUE)

# Plot the tiles and raster
plotRGB(raster)
plot(tiles, add = TRUE)
rgeos::polygonsLabel(
    tiles,
    labels = c(1:length(tiles)),
    method = "centroid",
    doPlot = TRUE,
    cex = 0.5
)


# Process tiles -----------------------------------------------------------
# Select tile 13 to use for testing, change to false color
# NIR = Red; Red = Green; Green = Blue
# This makes vegetation stand out
tile_falsecolor <- crop(
    raster,
    tiles[13]) %>%
    subset(subset = c(4,1,2))

# Normalize values and convert to a maximum of 255 range
maxs <- c(
    max(tile_falsecolor[,,1], na.rm = TRUE),
    max(tile_falsecolor[,,2], na.rm = TRUE),
    max(tile_falsecolor[,,3], na.rm = TRUE)
)

tile_norm <- tile_falsecolor %>%
    scale(center = FALSE, scale = maxs)

tile_norm <- tile_norm * 255

rgb_array <- as.array(tile_norm)

# Perform linear stretching for contrast enhancing
# https://github.com/rspatial/raster/blob/master/R/stretch.R
dims <- dim(rgb_array)
rgb_stretch_array <- array(dim = dims)

for(i in 1:3){
    layer <- rgb_array[,,i]
    layer <- as.vector(layer)
    v <- stats::quantile(layer, c(0.02, 0.98), na.rm = TRUE)
    temp <- (255 * (layer - v[1])) / (v[2] - v[1])
    temp[temp < 0] <- 0
    temp[temp > 255] <- 255
    rgb_stretch_array[,,i] <- array(temp, dim = dims[1:2])
}

# Add back the NAs as max, for plotting
rgb_stretch_array[is.na(rgb_stretch_array)] <- 255
rgb_array[is.na(rgb_array)] <- 255

# Test if my implementation worked 
# (left: mine, right: raster's - top: no stretching, bottom: stretched)
par(mfrow = c(2,2))
plot(grDevices::as.raster(rgb_array, max = 255))
plotRGB(tile_norm)
plot(grDevices::as.raster(rgb_stretch_array, max = 255))
plotRGB(tile_norm, stretch = "lin")



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
raster_data <- as.data.frame(raster)
