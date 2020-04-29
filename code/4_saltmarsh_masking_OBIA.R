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

tiles <- sf::st_make_grid(
    x = st_as_sfc(st_bbox(raster)),
    cellsize = 1000,
    what = "polygons") 

# Plot the tiles and raster
plotRGB(raster, stretch = "lin")
plot(tiles, add = TRUE)
text(
    x = st_coordinates(st_centroid(tiles))[,1],
    y = st_coordinates(st_centroid(tiles))[,2],
    labels = 1:length(tiles),
    cex = 0.5)


# Process tiles -----------------------------------------------------------
# Select tile 13 to use for testing, change to false color
# NIR = Red; Red = Green; Green = Blue
# This makes vegetation stand out

tile_falsecolor <- crop(
    raster,
    as_Spatial(tiles[13])) %>%
    subset(subset = c(4,1,2))

# If any cell in the tile is not NA, process the tile. 
if(any(!is.na(values(tile_falsecolor)[,1]))) {
    # Normalize values 
    maxs <- c(
        max(tile_falsecolor[,,1], na.rm = TRUE),
        max(tile_falsecolor[,,2], na.rm = TRUE),
        max(tile_falsecolor[,,3], na.rm = TRUE)
    )
    
    tile_norm <- tile_falsecolor %>%
        scale(center = FALSE, scale = maxs)
    
    # Convert to 255 range
    tile_norm <- tile_norm * 255
    
    # Perform linear stretching for contrast enhancing
    # https://github.com/rspatial/raster/blob/master/R/stretch.R
    rgb_array <- as.array(tile_norm)
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
    
    # Add back the NAs as white, for plotting
    # In the future, all perfect white cells are NA
    rgb_stretch_array[is.na(rgb_stretch_array)] <- 255
    rgb_array[is.na(rgb_array)] <- 255
} else {
    # Give message saying the tile is empty
    message("Tile number", X,"is empty. Skipping tile")
}
# Test if my implementation worked 
# (left: mine, right: raster's - top: no stretching, bottom: stretched)
par(mfrow = c(2,2))
plot(grDevices::as.raster(rgb_array, max = 255),
     main = "Non stretched false color, self implemented")
plotRGB(tile_norm,
        main = "Non stretched false color, raster implemented")
plot(grDevices::as.raster(rgb_stretch_array, max = 255),
     main = "Stretched false color, self implemented")
plotRGB(tile_norm, stretch = "lin",
        main = "Stretched false color, raster implemented")



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

writeRaster(labels, "D:/Desktop/teste-FC.tif", overwrite=TRUE)
