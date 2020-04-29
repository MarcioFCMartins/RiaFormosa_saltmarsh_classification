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

predictors <- c("r", "g", "b", "nir", "elev", "slope", "ndvi")
# Apply K-means to create 20 clusters
k <- 20
start_time <- Sys.time()
k_means <- KMeans_rcpp(raster_data[complete.cases(raster_data),],
                   clusters  = k,
                   max_iters = 10,
                   num_init  = 1,
                   seed = 42)
end_time <- Sys.time()

end_time - start_time



# Create cluster map ------------------------------------------------------
# Create a numeric vector with length equal to number of cells in original raster
clusters <- as.numeric(rep(NA, nrow(raster_data)))

# Assign created clusters to non-NA raster cells
clusters[complete.cases(raster_data)] <- k_means$clusters


# Create empty raster with same properties as original one
map <- raster::raster(raster)

# Set values in new raster based on cluster vector
map <- raster::setValues(map, clusters)

# Save cluster raster
writeRaster(map,
            paste0("./5_outputs/kmeans-classification/",
                  k,
                  paste(predictors, collapse = "-"),
                  "3.tif"),
            overwrite = TRUE
            )



# Filter clusters ---------------------------------------------------------

# Manual inspection of the layer indicates that the following clusters
# are saltmarsh: 1, 3, 7, 9, 11, 16, 18, 19, 20
# WARNING, THIS SCRIPT WAS ORIGINALLY RAN WITHOUT A SEED 
# Because of this, we are only re-using the original cluster classification raster
# this is why most of the script is commented out: to protect the original clusters
full_clusters <- raster::raster("./5_outputs/kmeans-classification/ria_clusters-20all.tif")

marsh_clusters <- c(1, 3, 4, 7, 9, 11, 13, 16, 18, 19, 20)

# Create copy of cluster map
saltmarsh_mask <- full_clusters

# Set values of non-marsh clusters to NA
saltmarsh_mask[!(values(saltmarsh_mask) %in% marsh_clusters)] <- NA

# Set values of marsh clusters to 1
saltmarsh_mask[values(saltmarsh_mask) %in% marsh_clusters] <- 1

# Save result
writeRaster(saltmarsh_mask,
            "./5_outputs/kmeans-classification/saltmarsh_clusters.tif",
            overwrite = TRUE)


# Clean up mask -----------------------------------------------------------

# The clusters belonging to saltmarsh still had some issues
# Polygons were drawn which overlap the more problematic areas
cleanup_mask <- st_read("./5_outputs/kmeans-classification/cleanup_shapefile/cleanup_shapefile.shp")

# Remove areas covered by cleanup_mask
saltmarsh_mask_final <- mask(x = saltmarsh_mask,
                             mask = cleanup_mask,
                             inverse = TRUE)

# Re-sample this raster to 3x3 resolution - this was somehow lost in the clipped raster data
target_raster <- raster::brick("./3_data/raster_data/merged_rasters.tif")
saltmarsh_mask_final <- raster::resample(saltmarsh_mask_final, target_raster)

# Save final raster
writeRaster(saltmarsh_mask_final,
            "./5_outputs/kmeans-classification/saltmarsh_mask.tif",
            overwrite = TRUE)
