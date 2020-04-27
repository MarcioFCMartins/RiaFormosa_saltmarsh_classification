# Mapping Blue Carbon - RIAVALUE
# Create saltmarsh mask 
# Márcio Martins - November 2019
# marciomartinsred@gmail.com

library(ClusterR)       # KMeans_rcpp
library(raster)         # Raster data methods
library(tidyverse)  
library(raster)
library(sf)


# K-means -----------------------------------------------------------------

# Create raster where data will be stored later
raster <- raster::brick("./3_data/raster_data/merged_rasters_clipped&scaled.tif")
names(raster) <- c("r", "g", "b", "nir", "elev", "slope", "ndvi")
# # 8 clusters
# raster_data <- as.data.frame(raster)
# 
# # Elev and NDVI seem to give the best result
# predictors <- c("r", "g", "b", "nir", "elev", "slope", "ndvi")
# # Apply K-means
# k <- 20
# start_time <- Sys.time()
# k_means <- KMeans_rcpp(raster_data[complete.cases(raster_data),],
#                    clusters  = k,
#                    max_iters = 10,
#                    num_init  = 1,
#                    seed = 42)
# end_time <- Sys.time()
# 
# end_time - start_time
# 
# 
# 
# # Create cluster map ------------------------------------------------------
# # Create a numeric vector with length equal to number of cells in original raster
# clusters <- as.numeric(rep(NA, nrow(raster_data)))
# 
# # Assign created clusters to non-NA raster cells
# clusters[complete.cases(raster_data)] <- k_means$clusters
# 
# 
# # Create empty raster with same properties as original one
# map <- raster::raster(raster)
# 
# # Set values in new raster based on cluster vector
# map <- raster::setValues(map, clusters)
# 
# # Save cluster raster
# writeRaster(map,
#             paste0("./5_outputs/kmeans-classification/",
#                   k,
#                   paste(predictors, collapse = "-"),
#                   "3.tif"),
#             overwrite = TRUE
#             )



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
