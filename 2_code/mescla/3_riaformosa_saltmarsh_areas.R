# Monipor - estimate Ria Formosa saltmarsh areas

# Marcio Martins - October 2019
# marciomartinsred@gmail.com

# Settings ----------------------------------------------------------------
library(readxl)   # Read xlsx
library(openxlsx) # Write and format xlsx 
library(raster)   # Work with raster data in R
library(tidyverse)# Data wrangling, plotting, etc
library(sf)       # Spatial feature manipulation 
library(RStoolbox)# Remote sensing tools

options(stringsAsFactors = FALSE)

# Load data ---------------------------------------------------------------
# Get quadrats and quadrat features
source("./4_code/2_quadrat_features.R")

# Proj4 string for CRS PT-TM06/ETRS89
pt_crs <- "+proj=tmerc +lat_0=39.66825833333333 +lon_0=-8.133108333333334 +k=1 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs "
# Proj4 string for CRS WGS84
wgs84_crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "

# LIDAR data
terrain <- raster::brick("D:/Documentos/CCMAR/Resources&Tools/LIDAR bathymetry/dem_slope_aspect_flowdir.tif") %>%
  raster::subset(subset = c(1,2))  # Only keep dem and slope
projection(terrain) <- pt_crs
names(terrain) <- c("dem", "slope")

# Shapefiles of wetlands to use as a mask
mask <- st_read("./3_data/riaformosa_saltmarshes_shapefile/Saltmarsh.shp") %>%
  st_transform(pt_crs) %>%
  as_Spatial()

# Shapefile of water bodies to use as mask
water_body_mask <- st_read("./2_planning_recourses/sampling_zones/ria_formosa_waterbodies/Waterbodies.kml") %>%
  st_transform(pt_crs) %>%
  # Polygons come with Z dimension - drop it
  st_zm()

# Classification model ----------------------------------------------------

# Uncomment to re-run. Output has been saved in 
#   "./5_outputs/riaformosa_saltmarsh_areas/riaformosa_saltmarshes.tif"

# # Keep only important variables and convert to Spatial for RStoolbox
# classification_data <- quadrat_polygons %>%
#   filter(cluster != "unvegetated") %>%
#   select(dem, cluster) %>%
#   as_Spatial()
# 
# # Define classification model
# # Uses radial kernel support vector machine
# # 5-fold validation, .8 split 
# class_model <- superClass(img = terrain,
#                           trainData = classification_data,
#                           polygonBasedCV = TRUE,
#                           trainPartition = 0.8,
#                           minDist = 0,
#                           responseCol = "cluster", 
#                           model = "svmRadial",
#                           kfold = 5,
#                           predict = FALSE,
#                           tuneLength = 30,
#                           mode = "classification")
# 
# # Create a prediction map of all the Ria Formosa
# # This map can't identify areas that are unvegetated:
# #   - a mask will be applied later
# map <- predict(class_model,
#                 terrain,
#                 predType = "raw",
#                 filename = "./5_outputs/riaformosa_saltmarsh_areas/riaformosa_saltmarshes.tif",
#                 overwrite = TRUE)


# Mask classification map -------------------------------------------------

# Uncomment to re-run. Output has been saved in 
#   "./5_outputs/riaformosa_saltmarsh_areas/riaformosa_saltmarshes.tif"

# map <- raster::raster("./5_outputs/riaformosa_saltmarsh_areas/riaformosa_saltmarshes.tif")
# 
# map_masked <- raster::mask(x = map, 
#                    mask = mask)
# 
# writeRaster(map_masked,
#             "./5_outputs/riaformosa_saltmarsh_areas/riaformosa_saltmarshes_clipped.tif")


# Separate raster by water body -------------------------------------------

# Could not get this to work properly (getting NA for all cells in most water bodies)
# and asked Richard to do it manually. Files can be found in:
# ./5_outputs/riaformosa_saltmarsh_areas

