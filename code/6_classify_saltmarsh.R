# Mapping Blue Carbon - RIAVALUE
# Classify saltmarsh type - create final map
# Márcio Martins - December 2019
# marciomartinsred@gmail.com


# Settings ----------------------------------------------------------------
library(raster)   # Raster operations and manipulation
library(caret)    # Wrapper for several machine learning packages
library(dplyr)    # Data wrangling
library(tidyr)    # Data wrangling
library(sf)       # Simple-features - spatial data 
library(recipes)  # Data pre-processing for modelling

# Load data ---------------------------------------------------------------

# Raster data of the Ria Formosa
raster_data <- raster::brick("./3_data/raster_data/merged_rasters.tif") %>%
  raster::subset(subset = c(1,2,5:9)) # Remove aspect and flow direction
names(raster_data) <- c("elev", "slope", "ndvi", "nir", "r", "g", "b")

# Best classification model
model <- readRDS("./5_outputs/saltmarsh-classification-models/models-list.RDS")[["svm_linear"]]

# Data pre-processing recipe
recipe <- readRDS("./5_outputs/saltmarsh-classification-models/data_recipe.RDS")

# Saltmarsh presence/absence mask
mask <- raster::raster("./5_outputs/kmeans-classification/saltmarsh_mask.tif")

# Remove cells outside of the determined saltmarsh mask
# WARNING: This step is very slow
raster_data <- mask(raster_data, mask)

# Create prediction map ---------------------------------------------------

## Memory limitations - the total size of the data.frame is around 8.7 GB
# While it is small enough to hold in memory, it is too large to perform 
# all operations required by the "recipes" package

## A split-apply-combine based solution will be used:
#     1   - split data.frame into k smaller objects (subsets)
#     1.1 - save subsets to disk (optional - makes running script again faster)
#     2   - loop over subsets
#       2.1 - Read subset into memory
#       2.2 - Apply data pre-processing recipe
#       2.3 - Perform predictions
#       2.4 - Save predictions from subset 
#     3   - Place prediction values in new raster file

##Future improvements:
# Basic: use data.table's fread and fwrite 
# Slightly more advanced:
# Look into disk.frame and fst packages
# https://rpubs.com/xiaodai/intro-disk-fram
# https://www.fstpackage.org/

# Number of substes (larger RAM = less subsets required)
subset_k <- 20
## Save data.frame subsets to disk
# Total number of observations
cell_count <- length(raster_data$elev)
# Number of columns in the raster
cols <- ncol(raster_data$elev)
# Multiplier factor to split the data in 15 subsets
row_multiplier <- round(cell_count/subset_k/cols, digits = 0)
# Maximum number of rows in raster (used to prevent indexes over bounds)
max_row <- nrow(raster_data$elev)

for(i in 1:subset_k){
  # Starting row for subset
  start_row <- ((i - 1) * row_multiplier) + 1
  # Last row for subset (if smaller than raster number of rows)
  end_row <- ifelse(i * row_multiplier < max_row,
                    i * row_multiplier,
                    max_row)
  
  message(paste("Creating subset", i, "of ", subset_k))
  # Subset raster
  raster_subset <- raster_data[start_row:end_row,]
  
  message(paste("Saving subset", i, "of ", subset_k))
  # Save to disk (as .csv...really have to improve this)
  write.table(raster_subset,
              file = paste0("./5_outputs/tmp/raster_subset_", i, ".csv"),
              sep = ";",
              dec = ".",
              row.names = FALSE)
  message(paste("Subset", i, "of ", subset_k, " finished!"))
  rm("raster_subset")
  
}

# Create integer vector to store predictions
predictions <- integer()

# Predictors used for model
predictors <- c("elev", "slope", "ndvi", "nir", "r", "g", "b")

# Apply data pre-processing and create predictions
for(i in 1:subset_k){
  message(paste("Reading subset", i, "of ", subset_k))
  # Read subset of data into memory
  raster_subset <- read.delim(paste0("./5_outputs/tmp/raster_subset_", i, ".csv"),
                              sep = ";",
                              dec = ".")
  cell_indices <- complete.cases(raster_subset)
  # Remove na values from the masking process
  raster_subset <- na.omit(raster_subset)
  
  if(nrow(raster_subset) > 0){
    message(paste("Pre-processing subset", i, "of ", subset_k))
    # Apply pre-processing to subset
    raster_subset <- bake(recipe,
                          new_data = raster_subset,
                          composition = "data.frame")
    
    message(paste("Predicting subset", i, "of ", subset_k))
    # Create predictions for subset
    predictions_subset <- predict(object = model,
                                  newdata = raster_subset[,predictors],
                                  type = "raw")
  }
  # Create new prediction subset vector, with NAs for raster
  predictions_subset2 <- factor(rep(NA, length(cell_indices)), levels = c("1", "2", "3", "4"))
  
  if(nrow(raster_subset) > 0){
    predictions_subset2[cell_indices] <- factor(predictions_subset, levels = c("1", "2", "3", "4"))
  }
  # Append predictions to full predictions vector
  predictions <- factor(c(predictions, predictions_subset2), levels = c("1", "2", "3", "4"))
}

# Create a copy of the predictors raster (with no data stored)
map <- raster(raster_data)

# Set the cells, which are not NA in the raster_data object to
# the calculated prediction
map <- setValues(map, predictions)


# Save raster to disk
raster::writeRaster(
  map, 
  filename = "./5_outputs/saltmarsh_classification_raster/saltmarsh_classification_v2.tif",
  overwrite = TRUE)
