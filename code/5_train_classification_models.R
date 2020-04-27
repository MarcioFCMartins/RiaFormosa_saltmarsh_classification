# Mapping Blue Carbon - RIAVALUE
# Classify saltmarsh type - model training and selection
# Márcio Martins - November 2019
# marciomartinsred@gmail.com


# Settings ----------------------------------------------------------------
library(raster)   # Raster operations and manipulation
library(caret)    # Wrapper for several machine learning packages
library(CAST)     # Spatial-temporal methods for caret training
library(dplyr)    # Data wrangling
library(tidyr)    # Data wrangling
library(sf)       # Simple-features - spatial data 
library(recipes)  # Data pre-processing for modelling
library(rasclass) # max likelihood classification

# Data loading ------------------------------------------------------------

# See 1-extract-quadrat-info.R for raster information extraction code
quadrats <- st_read("./3_data/saltmarsh_quadrats/quadrats_plus_features/quadrats_training_data.shp")

colnames(quadrats) <-  read.csv2(
  "./3_data/saltmarsh_quadrats/quadrats_plus_features/column_names.csv")[,2]

# Full list of possible predictors
predictors <- c("elev", "slope", "ndvi", "nir", "r", "g", "b")

# Convert sf object to data.frame, remove geometry
full_data <- as.data.frame(st_drop_geometry(quadrats)) %>%
  # Create station identifier, convert station to factor
  mutate(station = group_indices(., water_body, site_nr),
         cluster = as.factor(cluster)) %>%
  # Remove unnecessary columns
  select(station, transect_nr, cluster, predictors) %>%
  arrange(station, cluster) %>%
  # Remove station 10, due to raster data not overlapping with some transects
  filter(station != 10)

# Select the transect number in each of the 9 stations that should be used 
# for holdout set
set.seed(42)
holdout_indices <- data.frame(station      = c(1:9),
                              holdout_transect= sample(1:3,
                                                       size = 9,
                                                       replace = TRUE))

# Add holdout transect id to full data and remove that transect
# Creates training dataset
train_data <- full_data %>%
  left_join(holdout_indices) %>%
  filter(transect_nr != holdout_transect) 

# Same as previous step, but keeps holdout transects only
# Creates holdout dataset
holdout_data <- full_data %>%
  left_join(holdout_indices) %>%
  filter(transect_nr == holdout_transect) 

# Create cross-validation indices by removing one sampling station per fold
set.seed(42)
cv_indices <- CreateSpacetimeFolds(train_data, 
                                   spacevar = "station",
                                   k = 9)


# Data pre-processing -----------------------------------------------------

# Recipe for data pre-processing. The advantage of a recipe is pre-processing can
# later be applied to the test data easily (e.g. center and scaling is performed 
# based on the mean and std deviation of the test data)
recipe <- recipe(cluster ~ elev + slope + ndvi + nir + r + g + b,
                 data = train_data) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

recipe_pca <- recipe(cluster ~ elev + slope + ndvi + nir + r + g + b,
                 data = train_data) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_pca(all_predictors(), threshold = 0.95)

# Use training data to estimate pre-processing parameters
# Raw predictors
recipe <- prep(recipe, train_data)

# Save this recipe as an object for use outside of this script
saveRDS(recipe,
        "./5_outputs/saltmarsh-classification-models/data_recipe.RDS")

# Apply steps to train and holdout sets
train_data <- bake(recipe,
                   new_data = train_data) %>%
  as.data.frame() 

holdout_data <- bake(recipe,
                     new_data = holdout_data) %>%
  as.data.frame()

# PCA-treated predictors
recipe_pca <- prep(recipe_pca, train_data)

# Save this recipe as an object for use outside of this script
saveRDS(recipe_pca,
        "./5_outputs/saltmarsh-classification-models/data_recipe_pca.RDS")

train_data_pca <- bake(recipe_pca,
                   new_data = train_data) %>%
  as.data.frame() 

holdout_data_pca <- bake(recipe_pca,
                     new_data = holdout_data) %>%
  as.data.frame()


# Maximum likelihood models -----------------------------------------------

### Raw predictors
rasc_train <- new("rasclass")

# Due to the way this package works, I need to create a full dataset where:
#   Training samples have the true cluster
#   Holdout samples have NA for cluster
rasc_data <- bind_rows(train_data, 
                       holdout_data[,predictors])

rasc_train <- setRasclassData(rasc_data, 
                              ncols = 1, 
                              nrows = nrow(full_data),	
                              xllcorner = 0, 
                              yllcorner = 0, 
                              cellsize = 1, 
                              NAvalue = -9999,	
                              samplename = "cluster")

mlc <- classifyRasclass(rasc_train, method = 'maximumLikelihood')

### PCA reduced predictors

rasc_train_pca <- new("rasclass")

rasc_data_pca <- bind_rows(train_data_pca, 
                       holdout_data_pca)

rasc_train_pca <- setRasclassData(rasc_data_pca, 
                              ncols = 1, 
                              nrows = nrow(full_data),	
                              xllcorner = 0, 
                              yllcorner = 0, 
                              cellsize = 1, 
                              NAvalue = -9999,	
                              samplename = "cluster")

mlc_pca <- classifyRasclass(rasc_train_pca, method = 'maximumLikelihood')


# Train models on standardized predictors ---------------------------------

# vector of target classes to train models
target <- train_data$cluster
models <- list()
tune_length <- 50

models$svm_rad <- train(train_data[,predictors],
                          target,
                          method ="svmRadial",
                          metric = "Kappa",
                          tuneLength=tune_length,
                          trControl=trainControl(method="cv",
                                                 index = cv_indices$index,
                                                 search = "grid"),
                          scaled = FALSE)


models$svm_linear <- train(train_data[,predictors],
                          target,
                          method ="svmLinear",
                          metric = "Kappa",
                          tuneLength=tune_length,
                          trControl=trainControl(method="cv",
                                                 index = cv_indices$index,
                                                 search = "grid"),
                          scaled = FALSE)


models$rf <- train(train_data[,predictors],
                  target,
                  method ="rf",
                  metric = "Kappa",
                  tuneLength=tune_length,
                  trControl=trainControl(method="cv",
                                         index = cv_indices$index,
                                         search = "grid"))


models$nb <- train(train_data[,predictors],
                  target,
                  method    ="naive_bayes",
                  metric    = "Kappa",
                  tuneLength= tune_length,
                  trControl = trainControl(method="cv",
                                         index = cv_indices$index,
                                         search = "grid"))


models$nnet <- train(train_data[,predictors],
                  target,
                  method     = "nnet",
                  metric     = "Kappa",
                  tuneLength = tune_length,
                  trControl  = trainControl(method="cv",
                                         index = cv_indices$index,
                                         search = "grid"))
saveRDS(models,
        "./5_outputs/saltmarsh-classification-models/models-list.RDS")

rm(models)
# Train models on stadardized and PCA reduced predictors ------------------

pca_predictors <- c("PC1", "PC2", "PC3", "PC4")
models_pca <- list()


models_pca$svm_rad_pca <- train(train_data_pca[,pca_predictors],
                          target,
                          method ="svmRadial",
                          metric = "Kappa",
                          tuneLength=tune_length,
                          trControl=trainControl(method="cv",
                                                 index = cv_indices$index,
                                                 search = "grid"),
                          scaled = FALSE)


models_pca$svm_linear_pca <- train(train_data_pca[,pca_predictors],
                          target,
                          method ="svmLinear",
                          metric = "Kappa",
                          tuneLength=tune_length,
                          trControl=trainControl(method="cv",
                                                 index = cv_indices$index,
                                                 search = "grid"),
                          scaled = FALSE)

models_pca$rf_pca <- train(train_data_pca[,pca_predictors],
                  target,
                  method ="rf",
                  metric = "Kappa",
                  tuneLength=tune_length,
                  trControl=trainControl(method="cv",
                                         index = cv_indices$index,
                                         search = "grid"))


models_pca$nb_pca <- train(train_data_pca[,pca_predictors],
                  target,
                  method    ="naive_bayes",
                  metric    = "Kappa",
                  tuneLength= tune_length,
                  trControl = trainControl(method="cv",
                                         index = cv_indices$index,
                                         search = "grid"))


models_pca$nnet_pca <- train(train_data_pca[,pca_predictors],
                  target,
                  method     = "nnet",
                  metric     = "Kappa",
                  tuneLength = tune_length,
                  trControl  = trainControl(method="cv",
                                         index = cv_indices$index,
                                         search = "grid"))


saveRDS(models_pca,
        "./5_outputs/saltmarsh-classification-models/models-list-pca.RDS")

rm(models_pca)
