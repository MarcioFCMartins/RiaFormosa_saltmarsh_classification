# obsolete - after 2 days of implemention this I realized raster::stretch exists
# Function to perform contrast enhancement in a satellite image
# Uses linear stretching
# Algorithm from:
# https://github.com/rspatial/raster/blob/master/R/stretch.R
# Márcio Martins - April 2020
# marciomartinsred@gmail.com

# Requirements:
# raster - methods to operate on raster data without loading fully to memory
# sf     - simple features: spatial operations and methods for vector data

# Raster package handles all the memory allocation and temporary file creation (see rasterTmpFile).
# All scaling, stretching and clipping is done in one raster::calc call 
# Not ran in parallel. To run in parallel, apply to individual layers using foreach

contrast_stretch <- function(
    x, 
    quantiles = c(0.02, 0.98), 
    filename,
    overwrite = FALSE) {
    
    if(!class(x) %in% c("RasterLayer", "RasterStack", "RasterBrick") ||
       missing(x)){
        stop("x should be a raster object")
    }
    
    # Max values for max scaling
    maxs <- raster::maxValue(x)
    
    # Limits used for stretching (in 0-255 range)
    limits <- matrix(
        quantile(x, probs = quantiles, na.rm = TRUE)/maxs * 255,
        ncol = 2)
    
    # Stretch full image, save to path if given
    x <- if(!missing(filename)){
        calc(
            x, 
            fun = function(x) {
                # max scaling
                values <- x / maxs
                values <- values * 255
                # Scale based on quantiles
                values <- (255 * (values - limits[,1]))/(limits[,2] - limits[,1])
                # Clip to 0-255 range
                values[values < 0] <- 0
                values[values > 255] <- 255
                return(values)
            },
            filename = filename,
            overwrite = overwrite)} 
    else {
        calc(
            x, 
            fun = function(x) {
                # max scaling
                values <- x / maxs
                values <- values * 255
                # Scale based on quantiles
                values <- (255 * (values - limits[,1]))/(limits[,2] - limits[,1])
                # Clip to 0-255 range
                values[values < 0] <- 0
                values[values > 255] <- 255
                return(values)
            },
            overwrite = overwrite)
    }
    
    
    return(x)
}
