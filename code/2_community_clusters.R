# Monipor quadrat feature preparation script
# Adds vegetation, clustersm elevation and slope to quadrats in transects
# Marcio Martins - July 2019
# marciomartinsred@gmail.com

# Settings ----------------------------------------------------------------
library(readxl)   # Read xlsx
library(raster)   # Tools to handle rasters
library(dplyr)    # Data wrangling
library(tidyr)    # Data wrangling
library(sf)       # Spatial feature manipulation 
library(vegan)    # Ecology analysis functions - for bray curtis distances

options(stringsAsFactors = FALSE)

# Proj4 string for CRS PT-TM06/ETRS89
pt_crs <- "+proj=tmerc +lat_0=39.66825833333333 +lon_0=-8.133108333333334 +k=1 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs "
# Proj4 string for CRS WGS84
wgs84_crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "

# Load data ---------------------------------------------------------------
quadrats <- st_read("./outputs/quadrats_raw/quadrats_raw.gpkg")

# Add vegetation presence to individual quadrats --------------------------
# Load a wide format presence matrix
sheets <- excel_sheets("./data/saltmarsh_cover/ria_cover_matrices.xlsx")

presence_wide <- data.frame()
for(i in 1:length(sheets)){
  sheet <- sheets[i]
  matrix <- read_xlsx("./data/saltmarsh_cover/ria_cover_matrices.xlsx",
                      sheet = sheet)
  matrix[,-1] <- replace(x = matrix[,-1], list = matrix[,-1] > 0, values = 1)

  matrix[is.na(matrix)] <- 0
  
  distances <- as.numeric(colnames(matrix)[-1])
  
  matrix <- t(matrix)
  colnames(matrix) <- matrix[1,]
  matrix <- as.data.frame(matrix[-1,]) %>%
    mutate_all(.funs = function(x) as.numeric(as.character(x)))
  matrix$transect_id <- as.numeric(sheet)
  matrix$quad_index <- distances
  
  presence_wide <- bind_rows(presence_wide, matrix)
}

# Move columns with information to the end of data.frame
presence_wide <- presence_wide %>%
  select(-transect_id, -quad_index, everything())


# Turn all na's into 0
presence_wide[is.na(presence_wide)] <- 0

# Add clustering information to individual quadrats -----------------------
# Add column indicating if vegetation is present
presence_wide <- presence_wide %>%
  # Sum all species presences
  cbind("unvegetated" = apply(presence_wide[,1:27], 1, sum)) %>%
  # If no species are present, mark as unvegetated
  mutate(unvegetated = ifelse(unvegetated == 0, 1, 0)) 
  

vegetated_matrices <- presence_wide %>%
  filter(unvegetated == 0)

unvegetated_matrices <- presence_wide %>%
  filter(unvegetated == 1)

# Get pairwise Bray-Curtins binary distances (AKA Sorensen index)
bray_distances <- vegdist(vegetated_matrices[,c(1:27)],
                          method = "bray",
                          binary = TRUE)

# Some references for usage of clustering:
# Cluster. Ward's linkage was used based on 
# "Robustness of three hierarchical agglomerative clustering 
# techniques for ecological data" - See more references for what might be appropriate

# See also:
# "Use of the Bray-Curtis Similarity Measure in Cluster Analysis of Foraminiferal Data"

# "Bacterial bioclusters relate to hydrochemistry in New Zealand groundwater"
## This paper even shows some more advanced clustering options

# "Multivariate Analysis of Ecological Communities in R: vegan tutorial"; Chapter 6

# SELECT NUMBER OF CLUSTERS
cluster_n <- 4

dendrogram <- hclust(bray_distances, method = "ward.D")

clusters <- cutree(dendrogram, k = cluster_n)

# Add clusters to the vegetated and unvegetated matrices, bind them back together
vegetated_matrices <- vegetated_matrices %>%
  mutate(cluster = clusters)

unvegetated_matrices <- unvegetated_matrices %>%
  mutate(cluster = "unvegetated")

presence_wide <- rbind(vegetated_matrices, unvegetated_matrices)

quadrats_spp <- quadrats %>%
  left_join(presence_wide, by = c("transect_id", "quad_index")) %>%
  arrange(transect_id, quad_index)

# Save the final quadrats as shapefiles -----------------------------------
st_write(quadrats_spp,
         "./outputs/quadrats_species/quadrats_species.gpkg")

