library(readxl)
library(tidyverse)
library(zoo)
library(plotly)
library(scales)
library(openxlsx)
library(vegan)
library(MMartins)
library(ggpubr)
library(dendextend)
library(ggdendro)

#### TO DO

# Load cover percentage matrices ------------------------------------------

# Get list of transects (excel sheets)
sheets <- excel_sheets("./3_data/raw/saltmarsh_cover/ria_cover_matrices.xlsx")

# Read transect matrices, also checks if any quadract has over 100% coverage
# and warns the user if so
data <- data.frame()
for(i in 1:length(sheets)){
  sheet <- sheets[i]
  matrix <- read_xlsx("./3_data/raw/saltmarsh_cover/ria_cover_matrices.xlsx",
                      sheet = sheet)
  # Check if any column adds to over 100
  if(any(apply(matrix[,-1], 2, sum, na.rm = TRUE) > 100)){
    wrong_column <- colnames(matrix[,-1])[apply(matrix[,-1], 2, sum, na.rm = TRUE) > 100]
    message(paste("The column(s)", paste(wrong_column, collapse = ","), "in", sheet, "adds to over 100 percent. Fix data\n"))
  }
  matrix[,c(-1)] <- replace(x = matrix[,-1], list = is.na(matrix[,-1]), values = 0)
  matrix$transect <- sheet
  data <- bind_rows(data, matrix)
}


# Load presence matrices (in wide format) ---------------------------------
sheets <- excel_sheets("./3_data/clean/saltmarsh_cover/ria_cover_matrices.xlsx")

presence_wide <- data.frame()
for(i in 1:length(sheets)){
  sheet <- sheets[i]
  matrix <- read_xlsx("./3_data/clean/saltmarsh_cover/ria_cover_matrices.xlsx",
                      sheet = sheet)
  matrix[,-1] <- replace(x = matrix[,-1], list = !is.na(matrix[,-1]), values = 1)
  matrix[is.na(matrix)] <- 0
  
  distances <- as.numeric(colnames(matrix)[-1])
  scaled_distances <- distances/max(distances) # Get scaled distances
  scaled_distances <- ceiling((scaled_distances / 0.025)) * 0.025 # Round to nearest 2.5%
  
  matrix <- t(matrix)
  colnames(matrix) <- matrix[1,]
  matrix <- as.data.frame(matrix[-1,]) %>%
    mutate_all(.funs = function(x) as.numeric(as.character(x)))
  matrix$transect_id <- as.numeric(sheet)
  matrix$distances <- distances
  matrix$scaled_distances <- scaled_distances
  
  presence_wide <- bind_rows(presence_wide, matrix)
}

# Move information columns to end of data.frame
presence_wide <- presence_wide %>%
  select(-transect_id, -distances, -scaled_distances, everything())

# Turn all na's into 0
presence_wide[is.na(presence_wide)] <- 0

# Join with elevation data (after running 1_get_spatial_data)
presence_wide <- presence_wide %>%
  left_join(quadrat_polygons[,c("transect_id", "quad_index", "dem", "slope", "geometry")],
            by = c("transect_id", "distances" =  "quad_index"))


# Hierarchical clustering analysis based on bray-curtis distances ---------

# Remove unvegetated rows from presence matrices
bray_matrices <- presence_wide %>%
  cbind("total" = apply(presence_wide[,1:28], 1, sum)) %>%
  filter(total > 0)

# Get pairwise bray curtis distances 
bray_distances <- vegdist(bray_matrices[,c(1:28)],
                          method = "bray")



# Cluster. Ward's linkage was used based on 
# "Robustness of three hierarchical agglomerative clustering 
# techniques for ecological data" - See more references for what might be appropriate

# See also:
# "Use of the Bray-Curtis Similarity Measure in Cluster Analysis of Foraminiferal Data"

# "Bacterial bioclusters relate to hydrochemistry in New Zealand groundwater"
## This paper even shows some more advanced clustering options

# "Multivariate Analysis of Ecological Communities in R: vegan tutorial"; Chapter 6

# SELECT NUMBER OF CLUSTERS
cluster_n <- 3


dendrogram <- hclust(bray_distances, method = "ward.D")

clusters <- cutree(dendrogram, k = cluster_n)

palette <- brewer_pal(type = "qual", palette = "Set1")(cluster_n)
# Graphical summary of cluster composition and distribution

dendrogram_formated <- dendrogram %>%
  as.dendrogram() %>%  # Convert to dendrogram class
  color_branches(k = cluster_n, col = palette) %>% # Set branch color
  rev() %>%  # Reverse leaf order
  as.ggdend() %>%
  ggplot(horiz = TRUE,
         labels = FALSE,
         theme = NULL) +
  labs(title = "Dendrogram",
       x = "",
       y = "Clusters defined by Ward's method, based on Bray-Curtis distances") +
  scale_y_reverse(expand = c(0, 0),limits = c(300,0)) +
  theme_custom(axis = FALSE) +
  theme(axis.text = element_blank())


sdistances_hist <- bray_matrices %>%
  cbind(clusters) %>%
  ggplot() +
  geom_histogram(
    aes(x = dem.x,
        group = clusters, 
        fill = as.factor(clusters)),
    bins = 13) +
  facet_grid(rows = vars(clusters),
             switch = "y") +
  labs(title = "Distribution of clusters along transect",
       x = "Elevation relative to hydrographic zero (m)",
       y = "Counts") +
  scale_y_continuous(position = "right") +
  scale_fill_manual(values = palette, guide = FALSE) +
  theme_custom() 

species_heatmap <- bray_matrices %>%
  cbind(clusters) %>%
  gather(key = "species", value = "presence", 1:25) %>%
  group_by(clusters, species) %>%
  summarise(proportion_presences = sum(presence)/n()) %>%
  ungroup() %>%
  filter(proportion_presences > 0) %>%
  complete(clusters,species, fill = list(proportion_presences = 0)) %>%
  ggplot(aes(x = clusters, 
             y = species, 
             fill = proportion_presences)) +
    geom_raster() +
    geom_text(aes(label = 
      ifelse(proportion_presences > 0.1, 
             percent(proportion_presences, accuracy = 1),
             NA)),
      color = "white",
      fontface = "bold") +
    scale_fill_viridis_c(guide = FALSE) +
    labs(title = "Species presence per cluster",
         y = NULL,
         x = "Cluster") +
    theme_custom(axis = FALSE)

species_counts <- bray_matrices %>%
  cbind("species_nr" = apply(bray_matrices[,1:25], 1, sum)) %>%
  cbind(clusters) %>%
  group_by(clusters) %>%
  summarise(
    N = n(),
    min_species = min(species_nr),
    avg_species = mean(species_nr),
    max_species = round(max(species_nr),2))

species_counts$total_species <- bray_matrices[,1:25] %>%
  split(clusters) %>%
  lapply(FUN = function(x) length(apply(x, MARGIN = 2, sum)[apply(x, MARGIN = 2, sum) > 0])) %>%
  unlist()

species_counts <- species_counts %>%
  gather(key = "measure", value = "value", -clusters)

count_heatmap <- ggplot(species_counts,
                   aes(x = clusters, y = measure, fill = as.factor(clusters))) +
    geom_raster() +
    geom_text(aes(label = value),
      color = "white",
      fontface = "bold") +
    scale_fill_manual(values = palette) +
    theme_custom(axis = FALSE)

ggplot(presence_wide %>%
         gather(key = "key", value = "value",1:28) %>%
         filter(value == 1)) +
  geom_histogram(aes(x = dem)) +
  facet_wrap(~ key, scales = "free_y")

ggarrange(species_heatmap, 
          sdistances_hist, 
          nrow = 1,
          widths = c(1,2))

ggarrange(species_heatmap, count_heatmap, align = "v", nrow = 2)

