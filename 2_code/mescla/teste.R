library(readxl)
library(tidyverse)
library(zoo)
library(plotly)
library(scales)
library(openxlsx)
library(vegan)
library(MMartins)
library(gridExtra)
library(dendextend)
library(ggdendro)

#### TO DO
#### Add species identification when appropriate
#### Change presente matrices to have all species

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
sheets <- excel_sheets("./3_data/raw/saltmarsh_cover/ria_cover_matrices.xlsx")

presence_wide <- data.frame()
for(i in 1:length(sheets)){
  sheet <- sheets[i]
  matrix <- read_xlsx("./3_data/raw/saltmarsh_cover/ria_cover_matrices.xlsx",
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
  matrix$transect <- sheet
  matrix$distances <- distances
  matrix$scaled_distances <- scaled_distances
  
  presence_wide <- bind_rows(presence_wide, matrix)
}

presence_wide <- presence_wide[,-c(29:34)]


# Plot presence and cover percent along scaled transects ------------------
# Get maximum length of each transect
transect_distances <- data %>%
  gather(key = "distance",
         value = "cover",
         -Species, -transect,
         na.rm = TRUE) %>%
  mutate(distance = as.numeric(distance)) %>% 
  group_by(transect) %>% 
  summarise(transect_distance = max(distance))
  
# Cast to long format
data_long <- data %>%
  gather(key = "distance",
         value = "cover",
         -Species, -transect,
         na.rm = TRUE) %>%
  mutate(distance = as.numeric(distance)) %>%
  # Complete all species:scaled_distance:transect combinations
  complete(transect, distance, Species, fill = list(cover = 0)) %>%
  # Add a column with transect distance
  left_join(transect_distances, by = "transect") %>%
  # Calculate scaled distance (0 to 1, rounded to nearest 0.025)
  mutate(scaled_distance = 
           ceiling((distance / transect_distance) / 0.025) * 0.025) %>%
  # Remove the combinations that go beyond each transect's length
  filter(distance <= transect_distance) %>%
  # Add a column for presence/absence
  mutate(presence = ifelse(cover == 0, 0, 1))

# DATA CHECK - All combinations of species:scaled_distance:transect
# must have a frequency of one or higher to ensure the means are not
# biased towards uncommon species?
table(data_long[,c(1,6,3)]) %>% unique()

# Summarise data and smooth curve (average every point to neared 7.5%)
data_sum <- data_long %>%
  group_by(Species, scaled_distance) %>%
  summarise(mean_cover = mean(cover),
            mean_presence = mean(presence)) %>%
  mutate(
    smooth_cover = rollapply(
      data = mean_cover, 
      width = 3, 
      fill = NA, 
      align = "center",
      partial = TRUE,
      FUN = function(x) mean(x)),
    smooth_presence = rollapply(
      data = mean_presence, 
      width = 3, 
      fill = NA, 
      align = "center",
      partial = TRUE,
      FUN = function(x) mean(x)))

# Plot
ggplotly(
ggplot(data_sum, aes(x = scaled_distance, y = smooth_cover, color = Species)) +
  geom_line() +
  scale_y_continuous(
    name = "Cover",
    labels = scales::percent_format(scale = 1)) +
  labs(title = "Average cover of species along transects",
       x = "Scaled transect distance") +
  theme_minimal()
)

ggplotly(
ggplot(data_sum, aes(x = scaled_distance, y = smooth_presence, color = Species)) +
  geom_line() +
  scale_y_continuous(
    name = "Presence",
    labels = scales::percent_format()) +
  labs(title = "Average presence of species along transects",
       x = "Scaled transect distance") +
  theme_minimal()
)


# Hierarchical clustering analysis based on bray-curtis distances ---------

# Remove unvegetated rows from presence matrices
bray_matrices <- presence_wide %>%
  cbind("total" = apply(presence_wide[,1:25], 1, sum)) %>%
  filter(total > 0) %>%
  select(-total)

# Get pairwise bray curtis distances 
bray_distances <- vegdist(bray_matrices[,c(1:25)],
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

bray_matrices %>%
  cbind("species_nr" = apply(bray_matrices[,1:25], 1, sum)) %>%
  cbind(clusters) %>%
  group_by(clusters) %>%
  summarise(
    N = n(),
    avg_species = mean(species_nr))

# Graphical summary of cluster composition and distribution

dendrogram_formated <- dendrogram %>%
  as.dendrogram() %>%  # Convert to dendrogram class
  set("branches_k_color", k = cluster_n) %>% # Set branch color
  set("labels_colors", k = cluster_n) %>%    # Set label color
  rev() %>%  # Reverse leaf order
  as.ggdend() %>%
  ggplot(horiz = TRUE,
         labels = FALSE,
         theme = NULL) +
  labs(title = "Dendrogram",
       x = "",
       y = "") +
  scale_y_reverse(expand = c(0, 0),limits = c(300,0)) +
  theme_custom(axis = FALSE) +
  theme(axis.text = element_blank())


sdistances_hist <- bray_matrices %>%
  cbind(clusters) %>%
  ggplot() +
  geom_histogram(
    aes(x = scaled_distances,
        group = clusters, 
        fill = as.factor(clusters)),
    bins = 13) +
  facet_grid(rows = vars(clusters),
             switch = "y") +
  scale_fill_discrete(guide = FALSE) +
  labs(title = "Distribution of clusters along transect",
       x = "Scaled distance along transect",
       y = "Counts") +
  scale_y_continuous(position = "right") +
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
         caption = "Clusters defined by Ward's method, based on Bray-Curtis distances",
         y = NULL,
         x = "Cluster") +
    theme_custom(axis = FALSE)



grid.arrange(
  dendrogram_formated, sdistances_hist, species_heatmap,
  layout_matrix = matrix(c(1,1,2,2,2,2,2,
                           NA,3,3,3,3,NA,NA), ncol = 7, byrow = T))

