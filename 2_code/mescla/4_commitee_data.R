# Monipor - saltmarsh area estimation script

# Márcio Martins - July 2019
# marciomartinsred@gmail.com
# Estimate the coverage area for saltmarshes of the monipor areas


# Settings ----------------------------------------------------------------
library(readxl)   # Read xlsx
library(openxlsx) # Write and format xlsx 
library(raster)   # Read and operate raster data
library(tidyverse)
library(MMartins) # Custom functions
library(imputeTS)      # Interpolation between missing values

options(stringsAsFactors = FALSE)


# Load full data ----------------------------------------------------------
# Get quadrats and quadrat features
source("./4_code/2_quadrat_features.R")


saltmarsh_cover <- read_xlsx("./3_data/clean/monipor_clean_data.xlsx",
                             sheet = "matrices_long") %>%
  mutate_at(vars(quadrat), as.numeric) %>%
  arrange(water_system,
          water_body,
          site_nr,
          transect_id)


# Interpolate Alvor transects ---------------------------------------------

# For the Alvor transects, some quadrats were skipped due to transect length
# Interpolate between quadrat values

alvor_quadrat_nr <- saltmarsh_cover %>%
  filter(water_system == "Alvor") %>%
  pivot_wider(names_from = "species",
              values_from = "cover") %>%
  group_by(transect_id) %>%
  summarise(max = max(quadrat))

alvor_wide <- saltmarsh_cover %>%
  filter(water_system == "Alvor") %>%
  pivot_wider(names_from = "species",
              values_from = "cover")

all_quadrats <- c(0:alvor_quadrat_nr$max[1],
                  0:alvor_quadrat_nr$max[2],
                  0:alvor_quadrat_nr$max[3],
                  0:alvor_quadrat_nr$max[4],
                  0:alvor_quadrat_nr$max[5],
                  0:alvor_quadrat_nr$max[6])

transect_ids <- c(rep(1, alvor_quadrat_nr$max[1]+1),
                  rep(2, alvor_quadrat_nr$max[2]+1),
                  rep(3, alvor_quadrat_nr$max[3]+1),
                  rep(4, alvor_quadrat_nr$max[4]+1),
                  rep(5, alvor_quadrat_nr$max[5]+1),
                  rep(6, alvor_quadrat_nr$max[6]+1))

site_nrs <- c(
  rep(2, sum(length(c(
    0:alvor_quadrat_nr$max[1], 
    0:alvor_quadrat_nr$max[2],
    0:alvor_quadrat_nr$max[3])))),
  rep(1, sum(length(c(
    0:alvor_quadrat_nr$max[4], 
    0:alvor_quadrat_nr$max[5],
    0:alvor_quadrat_nr$max[6])))))


alvor_missing <- data.frame("water_system" = rep("Alvor", length(all_quadrats)),
                            "transect_id" = transect_ids,
                            "quadrat" = all_quadrats,
                            "site_nr" = site_nrs,
                            "water_body" = 1) %>%
  anti_join(alvor_wide, by = c("transect_id", "quadrat"))

# Add missing quadrats to data.frame and interpolate missing values
alvor_wide_full <- alvor_wide %>%
  bind_rows(alvor_missing) %>%
  arrange(transect_id,
          quadrat) %>%
  ungroup() %>%
  na_interpolation()

# Change Alvor interpolated data to long format
alvor_full <- alvor_wide_full %>%
  pivot_longer(cols = -c(1:5),
               names_to = "species",
               values_to = "cover")

# Add the interpolated Alvor transects to rest of data
saltmarsh_cover_full <- saltmarsh_cover %>%
  filter(water_system != "Alvor") %>%
  bind_rows(alvor_full)


# Estimate Ria Formosa coverages ------------------------------------------
ria_clusters <- presence_polygons %>%
  select(transect_id, quad_index, cluster) %>%
  st_drop_geometry()

ria_cover_wide <- saltmarsh_cover %>%
  filter(water_system == "Ria Formosa") %>%
  pivot_wider(names_from = "species",
              values_from= "cover",
              values_fill= list("cover" = 0)) %>%
  left_join(ria_clusters,
            by = c("transect_id", "quadrat" = "quad_index"))

ria_cover_long <- ria_cover_wide %>%
  pivot_longer(cols = -c(1:5, 34),
               names_to = "species",
               values_to= "cover")

saltmarsh_cover_full <- saltmarsh_cover_full %>%
  filter(water_system != "Ria Formosa") %>%
  bind_rows(ria_cover_long)


# Get average cover per water body and cluster ----------------------------
# Change cluster values from NA to 1 (only Ria had clusters so far)
saltmarsh_cover_full <- saltmarsh_cover_full %>%
  mutate(cluster = ifelse(is.na(cluster),1,cluster))

# Calculate the total number of quadrats per water body
quadrat_nr <- saltmarsh_cover_full %>%
  group_by(water_system, water_body, transect_id, cluster) %>%
  # Number of quadrats per transect
  summarise(total_quadrats = length(unique(quadrat))) %>%
  # Number of quadrats per water body
  group_by(water_system, water_body, cluster) %>%
  summarise(total_quadrats = sum(total_quadrats))

# Calculate the fraction covered by each species in each water body
cover_estimates <- saltmarsh_cover_full %>%
  group_by(water_system, water_body, species, cluster) %>%
  # Count total coverage area in squared meters (1 quadrat = 1m2),
  #    meaning a quadrat with 50% coverage has 0.5 m2
  summarise(total_cover = sum(cover/100)) %>%
  ungroup() %>%
    # Add the total quadrat number per water system, water body and cluster
  left_join(quadrat_nr, by = c("water_system", "water_body", "cluster")) %>%
  mutate(cover_fraction = total_cover/total_quadrats)
  
  
# Join all data into one --------------------------------------------------

# Load list of species given by the MONIPOR commiteee

template <- read_xlsx("./2_planning_recourses/sheets_template/layout_AQuA_Index_Calculator.xlsx",
                      sheet = "Species raw data") %>%
  select(species = Specie) %>%
  .[1:28,]

# Spread the estimates for each water body in an individual column
cover_estimates_wide <- full_join(
  template,
  cover_estimates %>%
    select(water_system, water_body, cluster, species, cover_fraction) %>%
    pivot_wider(names_from = c("water_system", "water_body", "cluster"),
                values_from = "cover_fraction",
                names_sep = "_" )) %>%
  filter(!(species %in% c("Walking platform", "unvegeated")))

# Replace the NAs with zero
cover_estimates_wide[is.na(cover_estimates_wide)] <- 0
write.xlsx(cover_estimates_wide, 
           "./5_outputs/commitee_tables/saltmashes/cover_fractions.xlsx")
