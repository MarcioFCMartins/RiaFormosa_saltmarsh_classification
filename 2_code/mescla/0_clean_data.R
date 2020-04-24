# Monipor data cleaning script
# Márcio Martins - July 2019
# marciomartinsred@gmail.com

# Settings ----------------------------------------------------------------
library(readxl)   # Read xlsx
library(openxlsx) # Write and format xlsx 
library(dplyr)    # Data wrangling
library(tidyr)    # Data wrangling
library(stringr)  # String manipulation
# https://github.com/MarcioFCMartins/MMartins
library(MMartins) # Custom functions - used to export nicely formated excel files
library(tibble)



options(stringsAsFactors = FALSE)

# Lookup table for short names for our water systems, used to create names
water_systems <- data.frame(
  water_system = c("Ria Formosa", "Alvor", "Aljezur", "Guadiana", "Arade"),
  short_name   = c("ria", "alvor", "aljez", "guad", "arade"))

# Seagrass data -----------------------------------------------------------

seagrass_gps <- read.csv("./3_data/raw/GPS_points/seagrasses.csv",
                         sep = ";")

seagrass_shoots <- read.csv("./3_data/raw/seagrass_shoots/shoot_count.csv",
                            sep = ";")

seagrass_data <- seagrass_gps %>%
  # Join GPS and shoot counts
  left_join(seagrass_shoots, by = "gps_point") %>%
  left_join(water_systems, by = "water_system") %>%
  # Change date to only contain date information
  mutate(time = str_extract(date, "[0-9]*[:][0-9]*"),
         date = str_extract(date, "\\d{1,4}/\\d{1,4}/\\d{1,4}"),
         name = paste0("sea_", short_name, "_WB", water_body,
                       "_s", site_nr),
         area = pi * 0.06^2,
         shoot_density = shoot_nr/area) %>% 
  group_by(name) %>%
  mutate(replicate =seq_along(name)) %>% 
  ungroup() %>%
  select(water_system, water_body, site_nr, name, date, time, latitude, longitude,
         species, replicate, shoot_nr, area, shoot_density, notes) %>%
  arrange(water_system, water_body, site_nr) 

seagrass_summary <- seagrass_data %>%
  group_by(name) %>%
  summarise(water_system = unique(water_system),
            water_body = unique(water_body),
            site_nr = unique(site_nr),
            date = unique(date),
            time = unique(time),
            latitude = unique(latitude),
            longitude = unique(longitude),
            species = unique(species),
            mean_shoot_density = mean(shoot_density)) %>%
  select(water_system, water_body, site_nr, name, date:mean_shoot_density) %>%
  arrange(water_system, water_body)


# Opportunistic macroalgae ------------------------------------------------

macroalgae_data <- read.csv("./3_data/raw/macroalgae/macroalgae.csv", 
                            sep = ";") %>%
  left_join(seagrass_gps, by = "gps_point") %>%
  left_join(water_systems, by = "water_system") %>%
  # Change date to only contain date information
  mutate(time = str_extract(date, "[0-9]*[:][0-9]*"),
         date = str_extract(date, "\\d{1,4}/\\d{1,4}/\\d{1,4}"),
         name = paste0("sea_", short_name, "_WB", water_body,
                       "_s", site_nr)) %>%
  select(water_system, water_body, site_nr, name, date, time, latitude, longitude,
         algae_species, cover, notes) %>%
  arrange(water_system, water_body)


# Saltmarsh data ----------------------------------------------------------

saltmarsh_gps <- read.csv("./3_data/raw/GPS_points/saltmarshes.csv",
                          sep = ";") %>%
  left_join(water_systems, by = "water_system") %>%
  group_by(water_system, water_body, site_nr) %>%
  # Create a variable that identifies the number of each transect for the
  # transects that are in the same site; clean dates; create name
  mutate(transect_nr = rep(seq_along(unique(transect_id)), each = 2),
         time = str_extract(date, "[0-9]*[:][0-9]*"),
         date= str_extract(date, "\\d{1,4}/\\d{1,4}/\\d{1,4}"),
         name = paste0("sal_", short_name, "_WB", water_body,
                       "_s", site_nr, "_t", transect_nr))

# Get transect matrices and transect lengths
matrices_files <- list.files("./3_data/raw/saltmarsh_cover/", 
                             pattern = ".xlsx")

matrices_sheets <- lapply(
  matrices_files,
  function(x) excel_sheets(paste0("./3_data/raw/saltmarsh_cover/", x))) 

names(matrices_sheets) <- matrices_files

matrices_sheets <- unlist(matrices_sheets) %>%
  as.data.frame() %>%
  rownames_to_column(var = "file") %>%
  rename(sheet = ".") %>%
  mutate(file = str_replace(file, "xlsx[0-9]*$", "xlsx"))


transect_lengths <- data.frame(
  "water_system"= as.character(),
  "transect_id" = as.character(), 
  "length" = as.numeric())

# Extract transect lengths
for(i in 1:nrow(matrices_sheets)){
  file <-  paste0("./3_data/raw/saltmarsh_cover/", matrices_sheets[i, "file"])
  sheet <- matrices_sheets[i, "sheet"]
  water_system <- str_extract(matrices_sheets[i, "file"],
                              "[a-z]*_") %>%
    str_remove("_")
  
  water_system_names <- data.frame(
    "long_name" = c("Ria Formosa", "Alvor", "Aljezur", "Arade", "Guadiana"),
    "short_name"= c("ria", "alvor", "aljezur", "arade", "guadiana")
  )
  
  # Change to full name so data can be joined later
  water_system <- water_system_names[water_system_names[,"short_name"] == water_system,"long_name"]
  
  
  matrix <- read_xlsx(path = file,
                      sheet = sheet)
  matrix <- gather(matrix, 
                   key = "distance", 
                   value = "cover", 
                   -Species,
                   na.rm = TRUE)
  
  distance <- max(as.numeric(matrix$distance)) + 1
  
  transect_lengths <- rbind(transect_lengths, 
                            data.frame(
                              "water_system" = water_system,
                              "transect_id"  = as.numeric(sheet), 
                              "length"       = distance))
}

# Clean-up the matrices, add identifications to species, create long format version
matrices_list <- list()
matrices_long <- data.frame()

species_id <- read_xlsx("./3_data/raw/species_identification.xlsx")

for(i in 1:nrow(matrices_sheets)){
  file <-  paste0("./3_data/raw/saltmarsh_cover/", matrices_sheets[i, "file"])
  sheet <- matrices_sheets[i, "sheet"]
  water_system <- str_extract(matrices_sheets[i, "file"],
                              "[a-z]*_") %>%
    str_remove("_")
  
  water_system_names <- data.frame(
    "long_name" = c("Ria Formosa", "Alvor", "Aljezur", "Arade", "Guadiana"),
    "short_name"= c("ria", "alvor", "aljezur", "arade", "guadiana")
  )
  
  # Change to full name so data can be joined later
  water_system <- water_system_names[water_system_names[,"short_name"] == water_system,"long_name"]
  
  # If a matrix has a species name equal to a temporary identification code
  # replace it with the species identification
  matrix <- read_xlsx(path = file,
                      sheet = sheet)
  species_to_replace  <- tolower(matrix$Species) %in% tolower(species_id$id)
  replacement_species <- species_id$species[tolower(species_id$id) %in% tolower(matrix$Species)]
  matrix$Species[species_to_replace] <- replacement_species
  
  # In case identified species are repeated, merge to previously identified
  matrix <- matrix %>%
    group_by(Species) %>%
    summarise_all(.funs = sum)
  
  # Replace NAs with 0
  matrix[is.na(matrix)] <- 0
  
  # Add unvegetated values
  matrix[nrow(matrix) + 1, c(2:ncol(matrix))] <- apply(
    X = matrix[,-1], 
    MARGIN = 2, 
    FUN = function(x) 100 - sum(x))
  
  matrix[nrow(matrix), 1] <- "unvegeated"
  
  # Put all matrices in a list
  matrices_list[[i]] <- matrix
  
  # Create long format version
  matrix_long <- matrix %>%
    pivot_longer(cols = -Species, 
                 names_to = "quadrat",
                 values_to = "cover",
                 values_drop_na = TRUE)
  
  matrix_long$transect_id <- as.numeric(sheet)
  matrix_long$water_system <- water_system
  
  matrices_long <- bind_rows(matrices_long, matrix_long)
}

saltmarsh_data <- left_join(saltmarsh_gps, 
                            transect_lengths, 
                            by = c("water_system","transect_id")) %>%
  select(water_system, water_body, site_nr, transect_nr, transect_id, name, start_end, date, 
         latitude, longitude, notes) %>%
  arrange(water_system, water_body, site_nr, transect_nr, desc(start_end))

saltmarsh_summary <- left_join(saltmarsh_gps, 
                            transect_lengths, 
                            by = c("water_system","transect_id")) %>%
  group_by(water_system, transect_id) %>%
  summarise(water_body   = unique(water_body),
            site_nr      = unique(site_nr),
            name         = unique(name), 
            date         = unique(date),
            time         = paste(time, collapse = " - "),
            length       = unique(length),
            notes        = paste(unique(notes), collapse = " ")) %>%
  select(water_system, water_body, site_nr, transect_id, name, date, time, length, notes) %>%
  arrange(water_system, water_body, site_nr)

matrices_long <- matrices_long %>%
  left_join(saltmarsh_summary[,c("water_system", "water_body", "site_nr", "transect_id")],
            by = c("water_system","transect_id")) %>%
  select(water_system, water_body, site_nr, transect_id, quadrat, 
         species = Species, cover)
# Save data to excel ------------------------------------------------------
# Export the cover matrices
mapply(function(excel_file, sheet_name, df) df_to_xlsx(paste0("./3_data/clean/saltmarsh_cover/", excel_file), sheet_name, df),
       excel_file  = matrices_sheets$file,
       sheet_name = matrices_sheets$sheet,
       df = matrices_list)

# Save a clean file with summary data and gps points
excel_file <- "./3_data/clean/monipor_clean_data.xlsx"
## Add sheets
# Opportunistic macroalgae
df_to_xlsx(excel_file, 
           sheet_name = "macroalgae", 
           df = macroalgae_data, 
           merge_cols = c("water_system", "water_body", "site_nr"))

# Seagrass summary
df_to_xlsx(excel_file, 
           sheet_name = "seagrass_summary", 
           df = seagrass_summary,
           merge_cols = c("water_system", "water_body"))

# Seagrass full data
df_to_xlsx(excel_file, 
           sheet_name = "seagrass_data", 
           df = seagrass_data,
           merge_cols = c("water_system", "water_body", "site_nr"))

# Saltmarsh transect
df_to_xlsx(excel_file, 
           sheet_name = "saltmarsh_summary", 
           df = saltmarsh_summary,
           merge_cols = c("water_system", "water_body", "site_nr"))

# Saltmarsh full data
df_to_xlsx(excel_file, 
           sheet_name = "saltmarsh_data", 
           df = saltmarsh_data,
           merge_cols = c("water_system", "water_body", "site_nr", "transect_nr"))

# Saltmarsh long format matrices
df_to_xlsx(excel_file, 
           sheet_name = "matrices_long", 
           df = matrices_long)
