# ------------------------------------------------------------------------------
# Script: data_cleaning.R
# Author: Bisrat
#
# Description:
# This script handles the data cleaning and preprocessing for the analysis of
# rainfall data in Ethiopia. It loads raster data, processes it into a data frame,
# reshapes the data for further analysis, and outputs the cleaned data as CSV files.
# The script also filters and selects the relevant data, merging it with additional
# metadata for analysis. The data includes raster data for rainfall from multiple sources
# for the years 2018 and 2021. The output files are saved as CSV for further use
# in the plotting and analysis scripts.
#
# The following steps are performed:
# 1. Loads necessary packages for data manipulation and spatial analysis.
# 2. Loads filtered raster files for rainfall data and processes them.
# 3. Reshapes the data frame and merges it with metadata.
# 4. Summarizes the annual rainfall totals and saves the results into CSV files.
# ------------------------------------------------------------------------------

# Setup package loading and data cleaning functions
library(pacman)
pacman::p_load(
  haven,
  here,
  plotly,
  leaflet,
  ggthemes,
  ggstatsplot,
  sjPlot,
  sjlabelled,
  gtsummary,
  geosphere,
  ggpie,
  ggthemr,
  webr,
  gtools,
  xml2,
  pRecipe,
  cowplot,
  data.table,
  ggpubr,
  grid,
  raster,
  scales,
  reticulate,
  tidyverse,
  sf,
  terra
)

# Font settings
my_font <- "Segoe UI Black"
my_font_2 <- "Futura-Normal"
my_font_3 <- "Poppins"
my_size <- 12

# Load the shapefile for Ethiopia
ethiopia_shape_path <- here::here("data", "spatial_data", "gadm41_ETH_0.shp")
ethiopia_shape <- sf::st_read(ethiopia_shape_path)

# Load filtered raster files for Ethiopia rainfall data
filtered_files <- list.files(
  here::here("data", "raw_data", "cropped_rf_data"),
  pattern = "filtered",
  full.names = TRUE
)

ethiopia_filtered_all <- list()
for (i in seq_along(filtered_files)) {
  raster_brick <- raster::brick(filtered_files[i])
  file_name = sub("_.*", "", basename(filtered_files[i]))
  ethiopia_filtered_all[i] <- raster_brick
}

# Filter and keep only raster files with data for both 2018 and 2021 (nlayes = 24)
ethiopia_filtered <- ethiopia_filtered_all[sapply(ethiopia_filtered_all, nlayers) == 24]

# Read data sources CSV file
data_sources <- read_csv(here::here("tmp", "data_sources.csv"))

# Convert raster data to data frames and combine them
ethiopia_df_list <- list()
for (i in seq_along(ethiopia_filtered)) {
  raster_brick <- ethiopia_filtered[[i]]
  raster_df <- terra::as.data.frame(raster_brick, xy = TRUE)
  raster_df$variable <- names(ethiopia_filtered)[i]
  ethiopia_df_list[[i]] <- raster_df
}

ethiopia_df <- do.call(rbind, ethiopia_df_list)

# Pivot and reshape data frame for further analysis
ethiopia_df_2 <- ethiopia_df %>%
  pivot_longer(
    cols = -c(variable, x, y),
    names_to = "date",
    values_to = "tp"
  ) %>%
  mutate(year = as.numeric(substr(date, 2, 5)), month = as.numeric(substr(date, 7, 8))) %>%
  select(variable, x, y, date, year, month, tp) %>%
  left_join(data_sources, by = c("variable" = "variable")) %>%
  filter(!is.na(tp)) %>%
  mutate(month = factor(month, levels = 1:12, labels = month.abb))

# Write cleaned data to CSV
write_csv(ethiopia_df_2, here("tmp", "ethiopia_df_2.csv"))
write_csv(data_sources, here("tmp", "data_sources.csv"))

# Summarize annual rainfall totals by dataset and location
ethiopia_annual <- ethiopia_df_2 %>%
  group_by(variable, year, x, y) %>%
  summarise(tp = sum(tp)) %>%
  left_join(data_sources, by = c("variable" = "variable")) %>%
  mutate(dataset_2 = paste0(dataset, '\n', "[", product_type, "]"))

# Filter annual data for 2018 and 2021 and save to CSV
ethiopia_annual_2018 <- ethiopia_annual %>% filter(year == 2018)
ethiopia_annual_2021 <- ethiopia_annual %>% filter(year == 2021)

write_csv(ethiopia_annual_2018,
          here("tmp", "ethiopia_annual_2018.csv"))
write_csv(ethiopia_annual_2021,
          here("tmp", "ethiopia_annual_2021.csv"))
