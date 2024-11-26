#----------------------------------------------------------------------------------------------------
# Script: data_cleaning.R
# Author: Bisrat
# This script processes rainfall data for Ethiopia, performing the following steps:

# 1. Load Libraries: Uses pacman to load necessary libraries for data processing and visualization.
# 2. Define Paths: Sets paths for rainfall data and the Ethiopia shapefile.
# 3. Read Shapefile: Reads Ethiopia's shapefile using sf package.
# 4. Subset Rainfall Data:
#    - Iterates over each rainfall data file.
#    - Subsets data for the years 2017 to 2021 within the specified geographic bounding box.
#    - Saves subsetted data; skips files with no data for the specified region.
# 5. Crop Rainfall Data:
#    - Iterates over subsetted datasets.
#    - Crops data to Ethiopia's shape using terra package.
#    - Saves cropped data.
# 6. Filter Rainfall Data:
#    - Iterates over cropped datasets.
#    - Filters data for the years 2018 and 2021.
#    - Saves filtered data; skips files with no data for these years.
# 7. Simplify Dataset Names: Simplifies the names of the filtered datasets for easier reference.
#----------------------------------------------------------------------------------------------------

# Load required libraries using pacman for easier management
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
  tidyverse
)

# Define the path to the rainfall data directory
rain_fall_data_path <- here("data", "raw_data", "rainfall_data")

# Get a list of all file names in the rainfall data directory
prec_names <- list.files(rain_fall_data_path, full.names = TRUE)

# Define the path to the Ethiopia shapefile and read it using sf
ethiopia_shape_path <- here::here("data", "spatial_data", "gadm41_ETH_0.shp")
ethiopia_shape <- sf::st_read(ethiopia_shape_path)

# Initialize an empty list to store subsetted rainfall data
ethiopia_subset <- list()

# Loop through each rainfall data file to subset the data for Ethiopia
for (dataset_count in 1:length(prec_names)) {
  # Define the output path for the subsetted data
  output_path <- here::here("data",
                            "raw_data",
                            "subset_rf_data",
                            paste0("subset_", basename(prec_names[dataset_count])))
  # Subset the data for the specified years and bounding box
  subsetted_data <- pRecipe::subset_data(
    x = prec_names[dataset_count],
    yrs = c(2017, 2021),
    box = c(32.95418, 47.78942, 3.42206, 14.95943)
  )
  
  # If no data is found for the specified region, print a message and skip
  if (nlayers(subsetted_data) == 0) {
    print(paste0(
      "No data for ",
      basename(prec_names[dataset_count]),
      " in the specified region."
    ))
    next
  }
  
  # Rename the time dimension to "time_2" and save the subsetted data
  names(subsetted_data@z) <- "time_2"
  terra::writeRaster(subsetted_data, output_path, overwrite = TRUE)
  ethiopia_subset[basename(prec_names[dataset_count])] <- subsetted_data
  print(paste0("Subsetted data for ", basename(prec_names[dataset_count]), " has been saved."))
}

# Initialize an empty list to store cropped rainfall data
ethiopia_cropped <- list()

# Loop through each subsetted dataset to crop the data to the Ethiopia shape
for (i in seq_along(ethiopia_subset)) {
  if (nlayers(ethiopia_subset[[i]]) != 0) {
    # Define the output path for the cropped data
    output_path <- here::here("data",
                              "raw_data",
                              "cropped_rf_data",
                              paste0("cropped_", names(ethiopia_subset)[i]))
    raster_brick <- ethiopia_subset[[i]]
    crs(raster_brick) <- st_crs(ethiopia_shape)
    cropped_raster <- terra::mask(raster_brick, ethiopia_shape)
    names(cropped_raster@z) <- "time_2"
    terra::writeRaster(cropped_raster, output_path, overwrite = TRUE)
    ethiopia_cropped[[names(ethiopia_subset)[i]]] <- cropped_raster
  }
}

# Initialize an empty list to store filtered rainfall data
ethiopia_filtered <- list()

# Loop through each cropped dataset to filter data for the years 2018 and 2021
for (i in seq_along(ethiopia_cropped)) {
  raster_brick <- ethiopia_cropped[[i]]
  layer_names <- names(raster_brick)
  layer_years <- as.numeric(substr(layer_names, 2, 5))
  layer_indices <- which(layer_years %in% c(2018, 2021))
  filtered_raster <- raster_brick[[layer_indices]]
  
  # If there is no data for either 2018 or 2021, print a message and skip
  if (nlayers(filtered_raster) != 24) {
    print(paste0(
      "No data for ",
      names(ethiopia_cropped)[i],
      " either 2018 or 2021."
    ))
    next
  }
  
  # Store the filtered raster data and save it
  ethiopia_filtered[[names(ethiopia_cropped)[i]]] <- filtered_raster
  terra::writeRaster(
    filtered_raster,
    here::here(
      "data",
      "raw_data",
      "cropped_rf_data",
      paste0("filtered_", names(ethiopia_cropped)[i])
    ),
    overwrite = TRUE
  )
}

# Simplify names of the filtered datasets for easier reference
names(ethiopia_filtered) <- sub("_.*", "", names(ethiopia_filtered))
