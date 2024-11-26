# ------------------------------------------------------------------------------
# Script: Data Processing and Analysis
# Author: Bisrat
#
# Description:
# This script processes the LSMS data for Ethiopia from the 2018 and 2021 datasets.
# The data includes household, plot, and crop-level information, including rain data, with the 
# goal of conducting statistical analysis. The script performs data cleaning, manipulation, 
# and creates visualizations for key variables like maize yield and nitrogen application.
# ------------------------------------------------------------------------------
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

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
  corrplot,
  webr,
  gtools,
  xml2,
  raster,
  tidyverse,
  DescTools,
  plm
)
extrafont::loadfonts(quiet = T)
my_font <- "Frutiger"
my_font_2 <- "Segoe UI"


my_size <- 12
my_colors <- list(
  "#F09C26",
  "#50AE5E",
  "#BFCCB5",
  "#7C96AB",
  "#B7B7B7",
  "#EDC6B1",
  "#F5F5DC",
  "#A9A9A9",
  "#FFC0CB",
  "#8B0000",
  "#8A2BE2",
  "#8F9779"
)

colors <- c(
  "#8F9779",
  "#FFD700",
  "#8B4513",
  "#87CEEB",
  "#FFA500",
  "#F5F5DC",
  "#A9A9A9",
  "#FFC0CB",
  "#8B0000",
  "#596baf",
  "#8A2BE2"
)

my_colors_1 <- c(
  "#ffae49",
  "#44b7c2",
  "#024b7a",
  "#ee4144",
  "#1f5e77",
  "#c792e9",
  "#5eeda0",
  "#019d9c",
  "#83329b"
)

likert_colors <- c("#a3336d", "#b55b8a", "#7a88bf", "#596baf", "#e7e7e7")

my_colors <- c(
  "#1f77b4",
  "#ff7f0e",
  "#2ca02c",
  "#d62728",
  "#9467bd",
  "#8c564b",
  "#e377c2",
  "#7f7f7f",
  "#bcbd22",
  "#17becf"
)
color_new <- c(
  "#F2DFEB",
  "#AEDFF2",
  "#72DBF2",
  "#F2C879",
  "#F2D8A7",
  "#8990B3",
  "#FFD3C4",
  "#DEE3FF",
  "#DEFFC4",
  "#A0B392"
)
Darjeeling1 <- wesanderson::wes_palettes$Darjeeling1
Darjeeling2 <- wesanderson::wes_palettes$Darjeeling2
Royal2 <- wesanderson::wes_palettes$Royal2
Moonrise1 <- wesanderson::wes_palettes$Moonrise1
Zissou1 <- wesanderson::wes_palettes$Zissou1Continuous
Chevalier1 <- wesanderson::wes_palettes$Chevalier1
Cavalcanti1 <- wesanderson::wes_palettes$Cavalcanti1
GrandBudapest1 <- wesanderson::wes_palettes$GrandBudapest1
bar_colors <- c("#1B81BB",
                "#288D8D",
                "#5F4C60",
                "#7570B3",
                "#9F0000",
                "#FFBB70",
                "#bcbd22")

# devtools::install_github('cttobin/ggthemr')
ggthemr("pale")



# Function to find non-common column names between two data frames
#' This function takes a list of data frames and identifies columns that are not common across all data frames in the list.
#' It returns a list where each element corresponds to a data frame from the input list and contains the columns that are not present in that specific data frame but are present in at least one other data frame in the list.
#'
#' @param df_list A list of data frames to analyze for non-common columns.
#'
#' @return A list where each element is a vector of column names. These column names are not present in the corresponding data frame from the input list but appear in at least one other data frame in the list.

findNonCommonColumns <- function(df_list) {
  all_columns <- unique(unlist(lapply(df_list, colnames)))
  non_common_columns_list <- list()
  for (df in df_list) {
    non_common_columns <- setdiff(all_columns, colnames(df))
    non_common_columns_list[[length(non_common_columns_list) + 1]] <- non_common_columns
  }
  return(non_common_columns_list)
}

#' Check Unique Identifiers
#'
#' This function checks whether the specified columns in a data frame are unique identifiers. If a single column is provided, it checks if that column is a unique identifier. If multiple columns are provided, it checks if the combination of those columns is a unique identifier.
#'
#' @param data A data frame containing the data to be checked.
#' @param cols A character vector specifying the column names to be checked.
#'
#' @return A character vector with a message indicating whether the specified column or combination of columns is a unique identifier.
#'

#'
#' @export
check_unique_identifiers <- function(data, cols) {
  if (length(cols) == 1) {
    col <- cols[1]
    if (n_distinct(data[[col]]) == nrow(data)) {
      return(paste(col, "is a unique identifier."))
    } else {
      return(paste(col, "is not a unique identifier."))
    }
  } else {
    combined_cols <- data %>%
      unite("combined",
            all_of(cols),
            sep = "-",
            remove = FALSE)
    if (n_distinct(combined_cols$combined) == nrow(data)) {
      return(paste(
        paste(cols, collapse = ", "),
        "are a unique identifier when combined."
      ))
    } else {
      return(paste(
        paste(cols, collapse = ", "),
        "are not a unique identifier when combined."
      ))
    }
  }
}

# Define the impute_percentiles function
impute_percentiles <- function(column) {
  # Calculate the 1st and 90th percentiles
  p1 <- quantile(column, probs = 0.1, na.rm = TRUE)
  p90 <- quantile(column, probs = 0.90, na.rm = TRUE)
  
  # Replace the values below the 1st percentile and above the 90th percentile with NA
  column[column < p1 | column > p90] <- NA
  
  return(column)
}





# DONOT RUN THIS
# list all the files in the raw_data folder

# intermediated_data_path <- here::here("data", "intermediated_data")
# raw_data_path <- here::here("data", "raw_data")
# raw_data_names <- list.files(raw_data_path, full.names = TRUE)
# Loop through each filename
# for (filename in raw_data_names) {
#  # Extract the year from the filename
#  year <- str_extract(filename, "\\d{4}")
#
#  # Define the path for the year folder
#  year_folder <- file.path(intermediated_data_path, year)
#
#  # Create the year folder if it doesn't exist
#  if (!dir.exists(year_folder)) {
#    dir.create(year_folder)
#  }
#
#  # Unzip the file into the year folder
#  unzip(filename, exdir = year_folder)
# }





# Define the function to extract metadata
extract_metadata_from_lsms <- function(year) {
  # Determine the file path based on the year
  if (year == 2018) {
    xml_file <- read_xml(here::here("tools", "ETH_2018_ESS_v03_M.xml"))
  } else if (year == 2021) {
    xml_file <- read_xml(here::here("tools", "ETH_2021_ESPS-W5_v01_M.xml"))
  } else {
    stop("Year not supported. Please provide either 2018 or 2021.")
  }
  
  # Define the namespace
  ns <- c(ddi = "ddi:codebook:2_5")
  
  # Extract information from each fileDscr
  fileDscr_nodes <- xml_find_all(xml_file, ".//ddi:fileDscr", ns)
  
  # Extract relevant information from each fileDscr node, including the ID attribute
  file_info <- lapply(fileDscr_nodes, function(node) {
    ID_ <- xml_attr(node, "ID")
    fileName <- xml_text(xml_find_first(node, ".//ddi:fileName", ns))
    caseQty <- xml_text(xml_find_first(node, ".//ddi:caseQnty", ns))
    varQty <- xml_text(xml_find_first(node, ".//ddi:varQnty", ns))
    fileCont <- xml_text(xml_find_first(node, ".//ddi:fileCont", ns))
    
    data.frame(
      ID_ = ID_,
      fileName,
      caseQty,
      varQty,
      fileCont,
      stringsAsFactors = FALSE
    )
  })
  
  # Combine the extracted information into a single data frame
  file_info_df <- bind_rows(file_info)
  
  # Extract information from each dataDscr
  dataDscr_nodes <- xml_find_all(xml_file, ".//ddi:dataDscr/ddi:var", ns)
  
  # Extract relevant information from each dataDscr node, including ID, name, files, interval, label, and categories
  data_info <- lapply(dataDscr_nodes, function(node) {
    ID <- xml_attr(node, "ID")
    name <- xml_attr(node, "name")
    files <- xml_attr(node, "files")
    interval <- xml_attr(node, "intrvl")
    label <- xml_text(xml_find_first(node, ".//ddi:labl", ns))
    
    # Extract categories
    catgry_nodes <- xml_find_all(node, ".//ddi:catgry", ns)
    catgry_labels <- sapply(catgry_nodes, function(cat_node) {
      catLabl <- xml_text(xml_find_first(cat_node, ".//ddi:labl", ns))
      catLabl
    })
    
    catgry_string <- paste(catgry_labels, collapse = "; ")
    
    data.frame(ID,
               name,
               files,
               interval,
               label,
               catgry_string,
               stringsAsFactors = FALSE)
  })
  
  # Combine the extracted information into a single data frame
  data_info_df <- bind_rows(data_info)
  
  # Merge the data frames
  meta_table <- data_info_df %>%
    left_join(file_info_df, by = c("files" = "ID_"))
  
  return(meta_table)
}

meta_table_2018 <- extract_metadata_from_lsms(2018)
meta_table_2021 <- extract_metadata_from_lsms(2021)




# read all the data for 2018 and 2021

file_names_2018 <- list.files(here::here("data", "intermediated_data", "2018"), full.names = TRUE)
file_names_2021 <- list.files(here::here("data", "intermediated_data", "2021"), full.names = TRUE)

data_list_2018 <- map(file_names_2018, ~ as_factor(read_dta(.x), only_labelled = TRUE))
names(data_list_2018) <- gsub(".dta", "", basename(file_names_2018))
data_list_2021 <- map(file_names_2021, ~ as_factor(read_dta(.x), only_labelled = TRUE))
names(data_list_2021) <- gsub(".dta", "", basename(file_names_2021))


#------------------------------------------------------------------------------------------


hh_identification_2018 <- data_list_2018$sect_cover_hh_w4 %>%
  # remove columns that containd CONFIDENTIAL inheir row
  dplyr::select(-which(sapply(., function(col)
    any(
      grepl("\\**CONFIDENTIAL\\**", col)
    ))))

check_unique_identifiers(hh_identification_2018, c("ea_id", "household_id"))

hh_geovars_2018 <- data_list_2018$ETH_HouseholdGeovariables_Y4 %>%
  dplyr::select(household_id, twi, srtm1k, slopepct, lat_mod, lon_mod) %>%
  rename(
    twi_ne = twi,
    srtm_1k = srtm1k,
    afmnslp_pct = slopepct,
    lat_dd_mod = lat_mod,
    lon_dd_mod = lon_mod
  )
hh_geovars_2021 <- data_list_2021$eth_householdgeovariables_y5 %>%
  dplyr::select(household_id,
                twi_ne,
                srtm_1k,
                afmnslp_pct,
                lat_dd_mod,
                lon_dd_mod)

check_unique_identifiers(hh_geovars_2018, c("household_id"))

# anti_join(hh_identification_2018, hh_geovars_2018, by = "household_id")
only_in_gps <- anti_join(hh_geovars_2018, hh_identification_2018, by = "household_id")

plot_geovars_2018 <- data_list_2018$ETH_PlotGeovariables_Y4
plot_geovars_2021 <- data_list_2021$eth_plotgeovariables_y5 %>%
  dplyr::select(household_id,
                twi_ne,
                srtm_1k,
                afmnslp_pct,
                lat_dd_mod,
                lon_dd_mod)



#----------------------------------------------------------------------------------------------------

# Household adult equivalency units (units)
# Male plot manager (1/0)
# Hired labor (1/0)
# Any household member could sell land (1/0)
# Age of plot manager (years)
# Owned household assets (Thousand birr)
# Area planted (hectares)
# Nitrogen applied (kilograms per hectare)
# Phosphorus per hectare (kilograms)
# Seeding rate (kilograms per hectare)
# Organic fertilizer (1/0)
# Farmer purchased seed (1/0)
# Mechanization (1/0)
# Animal traction use (1/0)
# Irrigation (1/0)
# Agro chemical use (1/0)
# Topographic wetness index (units)
# Slope (percent)
# Annual mean temperature (°C×10)
# Annual precipitation (mm)
# Plot elevation (meters)
# No other crop planted (1/0)
# One other crops planted (1/0)
# Two other crops planted (1/0)
# Three or more other crops planted (1/0)
# Legume grown on plot (1/0)
# Maize price (birr per kilograms)
# Fertilizer price (birr per kilograms)
#----------------------------------------------------------------------------------------------------

# household adult equivalency units
adult_equivalance_scale_oecd1_2018 <- data_list_2018$sect1_hh_w4 %>%
  dplyr::select(household_id, individual_id, s1q02, s1q03a) %>%
  arrange(household_id, desc(s1q03a)) %>%
  group_by(household_id) %>%
  mutate(
    weight = case_when(
      s1q03a >= 14 & row_number() == 1 ~ 1,
      s1q03a >= 14 & row_number() > 1 ~ 0.5,
      s1q03a < 14 ~ 0.3
    )
  ) %>%
  group_by(household_id) %>%
  summarise(household_adult_equivalency_units_oecd2 = sum(weight))

adult_equivalance_scale_oecd2_2018 <- data_list_2018$sect_cover_hh_w4 %>%
  dplyr::select(household_id, saq09) %>%
  # take the square root of the number of household members
  mutate(household_adult_equivalency_oecd3 = round(sqrt(saq09), 2)) %>%
  dplyr::select(-saq09)

v1_household_adult_equivalency_units_2018 <- left_join(adult_equivalance_scale_oecd1_2018,
                                                       adult_equivalance_scale_oecd2_2018,
                                                       by = "household_id") %>%
  sjlabelled::var_labels(
    household_adult_equivalency_units_oecd2 = "Household adult equivalency units (OECD2)",
    household_adult_equivalency_oecd3 = "Household adult equivalency units (OECD3)"
  )

# 2021
adult_equivalance_scale_oecd1_2021 <- data_list_2021$sect1_hh_w5 %>%
  dplyr::select(household_id, individual_id, s1q02, s1q03a) %>%
  arrange(household_id, desc(s1q03a)) %>%
  group_by(household_id) %>%
  mutate(
    weight = case_when(
      s1q03a >= 14 & row_number() == 1 ~ 1,
      s1q03a >= 14 & row_number() > 1 ~ 0.5,
      s1q03a < 14 ~ 0.3
    )
  ) %>%
  group_by(household_id) %>%
  summarise(household_adult_equivalency_units_oecd2 = sum(weight))

adult_equivalance_scale_oecd2_2021 <- data_list_2021$sect_cover_hh_w5 %>%
  dplyr::select(household_id, saq09) %>%
  # take the square root of the number of household members
  mutate(household_adult_equivalency_oecd3 = round(sqrt(saq09), 2)) %>%
  dplyr::select(-saq09)

v1_household_adult_equivalency_units_2021 <- left_join(adult_equivalance_scale_oecd1_2021,
                                                       adult_equivalance_scale_oecd2_2021,
                                                       by = "household_id") %>%
  sjlabelled::var_labels(
    household_adult_equivalency_units_oecd2 = "Household adult equivalency units (OECD2)",
    household_adult_equivalency_oecd3 = "Household adult equivalency units (OECD3)"
  )

#----------------------------------------------------------------------------------------------------

individaul_gender_2018 <- data_list_2018$sect1_hh_w4 %>% dplyr::select(household_id, individual_id, s1q02, s1q03a)
individual_education_2018 <- data_list_2018$sect2_hh_w4 %>% dplyr::select(household_id, individual_id, s2q06)
v2_male_plot_manager_2018 <- data_list_2018$sect3_pp_w4 %>%
  dplyr::select(household_id, parcel_id, field_id, s3q13) %>%
  rename(individual_id = s3q13) %>%
  left_join(individaul_gender_2018) %>%
  left_join(individual_education_2018) %>%
  filter(!is.na(individual_id)) %>%
  rename(s3q13 = individual_id)

# 2021
individaul_gender_2021 <- data_list_2021$sect1_hh_w5 %>% dplyr::select(household_id, individual_id, s1q02, s1q03a)
individual_education_2021 <- data_list_2021$sect2_hh_w5 %>% dplyr::select(household_id, individual_id, s2q06)

v2_male_plot_manager_2021 <- data_list_2018$sect3_pp_w4 %>%
  dplyr::select(household_id, parcel_id, field_id, s3q13) %>%
  rename(individual_id = s3q13) %>%
  left_join(individaul_gender_2021) %>%
  left_join(individual_education_2021) %>%
  filter(!is.na(individual_id)) %>%
  rename(s3q13 = individual_id)

#----------------------------------------------------------------------------------------------------

plot_holder_characterstics_individual_2018 <- data_list_2018$sect1_pp_w4 %>%
  dplyr::select(ends_with("_id"), saq15, s1q03, s1q04)
plot_holder_characterstics_2018 <- data_list_2018$sect_cover_pp_w4 %>%
  dplyr::select(ends_with("_id"), saq16, saq12)

#----------------------------------------------------------------------------------------------------


parcel_characterstics_2018 <- data_list_2018$sect2_pp_w4 %>%
  dplyr::select(ends_with("_id"), s2q02, s2q03, s2q05, s2q06, s2q16, s2q17)

plot_characterstics_2018 <- data_list_2018$sect3_pp_w4 %>%
  dplyr::select(
    ends_with("_id"),
    s3q02a,
    s3q02b,
    s3q2b_os,
    s3q03,
    s3q03b,
    s3q04,
    s3q05,
    s3q07,
    s3q08,
    s3q13,
    s3q16,
    s3q17,
    s3q21,
    s3q21a,
    s3q22,
    s3q22a,
    s3q23,
    s3q23a,
    s3q24,
    s3q24a,
    s3q25,
    s3q26,
    s3q27,
    s3q27a,
    s3q30a,
    s3q30d,
    s3q30g,
    s3q31a,
    s3q40,
    s3q35,
    s3q34,
    s3q38,
    s3q36
  )
#----------------------------------------------------------------------------------------------------

crop_production_2018 <- data_list_2018$sect4_pp_w4 %>%
  dplyr::select(ends_with("_id"), s4q02, s4q03, s4q04, s4q05:s4q12)

crop_cut_information_2018 <- data_list_2018$sect9a_pp_w4 %>%
  dplyr::select(ends_with("_id"), sccq01, sccq03, sccq04, sccq05)

crop_harvest_2018 <- data_list_2018$sect9_ph_w4 %>%
  dplyr::select(ends_with("_id"), s9q00b, s9q03:s9q15)

crop_harvest_labour_mechanization_2018 <- data_list_2018$sect10_ph_w4 %>%
  dplyr::select(ends_with("_id"),
                s10q01a,
                s10q01d,
                s10q01g,
                s10q04,
                s10q05,
                s10q06)

plot_holder_characterstics_individual_2021 <- data_list_2021$sect1_pp_w5 %>%
  dplyr::select(ends_with("_id"), any_of(colnames(
    plot_holder_characterstics_individual_2018
  )))

plot_holder_characterstics_2021 <- data_list_2021$sect_cover_pp_w5 %>%
  dplyr::select(ends_with("_id"), any_of(colnames(plot_holder_characterstics_2018)))

parcel_characterstics_2021 <- data_list_2021$sect2_pp_w5 %>%
  dplyr::select(ends_with("_id"), any_of(colnames(parcel_characterstics_2018)))

plot_characterstics_2021 <- data_list_2021$sect3_pp_w5 %>%
  dplyr::select(ends_with("_id"), any_of(colnames(plot_characterstics_2018)))

crop_production_2021 <- data_list_2021$sect4_pp_w5 %>%
  dplyr::select(ends_with("_id"), any_of(colnames(crop_production_2018)))

crop_cut_information_2021 <- data_list_2021$sect9a_pp_w5 %>%
  dplyr::select(ends_with("_id"), any_of(colnames(crop_cut_information_2018)))

crop_harvest_2021 <- data_list_2021$sect9_ph_w5 %>%
  dplyr::select(ends_with("_id"), any_of(colnames(crop_harvest_2018)))

crop_harvest_labour_mechanization_2021 <- data_list_2021$sect10_ph_w5 %>%
  dplyr::select(ends_with("_id"), any_of(colnames(crop_harvest_labour_mechanization_2018)))
#----------------------------------------------------------------------------------------------------

# filter plots that have maize production
maize_production_2018_1 <- crop_harvest_2018 %>%
  filter(s9q00b == "2. MAIZE")

# count number of crops harvested by plot
total_number_of_crops_perplot <- crop_harvest_2018 %>%
  group_by(holder_id, household_id, parcel_id, field_id) %>%
  summarise(total_crops = n_distinct(s9q00b))

maize_production_2018 <- maize_production_2018_1 %>%
  left_join(total_number_of_crops_perplot) %>%
  mutate(num_other_crops = total_crops - 1) %>%
  dplyr::select(-total_crops) %>%
  left_join(crop_harvest_labour_mechanization_2018) %>%
  left_join(crop_cut_information_2018) %>%
  left_join(crop_production_2018) %>%
  left_join(plot_characterstics_2018) %>%
  left_join(parcel_characterstics_2018) %>%
  left_join(plot_holder_characterstics_2018) %>%
  left_join(hh_geovars_2018) %>%
  left_join(v1_household_adult_equivalency_units_2018) %>%
  dplyr::select(-s2q06) %>%
  left_join(v2_male_plot_manager_2018) %>%
  mutate(year = 2018) %>%
  dplyr::select(-s9q03) %>%
  # drop duplicated plot ids
  
  distinct(household_id, parcel_id, field_id, .keep_all = TRUE) %>%
  mutate(unique_plot_id = paste(household_id, parcel_id, field_id, sep = "")) %>%
  # replace missing values with 0 in s3q21a, s3q22a, s3q23a
  replace_na(list(
    s3q21a = 0,
    s3q22a = 0,
    s3q23a = 0
  )) %>%
  mutate(
    Total_Nitrogen = (s3q21a * 0.46) + (s3q22a * 0.18) + (s3q23a * 0.19),
    Total_Phosphorus = (s3q22a * 0.46) + (s3q23a * 0.38)
  )

# filter plots that have maize production
maize_production_2021_1 <- crop_harvest_2021 %>%
  filter(s9q00b == "2. 2.MAIZE")

# count number of crops harvested by plot
total_number_of_crops_perplot_2021 <- crop_harvest_2021 %>%
  group_by(holder_id, household_id, parcel_id, field_id) %>%
  summarise(total_crops = n_distinct(s9q00b))

maize_production_2021 <- maize_production_2021_1 %>%
  left_join(total_number_of_crops_perplot_2021) %>%
  mutate(num_other_crops = total_crops - 1) %>%
  dplyr::select(-total_crops) %>%
  left_join(crop_harvest_labour_mechanization_2021) %>%
  left_join(crop_cut_information_2021) %>%
  left_join(crop_production_2021) %>%
  left_join(plot_characterstics_2021) %>%
  left_join(parcel_characterstics_2021) %>%
  left_join(plot_holder_characterstics_2021) %>%
  left_join(hh_geovars_2021) %>%
  left_join(v1_household_adult_equivalency_units_2021) %>%
  dplyr::select(-s2q06) %>%
  left_join(v2_male_plot_manager_2021) %>%
  mutate(year = 2021) %>%
  dplyr::select(-s9q03) %>%
  # drop duplicated plot ids
  distinct(household_id, parcel_id, field_id, .keep_all = TRUE) %>%
  mutate(unique_plot_id = paste(household_id, parcel_id, field_id, sep = "")) %>%
  # replace missing values with 0 in s3q21a, s3q22a, s3q23a
  replace_na(list(
    s3q21a = 0,
    s3q22a = 0,
    s3q23a = 0
  )) %>%
  mutate(
    Total_Nitrogen = (s3q21a * 0.46) + (s3q22a * 0.18) + (s3q23a * 0.19),
    Total_Phosphorus = (s3q22a * 0.46) + (s3q23a * 0.38)
  )
#----------------------------------------------------------------------------------------------------


findNonCommonColumns(list(maize_production_2018, maize_production_2021))

check_unique_identifiers(maize_production_2018, c("unique_plot_id"))
check_unique_identifiers(maize_production_2021, c("unique_plot_id"))

maize_production <- bind_rows(maize_production_2018, maize_production_2021) %>%
  select(unique_plot_id, everything())

maize_production_pd <- plm::pdata.frame(maize_production, index = c("unique_plot_id", "year")) %>%
  filter(!is.na(s9q06))

# balanced sample
maize_production_pd_balanced <- plm::make.pbalanced(maize_production_pd, balance.type = "shared.individuals") %>%
  sjlabelled::copy_labels(maize_production_2018)

maize_production_1 <- maize_production %>%
  mutate(hired_labor = case_when(s10q01a + s10q01d + s10q01g + s3q30a + s3q30d + s3q30g > 0 ~ 1, TRUE ~ 0)) %>%
  dplyr::select(
    holder_id:ea_id,
    unique_plot_id,
    year,
    s9q06,
    num_other_crops,
    s10q04,
    s3q30a,
    s4q05:s4q7b,
    s4q11:s3q2b_os,
    s3q04,
    s3q08,
    s3q16,
    s3q17,
    s3q21:s3q23a,
    s3q25:s3q27,
    Total_Nitrogen,
    Total_Phosphorus,
    s3q40:s3q36,
    s2q03,
    s2q16:s2q06,
    lat_dd_mod,
    lon_dd_mod
  )


main_data <- maize_production_1 %>%
  mutate(
    plot_area = s3q08 / 10000,
    nitrogen_per_hectare = Total_Nitrogen / plot_area,
    phosphorus_per_hectare = Total_Phosphorus / plot_area,
    seeding_rate = s4q11a / plot_area,
    maize_yield = s9q06 / plot_area,
    household_adult_equivalency_units = household_adult_equivalency_units_oecd2,
    male_plot_manager = ifelse(s1q02 == "1. Male", 1, 0),
    age_plot_manager = s1q03a,
    organic_fertilizer = ifelse(s3q25 == "1. YES" |
                                  s3q25 == "1. YES" | s3q25 == "1. YES", 1, 0),
    farmer_purchased_seed = ifelse(as.numeric(s4q12) > 0, 1, 0),
    mechanization = ifelse(s10q04 != "3. No", 1, 0),
    animal_traction = ifelse(
      s3q35 != "5. DIGGING BY HAND" &
        s3q35 != "2. Using rented tractor" &
        s3q35 != "6. OTHER (SPECIFY" &
        s3q35 != "2. USING RENTED TRACTOR",
      1,
      0
    ),
    irrigation = ifelse(s3q21 == "1. YES", 1, 0),
    under_extension = ifelse(s3q16 == "1. YES", 1, 0),
    twi = twi_ne,
    slope = afmnslp_pct,
    elevation = srtm_1k,
    no_other_crop = ifelse(num_other_crops == 0, 1, 0),
    one_other_crop = ifelse(num_other_crops == 1, 1, 0),
    two_other_crops = ifelse(num_other_crops == 2, 1, 0),
    three_or_more_other_crops = ifelse(num_other_crops >= 3, 1, 0),
    legume = ifelse(s3q34 == "1. YES", 1, 0),
    document_for_plot = ifelse(s2q03 == "1. YES", 1, 0),
    soil_erosion_measure = ifelse(s3q38 == "1. YES", 1, 0),
    number_of_tillage = s3q36,
    predominant_soil_type = s2q16,
    percived_soil_quality = s2q17,
    agro_chemical_use = ifelse(s4q05 == "1. YES" |
                                 s4q06 == "1. YES" | s4q07 == "1. YES", 1, 0),
    agro_chemical_use = ifelse(is.na(agro_chemical_use), 0, agro_chemical_use)
  ) %>%
  mutate(
    number_of_tillage = case_when(
      number_of_tillage == "1. Planted with no tillin" ~ 0,
      number_of_tillage == "2. Once" ~ 1,
      number_of_tillage == "3. Twice" ~ 2,
      number_of_tillage == "5. Not at all" ~ 0,
      TRUE ~ 0
    ),
    number_of_tillage = factor(
      number_of_tillage,
      levels = 0:2,
      labels = c("None", "Once", "Twice")
    ),
    # remove digits and . from the soil type, make it sentence case
    predominant_soil_type = gsub("[0-9.]", "", tolower(predominant_soil_type)),
    # remove trailing spaces
    predominant_soil_type = str_to_sentence(trimws(predominant_soil_type)),
    predominant_soil_type = case_when(
      str_detect(predominant_soil_type, "Other") ~ NA,
      TRUE ~ predominant_soil_type
    ),
    percived_soil_quality = gsub("[0-9.]", "", tolower(percived_soil_quality)),
    percived_soil_quality = str_to_sentence(trimws(percived_soil_quality)),
    percived_soil_quality = factor(
      percived_soil_quality,
      levels = c("Poor", "Fair", "Good"),
      labels = c("Poor", "Fair", "Good")
    ),
    no_other_crop = as.factor(no_other_crop),
    one_other_crop = as.factor(one_other_crop),
    two_other_crops = as.factor(two_other_crops),
    three_or_more_other_crops = as.factor(three_or_more_other_crops),
    document_for_plot = as.factor(document_for_plot)
  ) %>%
  sjlabelled::var_labels(
    plot_area = "Area planted (hectares)",
    nitrogen_per_hectare = "Nitrogen applied (kilograms per hectare)",
    phosphorus_per_hectare = "Phosphorus per hectare (kilograms)",
    seeding_rate = "Seeding rate (kilograms per hectare)",
    maize_yield = "Maize yield (tons per hectare)",
    household_adult_equivalency_units = "Household adult equivalency units (OECD2)",
    male_plot_manager = "Male plot manager (1/0)",
    age_plot_manager = "Age of plot manager (years)",
    organic_fertilizer = "Organic fertilizer (1/0)",
    farmer_purchased_seed = "Farmer purchased seed (1/0)",
    mechanization = "Mechanization (1/0)",
    animal_traction = "Animal traction use (1/0)",
    irrigation = "Irrigation (1/0)",
    under_extension = "Under extension (1/0)",
    twi = "Topographic wetness index (units)",
    slope = "Slope (percent)",
    elevation = "Plot elevation (meters)",
    no_other_crop = "No other crop planted (1/0)",
    one_other_crop = "One other crops planted (1/0)",
    two_other_crops = "Two other crops planted (1/0)",
    three_or_more_other_crops = "Three or more other crops planted (1/0)",
    legume = "Legume grown on plot (1/0)",
    document_for_plot = "Document for plot (1/0)",
    soil_erosion_measure = "Soil erosion measure (1/0)",
    number_of_tillage = "Number of tillage",
    predominant_soil_type = "Predominant soil type",
    percived_soil_quality = "Percived soil quality",
    agro_chemical_use = "Agro chemical use (1/0)",
    year = "Year"
  ) %>%
  dplyr::select(ends_with("_id"),
                year,
                plot_area:agro_chemical_use,
                lat_dd_mod,
                lon_dd_mod)

summarytools::view(
  summarytools::dfSummary(main_data),
  file = here::here("reports", "main_data_summary.html")
)

main_data_tbl <- main_data %>%
  dplyr::select(-c(ends_with("_id")), unique_plot_id, household_id, ea_id) %>%
  filter(!is.na(year)) %>%
  # rownames_to_column() %>%
  as.data.frame() %>%
  dplyr::select(unique_plot_id, ea_id, everything())

gtsummary::tbl_summary(
  main_data_tbl,
  include = -c(unique_plot_id, household_id, ea_id, lat_dd_mod, lon_dd_mod),
  by = year,
  type = list(
    twi = "continuous",
    no_other_crop = "dichotomous",
    one_other_crop = "dichotomous",
    two_other_crops = "dichotomous",
    three_or_more_other_crops = "dichotomous",
    legume = "dichotomous",
    document_for_plot = "dichotomous",
    soil_erosion_measure = "dichotomous",
    number_of_tillage = "categorical"
  ),
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n}({p}%)"
  ),
  missing = "no"
) %>%
  # add_overall() %>%
  add_p(test.args = all_tests("t.test") ~ list(simulate.p.value = T)) %>%
  add_significance_stars(thresholds = c(0.01, 0.05, 0.10))


# list file that start with filtered
filtered_files <- list.files(
  here::here("data", "raw_data", "cropped_rf_data"),
  pattern = "filtered",
  full.names = TRUE
)

ethiopia_filtered_all <- list()
for (i in seq_along(filtered_files)) {
  raster_brick <- raster::brick(filtered_files[i])
  file_name <- sub("_.*", "", basename(filtered_files[i]))
  ethiopia_filtered_all[i] <- raster_brick
}

filtered_files_2 <- sub("filtered_", "", basename(filtered_files))
names(ethiopia_filtered_all) <- sub("_.*", "", filtered_files_2)

# keep only the rasters that have data for both 2018 and 2021 nlayes = 24
ethiopia_filtered <- ethiopia_filtered_all[sapply(ethiopia_filtered_all, nlayers) == 24]

#----------------------------------------------------------------------------------------------------

# Initialize an empty data frame for the final output
rainfall_data <- data.frame()

# Loop through each raster brick in the list
for (brick_name in names(ethiopia_filtered)) {
  # Get the raster brick
  
  print(paste0("Processing ", brick_name))
  raster_brick <- ethiopia_filtered[[brick_name]]
  
  # Extract values for the GPS points using terra::extract
  extracted_values <- terra::extract(raster_brick, gps_points)
  
  # Convert extracted values to a data frame
  extracted_df <- as.data.frame(extracted_values)
  
  # Add a column for GPS point indices (to uniquely identify each point)
  extracted_df <- cbind(unique_plot_id = gps_points$unique_plot_id, extracted_df)
  
  # Reshape the data frame from wide to long format using pivot_longer
  reshaped_df <- pivot_longer(
    extracted_df,
    cols = starts_with("X"),
    # Select all the columns that start with "X" (default column names from extract)
    names_to = "year_month",
    # The name for the time variable
    values_to = paste0(brick_name, "_rainfall") # The name for the rainfall values
  ) %>%
    mutate(year_month = as.character(year_month)) %>%
    separate(year_month,
             into = c("year", "month", "day"),
             sep = "\\.") %>%
    # remove X from the year column
    mutate(year = gsub("X", "", year)) %>%
    mutate(
      year = as.numeric(year),
      month = as.numeric(month),
      month = factor(month, levels = 1:12, labels = month.abb)
    ) %>%
    arrange(year, month) %>%
    # group by plot_id and year to calculate total rainfall per year
    group_by(unique_plot_id, year) %>%
    summarise(across(ends_with("_rainfall"), sum, na.rm = TRUE)) %>%
    ungroup()
  
  # If this is the first iteration, initialize the rainfall_data data frame
  if (nrow(rainfall_data) == 0) {
    rainfall_data <- reshaped_df
  } else {
    # Otherwise, merge the reshaped data frame with the existing rainfall_data
    rainfall_data <- merge(
      rainfall_data,
      reshaped_df,
      by = c("unique_plot_id", "year"),
      all = TRUE
    )
  }
}

#----------------------------------------------------------------------------------------------------

rainfall_data_2 <- rainfall_data

# plot correlation matrix
correlation_matrix <- rainfall_data_2 %>%
  dplyr::select(ends_with("_rainfall")) %>%
  cor(use = "pairwise.complete.obs")

# remove _rainfall from the column names and row names also capitalize
colnames(correlation_matrix) <- str_to_upper(gsub("_rainfall", "", colnames(correlation_matrix)))
rownames(correlation_matrix) <- str_to_upper(gsub("_rainfall", "", rownames(correlation_matrix)))


# using png save plot

png(
  filename = here::here("reports", "correlation_matrix_2.png"),
  width = 2000,
  height = 2000,
  res = 300,
  units = "px",
  family = my_font
)

corrplot::corrplot(
  correlation_matrix,
  method = "shade",
  type = "lower",
  order = "hclust",
  tl.col = "black",
  diag = FALSE,
  col = colorRampPalette(c("blue", "white", "#D35230"))(200),
  addCoef.col = "white",
  family = my_font,
  tl.cex = 0.7,
  number.cex = 0.7,
  tl.srt = 45
)

dev.off()
# save the correlation plot


png(
  filename = here::here("reports", "correlation_matrix.png"),
  width = 1200,
  height = 1200,
  res = 200,
  units = "px",
  family = my_font
)

# Generate the plot
corrplot::corrplot(
  correlation_matrix,
  method = "square",
  diag = FALSE,
  order = "hclust",
  tl.col = "black",
  addrect = 3,
  rect.col = "#D35230",
  rect.lwd = 3,
  col = COL2("PiYG", 10),
  family = my_font
)

# Close the device
dev.off()

#----------------------------------------------------------------------------------------------------

data_sources <- read_csv("../tmp/data_sources.csv")

region_name <- hh_identification_2018 %>%
  dplyr::select(ea_id, saq01) %>%
  as_tibble() %>%
  mutate(ea_id = as.character(ea_id)) %>%
  distinct()
main_data_with_rainfall_1 <- main_data_tbl %>%
  group_by(unique_plot_id) %>%
  mutate(
    wave = case_when(
      2018 %in% year & 2021 %in% year ~ "Both",
      2018 %in% year ~ "2018",
      2021 %in% year ~ "2021",
      TRUE ~ "none"
    )
  ) %>%
  mutate(unique_plot_id = as.character(unique_plot_id),
         ea_id = as.character(ea_id)) %>%
  left_join(rainfall_data_2, by = c("unique_plot_id", "year")) %>%
  rename(
    household_ae = household_adult_equivalency_units,
    cru_ts_rainfall = `cru-ts_rainfall`,
    ncep_doe_rainfall = `ncep-doe_rainfall`
  ) %>%
  left_join(region_name, by = c("ea_id" = "ea_id")) %>%
  mutate(farmer_purchased_seed = ifelse(is.na(farmer_purchased_seed), 0, farmer_purchased_seed)) %>%
  droplevels()

#----------------------------------------------------------------------------------------------------

main_data_with_rainfall <- main_data_with_rainfall_1 %>%
  group_by(year) %>%
  mutate_at(vars(
    c(
      "maize_yield",
      "nitrogen_per_hectare",
      "phosphorus_per_hectare",
      "seeding_rate"
    )
  ), as.numeric) %>%
  mutate_at(vars(
    c(
      "maize_yield",
      "nitrogen_per_hectare",
      "phosphorus_per_hectare",
      "seeding_rate"
    )
  ), impute_percentiles) %>%
  mutate_at(vars(
    c(
      "maize_yield",
      "nitrogen_per_hectare",
      "phosphorus_per_hectare",
      "seeding_rate"
    )
  ), ~ replace_na(., mean(., na.rm = TRUE))) %>%
  mutate_at(vars(
    c(
      "maize_yield",
      "nitrogen_per_hectare",
      "phosphorus_per_hectare",
      "seeding_rate"
    )
  ), ~ DescTools::Winsorize(., quantile(
    ., probs = c(0.1, 0.8), na.rm = FALSE
  ))) %>%
  ungroup() %>%
  dplyr::select(unique_plot_id:year, wave, everything()) %>%
  sjlabelled::copy_labels(main_data_tbl) %>%
  sjlabelled::var_labels(
    chirps_rainfall = "CHIRPS v2.0 [Funk et al. (2015)]",
    cmap_rainfall = "CMAP Standard [Xie and Arkin (1997)]",
    cmorph_rainfall = "CMORPH [Joyce et al. (2004)]",
    cpc_rainfall = "CPC-Global[Chen et al. (2008)]",
    cru_ts_rainfall = "CRU TS v4.06[Harris et al. (2020)]",
    era5_rainfall = "ERA5 [Hersbach et al. (2020)]",
    fldas_rainfall = "FLDAS [McNally et al. (2017)]",
    gpcp_rainfall = "GPCP v2.3 [Adler et al. (2018)]",
    jra55_rainfall = "JRA-55 [Kobayashi et al. (2015)]",
    merra2_rainfall = "MERRA-2 [Gelaro et al. (2017)]",
    mswep_rainfall = "MSWEP v2.8 [Beck et al. (2019)]",
    ncep_doe_rainfall = "NCEP/DOE R2 [Kanamitsu et al. (2002)]",
    persiann_rainfall = "PERSIANN-CDR [Ashouri et al. (2015)]",
    precl_rainfall = "PREC/L [Chen et al. (2002)]",
    terraclimate_rainfall = "TerraClimate [Abatzoglou et al. (2018)]",
    unique_plot_id = "Unique plot identifier",
    ea_id = "Unique Enumerator area identifier",
    wave = "Panel Wave",
    plot_area = "Area planted (hectares)",
    nitrogen_per_hectare = "Nitrogen applied (kilograms per hectare)",
    phosphorus_per_hectare = "Phosphorus per hectare (kilograms)",
    seeding_rate = "Seeding rate (kilograms per hectare)",
    maize_yield = "Maize yield (tons per hectare)",
    household_ae = "Household adult equivalency units (OECD2)",
    male_plot_manager = "Male plot manager (1/0)",
    age_plot_manager = "Age of plot manager (years)",
    organic_fertilizer = "Organic fertilizer (1/0)",
    farmer_purchased_seed = "Farmer purchased seed (1/0)",
    mechanization = "Mechanization (1/0)",
    animal_traction = "Animal traction use (1/0)",
    irrigation = "Irrigation (1/0)",
    under_extension = "Under extension (1/0)",
    twi = "Topographic wetness index (units)",
    slope = "Slope (percent)",
    elevation = "Plot elevation (meters)",
    no_other_crop = "No other crop planted (1/0)",
    one_other_crop = "One other crops planted (1/0)",
    two_other_crops = "Two other crops planted (1/0)",
    three_or_more_other_crops = "Three or more other crops planted (1/0)",
    legume = "Legume grown on plot (1/0)",
    document_for_plot = "Document for plot (1/0)",
    soil_erosion_measure = "Soil erosion measure (1/0)",
    number_of_tillage = "Number of tillage",
    predominant_soil_type = "Predominant soil type",
    percived_soil_quality = "Percived soil quality",
    agro_chemical_use = "Agro chemical use (1/0)",
    year = "Year",
    saq01 = "Region",
    household_id = "Household identifier",
    lat_dd_mod = "Latitude (Modified)",
    lon_dd_mod = "Longitude (Modified)",
    maize_yield_winsor = "Maize yield (kilograms per hectare)",
    nitrogen_winsor = "Nitrogen applied (kilograms per hectare)",
    phosphorus_winsor = "Phosphorus per hectare (kilograms)",
    seeding_rate_winsor = "Seeding rate (kilograms per hectare)"
  )

#----------------------------------------------------------------------------------------------------

gtsummary::tbl_summary(
  main_data_with_rainfall,
  include = -c(unique_plot_id, ea_id, household_id),
  by = year,
  type = list(
    twi = "continuous",
    no_other_crop = "dichotomous",
    one_other_crop = "dichotomous",
    two_other_crops = "dichotomous",
    three_or_more_other_crops = "dichotomous",
    legume = "dichotomous",
    document_for_plot = "dichotomous",
    soil_erosion_measure = "dichotomous",
    number_of_tillage = "categorical"
  ),
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n}({p}%)"
  ),
  missing = "no"
) %>%
  add_overall() %>%
  add_p(test.args = all_tests("fisher.test") ~ list(simulate.p.value = T)) %>%
  add_significance_stars(thresholds = c(0.01, 0.05, 0.10))

# save the data
write_csv(
  main_data_with_rainfall,
  here::here("data", "cleaned_data", "main_data_with_rainfall.csv")
)

# dta
write_dta(
  main_data_with_rainfall,
  here::here("data", "cleaned_data", "main_data_with_rainfall.dta")
)
