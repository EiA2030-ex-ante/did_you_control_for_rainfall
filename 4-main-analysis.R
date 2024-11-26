## -----------------------------------------------------------------------------------------
# Script: data_cleaning.R
# Author: Bisrat

# This R script performs data analysis and modeling using LSMS-ISA Ethiopia, focusing on maize yield, nitrogen application, and rainfall data. It includes:
# - Data cleaning and preparation using the `tidyverse` and `haven` packages.
# - Variable labeling with `sjlabelled` for better readability.
# - Statistical models such as Pooled OLS, Fixed Effects (FE), and Lasso regression.
# - Visualization of key variables like maize yield and nitrogen per hectare.
# - Handling of missing data and creation of balanced panel data.
# - Construction of regression results tables and model summaries with robust standard errors.
# - Saving the results as LaTeX tables for later use in reports.
#
# Libraries used:
# - `tidyverse`, `haven`, `sjPlot`, `ggplot2`, `gtsummary`, `plm`, `glmnet`, `margins`, and others.
# - Data is read, cleaned, and analyzed using multiple models for regression and visualization.
#
# Results from the models are stored in data frames and visualized for better understanding.
# The code also includes various plots saved for use in the paper
## -----------------------------------------------------------------------------------------


knitr::opts_chunk$set(
  message = FALSE,
  echo = FALSE,
  warning = FALSE,
  tidy = TRUE,
  tidy.opts = list(width.cutoff = 50),
  out.width = "100%",
  fig.width = 7,
  fig.asp = 0.75
)

options(width = 100)

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
  webr,
  gtools,
  plm,
  lmtest,
  sandwich,
  kableExtra,
  lfe,
  glmnet,
  plotmo,
  modelsummary,
  tidyverse,
  margins
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


## -----------------------------------------------------------------------------------------
cleaned_data_path <- here("data", "cleaned_data")

main_data_0 <- as_factor(read_dta(paste0(
  cleaned_data_path, "/main_data_with_rainfall.dta"
)), only_labelled = TRUE)

main_data <- main_data_0 %>%
  mutate(year = as.factor(year)) %>%
  sjlabelled::var_labels(year = "Year") %>%
  filter(nitrogen_per_hectare > 0) %>%
  filter(saq01 != "TIGRAY" &
           saq01 != "AFAR" & saq01 != "SOMALI" & saq01 != "GAMBELLA") %>%
  # refactor the region variable
  mutate(saq01 = droplevels(saq01)) %>%
  mutate(predominant_soil_type = ifelse(predominant_soil_type == "", NA, predominant_soil_type)) %>%
  mutate(
    male_plot_manager = factor(male_plot_manager),
    organic_fertilizer = factor(organic_fertilizer),
    farmer_purchased_seed = factor(farmer_purchased_seed),
    irrigation = factor(irrigation),
    under_extension = factor(under_extension),
    agro_chemical_use = factor(agro_chemical_use),
    mechanization = factor(mechanization),
    no_other_crop = factor(no_other_crop),
    one_other_crop = factor(one_other_crop),
    two_other_crops = factor(two_other_crops),
    three_or_more_other_crops = factor(three_or_more_other_crops),
    legume = factor(legume),
    document_for_plot = factor(document_for_plot),
    soil_erosion_measure = factor(soil_erosion_measure),
    number_of_tillage = factor(number_of_tillage),
    year = factor(year)
  ) %>%
  sjlabelled::copy_labels(main_data_0)


## -----------------------------------------------------------------------------------------
pdata <- pdata.frame(main_data, index = c("unique_plot_id", "year")) %>%
  sjlabelled::copy_labels(main_data)
# Create a balanced panel by removing individuals with missing periods
# balanced_pdata <- make.pbalanced(main_data, balance.type = "shared.individuals", index = # c("unique_plot_id", "year"))%>%
#  mutate(log_maize_yield = log(maize_yield))%>%
#   filter(nitrogen_per_hectare > 0)


## -----------------------------------------------------------------------------------------
plt_maize_yield <- ggstatsplot::ggbetweenstats(
  data = main_data,
  x = year,
  y = maize_yield,
  type = "parametric",
  messages = FALSE,
  ylab = "Maize yield (Kilograms per hectare)",
  title = "Maize yield by year",
  package = "wesanderson",
  palette = "Cavalcanti1"
) +
  theme(plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8))
plt_nitrogen_per_hectare <- ggstatsplot::ggbetweenstats(
  data = main_data,
  x = year,
  y = nitrogen_per_hectare,
  type = "parametric",
  messages = FALSE,
  ylab = "Nitrogen (Kilograms per hectare )",
  title = "Nitrogen per hectare by year",
  package = "wesanderson",
  palette = "Cavalcanti1"
) +
  theme(plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8))

# combine the plots into a single plot using patchwork
interest_vars_mean <- plt_maize_yield + plt_nitrogen_per_hectare
interest_vars_mean
ggsave(
  here::here("reports", "interest_vars_mean.png"),
  interest_vars_mean,
  width = 12,
  height = 6,
  units = "in",
  dpi = 600
)

plt_maize_yield_median <- ggstatsplot::ggbetweenstats(
  data = main_data,
  x = year,
  y = maize_yield,
  type = "np",
  messages = FALSE,
  ylab = "Maize yield (Kilograms per hectare)",
  title = "Maize yield by year",
  package = "wesanderson",
  palette = "Cavalcanti1",
  summary.fun = "median"
) +
  theme(plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8))
plt_nitrogen_per_hectare_median <- ggstatsplot::ggbetweenstats(
  data = main_data,
  x = year,
  y = nitrogen_per_hectare,
  type = "np",
  messages = FALSE,
  ylab = "Nitrogen (Kilograms per hectare )",
  title = "Nitrogen per hectare by year",
  package = "wesanderson",
  palette = "Cavalcanti1",
  summary.fun = "median"
) +
  theme(plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8))


interest_vars_median <- plt_maize_yield_median + plt_nitrogen_per_hectare_median
interest_vars_median
ggsave(
  here::here("reports", "interest_vars_median.png"),
  interest_vars_median,
  width = 10,
  height = 5,
  units = "in",
  dpi = 600
)


## -----------------------------------------------------------------------------------------
rainfall_data_database <- read_csv("../utilities/rainfall_data_database.csv") %>%
  select(-Introduction) %>%
  separate(Link, into = c("link_1", "link_2"), sep = "\\]") %>%
  mutate(link_1 = str_remove_all(link_1, "\\[")) %>%
  separate(Reference,
           into = c("reference_1", "reference_2"),
           sep = "\\]") %>%
  mutate(reference_1 = str_remove_all(reference_1, "\\[")) %>%
  mutate(Reference = paste0("\\href{", reference_2, "}{", reference_1, "}")) %>%
  mutate(Link = paste0("\\href{", link_2, "}{", link_1, "}")) %>%
  select(-link_1, -link_2, -reference_1, -reference_2)

rain_db_tbl <- rainfall_data_database %>%
  kableExtra::kbl(
    linesep = "",
    escape = T,
    booktabs = TRUE,
    longtable = TRUE,
    caption = "Pooled OLS-Model-2",
    align = "lllllllllllllll",
    format = "pipe"
  ) %>%
  kableExtra::kable_styling(latex_options = c("repeat_header", "striped"),
                            font_size = 5) %>%
  landscape()

rain_db_tbl <- gsub("\\\\textbackslash\\{\\}href", "\\\\href", rain_db_tbl)
rain_db_tbl <- gsub("\\\\\\{", "\\{", rain_db_tbl)
rain_db_tbl <- gsub("\\\\\\}", "\\}", rain_db_tbl)




## -----------------------------------------------------------------------------------------
knitr::include_graphics(here::here("reports", "correlation_matrix.png"))


## -----------------------------------------------------------------------------------------
summary_table <- gtsummary::tbl_summary(
  main_data,
  include = -c(unique_plot_id, ea_id, household_id, wave),
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
    under_extension = "dichotomous",
    irrigation = "dichotomous",
    agro_chemical_use = "dichotomous",
    mechanization = "dichotomous",
    farmer_purchased_seed = "dichotomous",
    male_plot_manager = "dichotomous",
    animal_traction = "dichotomous",
    organic_fertilizer = "dichotomous",
    number_of_tillage = "categorical"
  ),
  value = list(
    no_other_crop ~ 1,
    one_other_crop ~ 1,
    two_other_crops ~ 1,
    three_or_more_other_crops ~ 1,
    legume ~ 1,
    document_for_plot ~ 1,
    soil_erosion_measure ~ 1,
    under_extension ~ 1,
    irrigation ~ 1,
    agro_chemical_use ~ 1,
    mechanization ~ 1,
    farmer_purchased_seed ~ 1,
    male_plot_manager ~ 1,
    animal_traction ~ 1,
    organic_fertilizer ~ 1
  ),
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n}({p}%)",
    all_dichotomous() ~ "{n}({p}%)"
  ),
  missing = "no"
) %>%
  add_overall() %>%
  add_difference(all_continuous() ~ "smd") %>%
  modify_column_hide(columns = c(p.value, ci)) %>%
  modify_table_body( ~ .x %>% select(-p.value, -std.error)) %>%
  modify_footnote(estimate = NA) %>%
  add_p(test.args = all_tests("t.test") ~ list(simulate.p.value = T)) %>%
  add_significance_stars(thresholds = c(0.01, 0.05, 0.10)) %>%
  modify_header(all_stat_cols() ~ "N = {n}") %>%
  as_kable_extra(
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    caption = "Descriptive statistics for key study variables",
    align = "lrrrrr"
  ) %>%
  add_header_above(c(
    " " = 1,
    "Pooled" = 1,
    "2018" = 1,
    "2021" = 1
  )) %>%
  kable_styling(
    latex_options = c("scale_down", "repeat_header", "hold_position", "striped"),
    font_size = 8,
    stripe_color = "gray!3"
  ) %>%
  row_spec(63, hline_after = TRUE)
summary_table <- gsub("midrule\\*", "midrule", summary_table)
summary_table


## -----------------------------------------------------------------------------------------
# Define the rainfall columns and region variable
rainfall_columns <- pdata %>%
  select(ends_with("rainfall")) %>%
  names()

region_var <- "saq01"

# Define the list of all independent variables excluding the dependent variable and the rainfall columns
model_0 <- c("year")

model_1 <- c(
  "plot_area",
  "seeding_rate",
  "household_ae",
  "male_plot_manager",
  "age_plot_manager",
  "organic_fertilizer",
  "farmer_purchased_seed",
  "irrigation",
  "under_extension",
  "agro_chemical_use",
  "mechanization",
  "twi",
  "no_other_crop",
  "one_other_crop",
  "two_other_crops",
  "legume",
  "document_for_plot",
  "soil_erosion_measure",
  "number_of_tillage",
  "year"
)

# fit lasso for variable selection

X <- makeX(pdata[, model_1], n = nrow(pdata), p = length(model_1))
Y <- pdata$maize_yield
lasso_model <- glmnet::cv.glmnet(
  x = X,
  y = Y,
  alpha = 1,
  nfolds = 20
)
plot(lasso_model)
lasso_fit <- glmnet::glmnet(
  x = X,
  y = Y,
  alpha = 01,
  lambda = lasso_model$lambda.min
)


lasso_coef <- coef(lasso_fit, s = "lambda.min")
model_2 <- rownames(lasso_coef)[which(lasso_coef != 0)]
model_4 <- model_2 <- c(
  "plot_area",
  "seeding_rate",
  "household_ae",
  "male_plot_manager",
  "age_plot_manager",
  "organic_fertilizer",
  "farmer_purchased_seed",
  "irrigation",
  "under_extension",
  "agro_chemical_use",
  "mechanization",
  "twi",
  "no_other_crop",
  "one_other_crop",
  "two_other_crops",
  "legume",
  "document_for_plot",
  "soil_erosion_measure",
  "number_of_tillage",
  "year"
)


## -----------------------------------------------------------------------------------------
# Create empty lists to store the results
regression_results_pooled <- list()
regression_results_fe <- list()
regression_results_felm <- list()
# Loop over each rainfall column
# remove na
cleaned_data <- na.omit(main_data)

write_dta(cleaned_data,
          here::here("data", "cleaned_data", "cleaned_data_stata.dta"))
for (rainfall_col in rainfall_columns) {
  #rainfall_col <- "chirps_rainfall"
  
  # Loop over each independent variable set (model_0, model_1, model_2)
  for (model_name in c("model_0", "model_1", "model_2", "model_4")) {
    # model_name <- "model_1"
    # Get the variables associated with the model_name
    independent_var <- get(model_name)
    
    # Construct the formula dynamically by including the current independent variables
    formula_1 <- as.formula(
      paste(
        "maize_yield ~ nitrogen_per_hectare + I(nitrogen_per_hectare^2) +",
        paste(independent_var, collapse = " + "),
        "+",
        rainfall_col
      )
    )
    
    formula_2 <- as.formula(
      paste(
        "maize_yield ~ nitrogen_per_hectare + I(nitrogen_per_hectare^2) +",
        paste(independent_var, collapse = " + "),
        "+",
        rainfall_col,
        "|",
        "factor(unique_plot_id)"
      )
    )
    
    formula_3 <- as.formula(
      paste(
        "maize_yield ~ nitrogen_per_hectare + I(nitrogen_per_hectare^2) +",
        paste(independent_var, collapse = " + ")
      )
    )
    
    if (model_name == "model_4") {
      formula_1 <- formula_2 <- formula_3
    }
    
    ### Pooled OLS model with robust standard errors
    ols_model <- lm(formula_1, data = cleaned_data)
    # Compute robust standard errors
    ols_robust_se <- broom::tidy(ols_model, se.type = "robust")
    
    # Get confidence intervals for OLS model
    ols_ci <- confint(ols_model, level = 0.95)
    
    # Get marginal effects
    ols_marginal_effects <- margins::margins(ols_model)
    
    ### Fixed Effects model with robust standard errors
    fe_model <- plm(formula_1, data = pdata, model = "within")
    # Compute robust standard errors for the FE model
    fe_robust_se <- broom::tidy(fe_model, se.type = "robust")
    
    # Get confidence intervals for Fixed Effects model
    fe_ci <- confint(fe_model, level = 0.95)
    
    # Get marginal effects
    
    #beta_1 <- fe_model$coefficients["nitrogen_per_hectare"]
    #beta_2 <- fe_model$coefficients["I(nitrogen_per_hectare^2)"]
    #
    #pdata2 <- pdata
    #pdata2$marginal_effect <- beta_1 + 2 * beta_2 * pdata2$nitrogen_per_hectare
    #
    #fe_marginal_effects <- mean(pdata2$marginal_effect)
    
    felm_model <- felm(formula_2, data = pdata)
    
    felm_robust_se <- broom::tidy(felm_model, se.type = "robust")
    
    felm_ci <- confint(felm_model, level = 0.95)
    
    #beta_1 <- as.data.frame(t(felm_model$coefficients))["nitrogen_per_hectare"][,1]#
    #beta_2 <- as.data.frame(t(felm_model$coefficients))["I(nitrogen_per_hectare^2)"]#[,1]
    #
    #pdata2$flm_marginal_effect <- beta_1 + 2 * beta_2 * pdat#a2$nitrogen_per_hectare
    #felm_marginal_effects <- mean(pdata2$flm_marginal_effect)
    
    
    # Ensure the list is initialized before assignment
    if (is.null(regression_results_pooled[[rainfall_col]])) {
      regression_results_pooled[[rainfall_col]] <- list()
    }
    
    if (is.null(regression_results_fe[[rainfall_col]])) {
      regression_results_fe[[rainfall_col]] <- list()
    }
    
    if (is.null(regression_results_felm[[rainfall_col]])) {
      regression_results_felm[[rainfall_col]] <- list()
    }
    
    # Store the results using the model name (e.g., model_0, model_1, model_2)
    regression_results_pooled[[rainfall_col]][[model_name]] <- list(
      summary = ols_model,
      confidence_intervals = ols_ci,
      se = ols_robust_se,
      marginal_effects = ols_marginal_effects
    )
    
    regression_results_fe[[rainfall_col]][[model_name]] <- list(summary = fe_model,
                                                                confidence_intervals = fe_ci,
                                                                se = fe_robust_se)
    
    regression_results_felm[[rainfall_col]][[model_name]] <- list(summary = felm_model,
                                                                  confidence_intervals = felm_ci,
                                                                  se = felm_robust_se)
  }
}


## -----------------------------------------------------------------------------------------

# Collect the nitrogen coefficients, standard errors, p-values, and confidence intervals in a single data frame

results_df_0 <- data.frame()

get_significance_stars <- function(p_value) {
  if (p_value < 0.001) {
    return("***")
  } else if (p_value < 0.01) {
    return("**")
  } else if (p_value < 0.05) {
    return("*")
  } else if (p_value < 0.1) {
    return(".")
  } else {
    return("")
  }
}

for (rainfall_col in rainfall_columns) {
  # rainfall_col <- "chirps_rainfall"
  for (model_name in c("model_0", "model_1", "model_2", "model_4")) {
    # model_name <- "model_1"
    pooled_result <- regression_results_pooled[[rainfall_col]][[model_name]]
    print(rainfall_col)
    marginal_effects <- summary(pooled_result$marginal_effects) %>%
      filter(factor == "nitrogen_per_hectare")
    nitrogen_row <- pooled_result$se %>%
      filter(term == "nitrogen_per_hectare")
    
    if (nrow(nitrogen_row) > 0) {
      nitrogen_coef <- nitrogen_row$estimate
      nitrogen_se <- nitrogen_row$std.error
      nitrogen_p <- nitrogen_row$p.value
      
      nitrogen_ci_lower <- pooled_result$confidence_intervals["nitrogen_per_hectare", "2.5 %"]
      nitrogen_ci_upper <- pooled_result$confidence_intervals["nitrogen_per_hectare", "97.5 %"]
      
      me <- marginal_effects$AME
      me_se <- marginal_effects$SE
      me_p <- marginal_effects$p
      me_ci_lower <- marginal_effects$lower
      me_ci_upper <- marginal_effects$upper
      
      significance <- get_significance_stars(nitrogen_p)
      
      row <- data.frame(
        rainfall_column = rainfall_col,
        model = model_name,
        model_type = "pooled",
        coefficient = nitrogen_coef,
        standard_error = nitrogen_se,
        lower_bound_ci = nitrogen_ci_lower,
        upper_bound_ci = nitrogen_ci_upper,
        significance = significance,
        marginal_effects = me,
        marginal_effect_se = me_se,
        marginal_effect_lower = me_ci_lower,
        marginal_effect_upper = me_ci_upper,
        marginal_effect_significance = get_significance_stars(me_p)
      )
      
      results_df_0 <- rbind(results_df_0, row)
    }
    
    fe_result <- regression_results_felm[[rainfall_col]][[model_name]]
    
    nitrogen_row <- fe_result$se %>%
      filter(term == "nitrogen_per_hectare")
    
    if (nrow(nitrogen_row) > 0) {
      nitrogen_coef <- nitrogen_row$estimate
      nitrogen_se <- nitrogen_row$std.error
      nitrogen_p <- nitrogen_row$p.value
      
      nitrogen_ci_lower <- fe_result$confidence_intervals["nitrogen_per_hectare", "2.5 %"]
      nitrogen_ci_upper <- fe_result$confidence_intervals["nitrogen_per_hectare", "97.5 %"]
      
      significance <- get_significance_stars(nitrogen_p)
      
      row <- data.frame(
        rainfall_column = rainfall_col,
        model = model_name,
        model_type = "fixed",
        coefficient = nitrogen_coef,
        standard_error = nitrogen_se,
        lower_bound_ci = nitrogen_ci_lower,
        upper_bound_ci = nitrogen_ci_upper,
        significance = significance,
        marginal_effects = NA,
        marginal_effect_se = NA,
        marginal_effect_lower = NA,
        marginal_effect_upper = NA,
        marginal_effect_significance = NA
      )
      
      results_df_0 <- rbind(results_df_0, row)
    }
  }
}


results_df <- results_df_0 %>%
  mutate(
    rainfall_column_2 = case_when(
      rainfall_column == "chirps_rainfall" ~ "CHIRPS v2.0",
      rainfall_column == "cmap_rainfall" ~ "CMAP Standard",
      rainfall_column == "cmorph_rainfall" ~ "CMORPH",
      rainfall_column == "cpc_rainfall" ~ "CPC-Global",
      rainfall_column == "cru_ts_rainfall" ~ "CRU TS v4.06",
      rainfall_column == "era5_rainfall" ~ "ERA5",
      rainfall_column == "fldas_rainfall" ~ "FLDAS",
      rainfall_column == "gpcp_rainfall" ~ "GPCP v2.3",
      rainfall_column == "jra55_rainfall" ~ "JRA-55",
      rainfall_column == "merra2_rainfall" ~ "MERRA-2",
      rainfall_column == "mswep_rainfall" ~ "MSWEP v2.8",
      rainfall_column == "ncep_doe_rainfall" ~ "NCEP/DOE R2",
      rainfall_column == "persiann_rainfall" ~ "PERSIANN-CDR",
      rainfall_column == "precl_rainfall" ~ "PREC/L",
      rainfall_column == "terraclimate_rainfall" ~ "TerraClimate"
    )
  ) %>%
  mutate(
    model_2 = case_when(
      model == "model_0" ~ "Maize_yield = Nitrogen + Nitrogen^2 + Year + rainfall",
      model == "model_1" ~ "Maize_yield = Nitrogen + Nitrogen^2 + Year + Controls + rainfall",
      model == "model_4" ~ "Maize_yield = Nitrogen + Nitrogen^2 + Controls"
    )
  )





## -----------------------------------------------------------------------------------------
# plot marginal effects
me <- results_df %>% filter(model != "model_2" &
                              model_type == "pooled") %>%
  filter(model != "model_4") %>%
  mutate(
    model_label = case_when(
      model == "model_0" ~ "[A] With-controls",
      model == "model_1" ~ "[B] Without-controls"
    )
  )

latex_font <- "Frutiger"
my_font <- "Frutiger"

facet_labels_pooled <- c("model_0" = "Y[ijt] == beta[0] + beta[1]*N[ijt] + beta[2]*N[ijt]^2 + beta[r]*R[ijt] + epsilon[ijt]", "model_1" = "Y[ijt] == beta[0] + beta[1]*N[ijt] + beta[2]*N[ijt]^2 + sum(beta[k]*X[kijt], k==3, K) + beta[r]*R[ijt]  + epsilon[ijt]")

facet_labels_pooled_2 <- c("model_0" = "[A] With-controls", "model_1" = "[B] Without-controls")
plot_me <- ggplot(me, aes(x = rainfall_column_2, y = marginal_effects)) +
  geom_point(aes(color = rainfall_column_2),
             size = 3,
             shape = 15) +
  geom_errorbar(
    aes(ymin = marginal_effect_lower, ymax = marginal_effect_upper),
    width = 0.2,
    color = "black"
  ) +
  geom_text(
    aes(label = paste0(round(marginal_effects, 1), significance)),
    vjust = -0.2,
    hjust = -0.5,
    angle = 0,
    size = 5,
    family = my_font
  ) +
  #geom_hline(yintercept = mean(results_pooled$coefficient), linetype = "dashed", color = "#09447D") +
  labs(title = "", x = "Data set", y = "APE") +
  coord_flip() +
  scale_color_manual(values = c(GrandBudapest1, Cavalcanti1, Darjeeling1, Moonrise1)) +
  ggthemes::theme_pander(base_family = my_font, base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    legend.position = "none",
    panel.grid.minor  = element_blank(),
    strip.text = element_text(
      family = latex_font,
      size = 14,
      face = "bold"
    )
  ) +
  facet_wrap( ~ model_label, ncol = 2)

plot_me

ggsave(
  here::here("reports", "plot_me.png"),
  plot_me,
  width = 6,
  height = 8,
  units = "in",
  dpi = 400
)


## -----------------------------------------------------------------------------------------


# Create separate data frames for pooled and fixed effects models
results_pooled <- results_df %>% filter(model_type == "pooled" &
                                          model != "model_2")
results_fixed <- results_df %>% filter(model_type == "fixed" &
                                         model != "model_2")
facet_labels_pooled <- c("model_0" = "Y[ijt] == beta[0] + beta[1]*N[ijt] + beta[2]*N[ijt]^2 + beta[r]*R[ijt] + epsilon[ijt]", "model_1" = "Y[ijt] == beta[0] + beta[1]*N[ijt] + beta[2]*N[ijt]^2 + sum(beta[k]*X[kijt], k==3, K) + beta[r]*R[ijt]  + epsilon[ijt]")

facet_labels_fixed <- c("model_0" = "Y[ijt] == beta[0] + beta[1]*N[ijt] + beta[2]*N[ijt]^2 + beta[r]*R[ijt] + alpha[i] + epsilon[ijt]", "model_1" = "Y[ijt] == beta[0] + beta[1]*N[ijt] + beta[2]*N[ijt]^2 + sum(beta[k]*X[kijt], k==3, K) + beta[r]*R[ijt] + alpha[i] + epsilon[ijt]")

latex_font <- "Frutiger"
my_font <- "Frutiger"
plot_pooled <- ggplot(results_pooled, aes(x = rainfall_column_2, y = coefficient)) +
  geom_point(aes(color = rainfall_column_2),
             size = 3,
             shape = 15) +
  geom_errorbar(
    aes(ymin = lower_bound_ci, ymax = upper_bound_ci),
    width = 0.2,
    color = "black"
  ) +
  geom_text(
    aes(label = paste0(round(coefficient, 2), significance)),
    vjust = -1,
    angle = 90,
    size = 4,
    family = my_font
  ) +
  #geom_hline(yintercept = mean(results_pooled$coefficient), linetype = "dashed", color = "#09447D") +
  labs(title = "Pooled OLS Model", x = "Data set", y = "Coefficient") +
  scale_color_manual(values = c(GrandBudapest1, Cavalcanti1, Darjeeling1, Moonrise1)) +
  ggthemes::theme_pander(base_family = my_font, base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none",
    panel.grid.minor  = element_blank(),
    strip.text = element_text(
      family = latex_font,
      size = 14,
      face = "bold"
    )
  ) +
  facet_wrap( ~ model,
              ncol = 1,
              labeller = as_labeller(facet_labels_pooled, label_parsed))

plot_pooled
# Plot for fixed effects models (faceted by model)
plot_fixed <- ggplot(results_fixed, aes(x = rainfall_column_2, y = coefficient)) +
  geom_point(size = 4,
             aes(color = rainfall_column_2),
             shape = 15) +
  geom_errorbar(
    aes(ymin = lower_bound_ci, ymax = upper_bound_ci),
    width = 0.2,
    color = "black"
  ) +
  geom_text(
    aes(label = paste0(round(coefficient, 2), significance)),
    vjust = -1,
    angle = 90,
    size = 4,
    family = my_font
  ) +
  #geom_hline(yintercept = mean(results_fixed$coefficient), linetype = "dashed", color = "#09447D") +
  labs(title = "Fixed Effects Model", x = "Data sets", y = "Coefficient") +
  scale_color_manual(values = c(GrandBudapest1, Cavalcanti1, Darjeeling1, Moonrise1)) +
  ggthemes::theme_pander(base_family = my_font, base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none",
    panel.grid.minor  = element_blank()
  ) +
  facet_wrap( ~ model,
              ncol = 1,
              labeller = as_labeller(facet_labels_fixed, label_parsed))

# Display the plots

ggsave(
  here::here("reports", "plot_pooled.png"),
  plot_pooled,
  width = 10,
  height = 5,
  units = "in",
  dpi = 600
)
plot_fixed
ggsave(
  here::here("reports", "plot_fixed.png"),
  plot_fixed,
  width = 10,
  height = 5,
  units = "in",
  dpi = 600
)


## -----------------------------------------------------------------------------------------
# from the list filter model_0 and sumary
regression_results_pooled_0 <- lapply(regression_results_pooled, function(x)
  x$model_4$summary)
names(regression_results_pooled_0) <- c(
  "Pooled OLS",
  "CMAP Standard",
  "CMORPH",
  "CPC-Global",
  "CRU TS v4.06",
  "ERA5",
  "FLDAS",
  "GPCP v2.3",
  "JRA-55",
  "MERRA-2",
  "MSWEP v2.8",
  "NCEP/DOE R2",
  "PERSIANN-CDR",
  "PREC/L",
  "TerraClimate"
)

regression_results_fe_0 <- lapply(regression_results_fe, function(x)
  x$model_4$summary)
names(regression_results_fe_0) <- c(
  "FE",
  "CMAP Standard",
  "CMORPH",
  "CPC-Global",
  "CRU TS v4.06",
  "ERA5",
  "FLDAS",
  "GPCP v2.3",
  "JRA-55",
  "MERRA-2",
  "MSWEP v2.8",
  "NCEP/DOE R2",
  "PERSIANN-CDR",
  "PREC/L",
  "TerraClimate"
)


## -----------------------------------------------------------------------------------------
ols_model_0_0 <- modelsummary::msummary(
  c(regression_results_pooled_0[1], regression_results_fe_0[1]),
  stars = TRUE,
  statistic = "({std.error})",
  ci = TRUE,
  digits = 3,
  vcov = "robust",
  coef_omit = '(Intercept)',
  coef_rename = c(
    "nitrogen_per_hectare" = "Nitrogen (Kg/ha)",
    "I(nitrogen_per_hectare^2)" = "Nitrogen^2 (Kg/ha)",
    "chirps_rainfall" = "CHIRPS v2.0",
    "cmap_rainfall" = "CMAP Standard",
    "cmorph_rainfall" = "CMORPH",
    "cpc_rainfall" = "CPC-Global",
    "cru_ts_rainfall" = "CRU TS v4.06",
    "era5_rainfall" = "ERA5",
    "fldas_rainfall" = "FLDAS",
    "gpcp_rainfall" = "GPCP v2.3",
    "jra55_rainfall" = "JRA-55",
    "merra2_rainfall" = "MERRA-2",
    "mswep_rainfall" = "MSWEP v2.8",
    'ncep_doe_rainfall' = "NCEP/DOE R2",
    "persiann_rainfall" = "PERSIANN-CDR",
    "precl_rainfall" = "PREC/L",
    "terraclimate_rainfall" = "TerraClimate",
    "plot_area" = "Plot area (ha)",
    "seeding_rate" = "Seeding rate (Kg/ha)",
    "household_ae" = "Household AE",
    "male_plot_manager1" = "Male plot manager",
    "age_plot_manager" = "Age of plot manager",
    "organic_fertilizer1" = "Organic fertilizer",
    "farmer_purchased_seed1" = "Farmer purchased seed",
    "irrigation1" = "Irrigation",
    "under_extension1" = "Under extension",
    "agro_chemical_use1" = "Agro chemical use",
    "mechanization1" = "Mechanization (1/0)",
    "twi" = "Topographic wetness index",
    "no_other_crop1" = "No other crop",
    "one_other_crop1" = "One other crop",
    "two_other_crops1" = "Two other crops",
    "legume1" = "Cultivated Legumes",
    "document_for_plot1" = "Document for plot=1",
    "soil_erosion_measure1" = "Soil erosion measure=1",
    "number_of_tillageOnce" = "Number of tillage (Once)",
    "number_of_tillageTwice" = "Number of tillage (Twice)",
    "year2021" = "Year=2021"
  )
)

ols_model_0 <- ols_model_0_0@table_dataframe %>%
  kableExtra::kbl(
    "latex",
    linesep = "",
    escape = F,
    booktabs = TRUE,
    longtable = TRUE,
    caption = "Pooled OLS-Model-1",
    align = "lrrrrrrrrrrrrrrr"
  ) %>%
  kableExtra::kable_styling(
    latex_options = c("repeat_header", "striped"),
    font_size = 4,
    stripe_color = "gray!3"
  ) %>%
  row_spec(46, hline_after = T) %>%
  row_spec(4, hline_after = T) %>%
  row_spec(1, bold = T, color = "#D35230") %>%
  row_spec(2, bold = T, color = "#D35230") %>%
  landscape()

ols_model_0

writeLines(ols_model_0,
           here::here("tables", "ols_model_0_without_rainfall.tex"))



## -----------------------------------------------------------------------------------------
# from the list filter model_0 and sumary
regression_results_pooled_1 <- lapply(regression_results_pooled, function(x)
  x$model_0$summary)
names(regression_results_pooled_1) <- c(
  "CHIRPS v2.0",
  "CMAP Standard",
  "CMORPH",
  "CPC-Global",
  "CRU TS v4.06",
  "ERA5",
  "FLDAS",
  "GPCP v2.3",
  "JRA-55",
  "MERRA-2",
  "MSWEP v2.8",
  "NCEP/DOE R2",
  "PERSIANN-CDR",
  "PREC/L",
  "TerraClimate"
)



## -----------------------------------------------------------------------------------------
ols_model_1_1 <- modelsummary::msummary(
  regression_results_pooled_1,
  stars = TRUE,
  statistic = "({std.error})",
  ci = TRUE,
  digits = 3,
  vcov = "robust",
  coef_omit = '(Intercept)',
  coef_rename = c(
    "nitrogen_per_hectare" = "Nitrogen (Kg/ha)",
    "I(nitrogen_per_hectare^2)" = "Nitrogen^2 (Kg/ha)",
    "chirps_rainfall" = "CHIRPS v2.0",
    "cmap_rainfall" = "CMAP Standard",
    "cmorph_rainfall" = "CMORPH",
    "cpc_rainfall" = "CPC-Global",
    "cru_ts_rainfall" = "CRU TS v4.06",
    "era5_rainfall" = "ERA5",
    "fldas_rainfall" = "FLDAS",
    "gpcp_rainfall" = "GPCP v2.3",
    "jra55_rainfall" = "JRA-55",
    "merra2_rainfall" = "MERRA-2",
    "mswep_rainfall" = "MSWEP v2.8",
    'ncep_doe_rainfall' = "NCEP/DOE R2",
    "persiann_rainfall" = "PERSIANN-CDR",
    "precl_rainfall" = "PREC/L",
    "terraclimate_rainfall" = "TerraClimate",
    "year2021" = "Year=2021"
  )
)

ols_model_1 <- ols_model_1_1@table_dataframe %>%
  kableExtra::kbl(
    "latex",
    linesep = "",
    escape = F,
    booktabs = TRUE,
    longtable = TRUE,
    caption = "Pooled OLS-Model-1",
    align = "lrrrrrrrrrrrrrrr"
  ) %>%
  kableExtra::kable_styling(
    latex_options = c("repeat_header", "striped"),
    font_size = 4,
    stripe_color = "gray!3"
  ) %>%
  row_spec(36, hline_after = T) %>%
  row_spec(6, hline_after = T) %>%
  row_spec(1, bold = T, color = "#D35230") %>%
  row_spec(2, bold = T, color = "#D35230") %>%
  landscape()

ols_model_1

writeLines(ols_model_1, here::here("tables", "ols_model_1.tex"))


## -----------------------------------------------------------------------------------------
# from the list filter model_1 and sumary
regression_results_pooled_2 <- lapply(regression_results_pooled, function(x)
  x$model_1$summary)
names(regression_results_pooled_2) <- c(
  "CHIRPS v2.0",
  "CMAP Standard",
  "CMORPH",
  "CPC-Global",
  "CRU TS v4.06",
  "ERA5",
  "FLDAS",
  "GPCP v2.3",
  "JRA-55",
  "MERRA-2",
  "MSWEP v2.8",
  "NCEP/DOE R2",
  "PERSIANN-CDR",
  "PREC/L",
  "TerraClimate"
)



## -----------------------------------------------------------------------------------------
ols_model_2_0 <- modelsummary::msummary(
  regression_results_pooled_2,
  stars = TRUE,
  statistic = "({std.error})",
  ci = TRUE,
  digits = 3,
  vcov = "robust",
  coef_omit = '(Intercept)',
  coef_rename = c(
    "nitrogen_per_hectare" = "Nitrogen (Kg/ha)",
    "I(nitrogen_per_hectare^2)" = "Nitrogen^2 (Kg/ha)",
    "chirps_rainfall" = "CHIRPS v2.0",
    "cmap_rainfall" = "CMAP Standard",
    "cmorph_rainfall" = "CMORPH",
    "cpc_rainfall" = "CPC-Global",
    "cru_ts_rainfall" = "CRU TS v4.06",
    "era5_rainfall" = "ERA5",
    "fldas_rainfall" = "FLDAS",
    "gpcp_rainfall" = "GPCP v2.3",
    "jra55_rainfall" = "JRA-55",
    "merra2_rainfall" = "MERRA-2",
    "mswep_rainfall" = "MSWEP v2.8",
    'ncep_doe_rainfall' = "NCEP/DOE R2",
    "persiann_rainfall" = "PERSIANN-CDR",
    "precl_rainfall" = "PREC/L",
    "terraclimate_rainfall" = "TerraClimate",
    "plot_area" = "Plot area (ha)",
    "seeding_rate" = "Seeding rate (Kg/ha)",
    "household_ae" = "Household AE",
    "male_plot_manager1" = "Male plot manager",
    "age_plot_manager" = "Age of plot manager",
    "organic_fertilizer1" = "Organic fertilizer",
    "farmer_purchased_seed1" = "Farmer purchased seed",
    "irrigation1" = "Irrigation",
    "under_extension1" = "Under extension",
    "agro_chemical_use1" = "Agro chemical use",
    "mechanization1" = "Mechanization (1/0)",
    "twi" = "Topographic wetness index",
    "no_other_crop1" = "No other crop",
    "one_other_crop1" = "One other crop",
    "two_other_crops1" = "Two other crops",
    "legume1" = "Cultivated Legumes",
    "document_for_plot1" = "Document for plot=1",
    "soil_erosion_measure1" = "Soil erosion measure=1",
    "number_of_tillageOnce" = "Number of tillage (Once)",
    "number_of_tillageTwice" = "Number of tillage (Twice)",
    "year2021" = "Year=2021"
  )
)


ols_model_2 <- ols_model_2_0@table_dataframe %>%
  kableExtra::kbl(
    linesep = "",
    escape = F,
    booktabs = TRUE,
    longtable = TRUE,
    caption = "Pooled OLS-Model-2",
    align = "lrrrrrrrrrrrrrrr"
  ) %>%
  kableExtra::kable_styling(
    latex_options = c("repeat_header", "striped"),
    font_size = 4,
    stripe_color = "gray!3"
  ) %>%
  # column_spec(width = "1cm") %>%
  row_spec(76, hline_after = T) %>%
  row_spec(4, hline_after = T) %>%
  row_spec(46, hline_after = T) %>%
  row_spec(1, bold = T, color = "#D35230") %>%
  row_spec(2, bold = T, color = "#D35230") %>%
  landscape()

ols_model_2

writeLines(ols_model_2, here::here("tables", "ols_model_2.tex"))



## -----------------------------------------------------------------------------------------
# from the list filter model_0 and sumary
regression_results_fe_0 <- lapply(regression_results_fe, function(x)
  x$model_0$summary)
names(regression_results_fe_0) <- c(
  "CHIRPS v2.0",
  "CMAP Standard",
  "CMORPH",
  "CPC-Global",
  "CRU TS v4.06",
  "ERA5",
  "FLDAS",
  "GPCP v2.3",
  "JRA-55",
  "MERRA-2",
  "MSWEP v2.8",
  "NCEP/DOE R2",
  "PERSIANN-CDR",
  "PREC/L",
  "TerraClimate"
)


## -----------------------------------------------------------------------------------------
fe_model_1_0 <- modelsummary::msummary(
  regression_results_fe_0,
  stars = TRUE,
  statistic = "({std.error})",
  ci = TRUE,
  digits = 3,
  vcov = "robust",
  coef_omit = '(Intercept)',
  coef_rename = c(
    "nitrogen_per_hectare" = "Nitrogen (Kg/ha)",
    "I(nitrogen_per_hectare^2)" = "Nitrogen^2 (Kg/ha)",
    "chirps_rainfall" = "CHIRPS v2.0",
    "cmap_rainfall" = "CMAP Standard",
    "cmorph_rainfall" = "CMORPH",
    "cpc_rainfall" = "CPC-Global",
    "cru_ts_rainfall" = "CRU TS v4.06",
    "era5_rainfall" = "ERA5",
    "fldas_rainfall" = "FLDAS",
    "gpcp_rainfall" = "GPCP v2.3",
    "jra55_rainfall" = "JRA-55",
    "merra2_rainfall" = "MERRA-2",
    "mswep_rainfall" = "MSWEP v2.8",
    'ncep_doe_rainfall' = "NCEP/DOE R2",
    "persiann_rainfall" = "PERSIANN-CDR",
    "precl_rainfall" = "PREC/L",
    "terraclimate_rainfall" = "TerraClimate",
    "year2021" = "Year=2021"
  )
)

fe_model_1 <- fe_model_1_0@table_dataframe %>%
  kableExtra::kbl(
    linesep = "",
    escape = F,
    booktabs = TRUE,
    longtable = TRUE,
    caption = "Fixed Effects-Model-1",
    align = "lrrrrrrrrrrrrrrr"
  ) %>%
  kableExtra::kable_styling(
    latex_options = c("repeat_header", "striped"),
    font_size = 4,
    stripe_color = "gray!3"
  ) %>%
  row_spec(36, hline_after = T) %>%
  row_spec(4, hline_after = T) %>%
  row_spec(1, bold = T, color = "#D35230") %>%
  row_spec(2, bold = T, color = "#D35230") %>%
  landscape()

fe_model_1
writeLines(fe_model_1, here::here("tables", "fe_model_1.tex"))


## -----------------------------------------------------------------------------------------
# from the list filter model_1 and sumary

regression_results_fe_1 <- lapply(regression_results_fe, function(x)
  x$model_1$summary)
names(regression_results_fe_1) <- c(
  "CHIRPS v2.0",
  "CMAP Standard",
  "CMORPH",
  "CPC-Global",
  "CRU TS v4.06",
  "ERA5",
  "FLDAS",
  "GPCP v2.3",
  "JRA-55",
  "MERRA-2",
  "MSWEP v2.8",
  "NCEP/DOE R2",
  "PERSIANN-CDR",
  "PREC/L",
  "TerraClimate"
)


## -----------------------------------------------------------------------------------------
fe_model_2_0 <- modelsummary::msummary(
  regression_results_fe_1,
  stars = TRUE,
  statistic = "({std.error})",
  ci = TRUE,
  digits = 3,
  vcov = "robust",
  coef_omit = '(Intercept)',
  coef_rename = c(
    "nitrogen_per_hectare" = "Nitrogen (Kg/ha)",
    "I(nitrogen_per_hectare^2)" = "Nitrogen^2 (Kg/ha)",
    "chirps_rainfall" = "CHIRPS v2.0",
    "cmap_rainfall" = "CMAP Standard",
    "cmorph_rainfall" = "CMORPH",
    "cpc_rainfall" = "CPC-Global",
    "cru_ts_rainfall" = "CRU TS v4.06",
    "era5_rainfall" = "ERA5",
    "fldas_rainfall" = "FLDAS",
    "gpcp_rainfall" = "GPCP v2.3",
    "jra55_rainfall" = "JRA-55",
    "merra2_rainfall" = "MERRA-2",
    "mswep_rainfall" = "MSWEP v2.8",
    'ncep_doe_rainfall' = "NCEP/DOE R2",
    "persiann_rainfall" = "PERSIANN-CDR",
    "precl_rainfall" = "PREC/L",
    "terraclimate_rainfall" = "TerraClimate",
    "plot_area" = "Plot area (ha)",
    "seeding_rate" = "Seeding rate (Kg/ha)",
    "household_ae" = "Household AE",
    "male_plot_manager1" = "Male plot manager",
    "age_plot_manager" = "Age of plot manager",
    "organic_fertilizer1" = "Organic fertilizer",
    "farmer_purchased_seed1" = "Farmer purchased seed",
    "irrigation1" = "Irrigation",
    "under_extension1" = "Under extension",
    "agro_chemical_use1" = "Agro chemical use",
    "mechanization1" = "Mechanization (1/0)",
    "twi" = "Topographic wetness index",
    "no_other_crop1" = "No other crop",
    "one_other_crop1" = "One other crop",
    "two_other_crops1" = "Two other crops",
    "legume1" = "Cultivated Legumes",
    "document_for_plot1" = "Document for plot=1",
    "soil_erosion_measure1" = "Soil erosion measure=1",
    "number_of_tillageOnce" = "Number of tillage (Once)",
    "number_of_tillageTwice" = "Number of tillage (Twice)",
    "year2021" = "Year=2021"
  )
)


fe_model_2 <- fe_model_2_0@table_dataframe %>%
  kableExtra::kbl(
    linesep = "",
    escape = F,
    booktabs = TRUE,
    longtable = TRUE,
    caption = "Fixed Effects-Model-2",
    align = "lrrrrrrrrrrrrrrr"
  ) %>%
  kableExtra::kable_styling(
    latex_options = c("repeat_header", "striped"),
    font_size = 4,
    stripe_color = "gray!3"
  ) %>%
  row_spec(74, hline_after = T) %>%
  row_spec(4, hline_after = T) %>%
  row_spec(44, hline_after = T) %>%
  row_spec(1, bold = T, color = "#D35230") %>%
  row_spec(2, bold = T, color = "#D35230") %>%
  landscape()

fe_model_2

writeLines(fe_model_2, here::here("tables", "fe_model_2.tex"))
