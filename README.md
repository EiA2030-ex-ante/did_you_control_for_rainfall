# did_you_control_for_rainfall
This repository contains the analysis code for the investigation of how the choice of rainfall dataset influences econometric model performance, particularly in the context of productivity and fertilizer response

# Abstract
The availability of spatially-explicit time-series estimates of rainfall and other weather outcomes has expanded rapidly over the past decade and a half. This proliferation of publicly-accessible rainfall data has changed how empirical analysis of farm production and productivity takes place: fortifying survey-derived data (e.g., on farm management and production outcomes) with spatial estimates of seasonal rainfall outcomes is now standard practice, much to the benefit of applied agricultural economic analysis. Yet guidance on which dataset to use, among the many available alternatives, has largely been lacking: while there have been some studies comparing rainfall data quality across alternative datasets, relative to some accuracy benchmark, there has not yet been systematic analysis of whether the choice of rainfall dataset may affect econometric model performance, or the estimation of other (non-rainfall) model parameters. Such a concern would arise from cases where any mismeasurement of rainfall is correlated with model error terms or other model covariates. To investigate this, we use panel data on plot-level maize yield outcomes in Ethiopia from 2018 and 2021, along with fifteen alternative spatio-temporal rainfall data products, including gauge-based, satellite-derived, and reanalysis datasets. We estimate yield response models, in which our primary interest is on the estimate of nitrogen use efficiency (NUE). Estimation results from alternative specifications and panel estimators indicate that, while coefficient estimates for rainfall vary considerably with alternative rainfall estimates, the coefficient estimates on nitrogen fertilizer (and consequently, our estimates of NUE) do not vary in a statistically significant way. Our results suggest that the choice of rainfall dataset does not significantly affect analytical conclusions about fertilizer response.

# Files and Directories

- **0_rainfall_data_download.R**: Script for downloading and subsetting rainfall data for Ethiopia.
- **1-rainfall-data-cleaning.R**: Script for cleaning and preprocessing the rainfall data.
- **2-lsms-data-cleaning.R**: Script for cleaning and preprocessing the LSMS (Living Standards Measurement Study) data.
- **3-visualization.R**: Script for visualizing the cleaned data, including rainfall and productivity metrics.
- **4-main-analysis.R**: Script for performing the main econometric analysis, including OLS and fixed effects models.
- **data/**: Directory containing various subdirectories for storing raw, intermediate, cleaned, and spatial data.
  - **cleaned_data/**: Contains the cleaned datasets ready for analysis.
  - **intermediate_data/**: Contains intermediate datasets generated during the cleaning process.
  - **raw_data/**: Contains the raw datasets as downloaded.
  - **spatial_data/**: Contains spatial data files such as shapefiles for Ethiopia.
- **LICENSE**: License file for the project.
- **README.md**: This file, providing an overview of the project.

## Prerequisites

You have the following R packages installed:

```r
install.packages(c("pacman", "haven", "here", "plotly", "leaflet", "ggthemes", "ggstatsplot", "sjPlot", "sjlabelled", "gtsummary", "geosphere", "ggpie", "ggthemr", "webr", "gtools", "xml2", "pRecipe", "cowplot", "data.table", "ggpubr", "grid", "raster", "scales", "reticulate", "tidyverse", "sf", "terra", "extrafont", "plm", "lmtest", "sandwich", "kableExtra", "lfe", "glmnet", "plotmo", "modelsummary", "margins"))
```

## License
This project is licensed under the Creative Commons Attribution 4.0 International License. See the LICENSE file for details.

## Acknowledgments
    - This work was made possible through the OneCGIAR Initiative on Excellence in Agronomy (INV-005431), as well as through the Guiding Acid Soil Management Investments in Africa (GAIA) project (Grant no: INV-029117), supported by the Bill & Melinda Gates Foundation (BMGF). 
    - We would like to thank all funders supporting research through contributions to the CGIAR Trust Fund: https://www.cgiar.org/funders/.
    - This project uses data from the LSMS-ISA Ethiopia and various rainfall datasets. Special thanks to the data providers and contributors.