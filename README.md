# CSISG Credit Card Dashboard Project

## Data Requirements
Place your data files in the following locations with the specified formats:

- **data/raw/**: Place your raw dataset here
  - **Format**: Excel (.xlsx) file
  - **Required columns**: 'satis', 'repur', 'Issuer', 'vn 7002 T01' (will be renamed to 'vn7002T01'), and other CSISG credit card industry variables
  - **File naming**: Use `csisg_dataset.xlsx` as the filename

- **data/**: Processed data files will be automatically generated here
  - `cleaned_data.rds`: Cleaned dataset after running data_cleaning.R
  - `processed_data.rds`: Further processed dataset after running preprocessing.R

## How to run
Run the following commands one by one:
```
source("data_cleaning.R")
```
```
source("preprocessing.R")
```
```
source("eda.R")
```
```
source("visualizations.R")
```
```
runApp("src/dashboard")
```

## Overview
This project analyzes the Customer Satisfaction Index of Singapore (CSISG) dataset for the credit card industry, focusing on OCBC Bank's performance. The goal is to identify key drivers of customer satisfaction, benchmark against competitors, and provide actionable insights through visualizations and an interactive Shiny dashboard.

## Folder Structure
- **data/raw/**: Contains the raw dataset (csisg_dataset.xlsx).
- **src/**: Contains R scripts for data cleaning, preprocessing, EDA, visualizations, and the dashboard.
- **output/**: Dynamically generated folder for plots and reports.

## Dependencies
- R packages: readxl, dplyr, ggplot2, ggthemes, shiny, ggridges, fmsb, networkD3
- Install missing packages via `install.packages(c("readxl", "dplyr", "ggplot2", "ggthemes", "shiny", "ggridges", "fmsb", "networkD3"))`

## Workflow
1. Run `src/data_cleaning.R` to load and clean the raw data, saving cleaned_data.rds.
2. Run `src/preprocessing.R` to further process the data (binning, recoding), saving processed_data.rds.
3. Run `src/eda.R` for exploratory analysis, generating summaries and reports in output/reports.
4. Run `src/visualizations.R` to generate static plots in output/plots.
5. Run the Shiny app via `src/dashboard/app.R` for interactive dashboard.

## Notes
- All paths are relative to the project root.
- The dashboard uses a consistent theme from ggthemes (e.g., theme_economist) for plots and shinytheme("flatly") for UI.
- Dataset assumed to have columns as described in the report (e.g., 'satis', 'repur', 'Issuer', 'vn 7002 T01' renamed to 'vn7002T01', etc.).
