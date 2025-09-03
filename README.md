# CSISG Credit Card Dashboard Project

## Prerequisites
- **R** (version 4.0 or higher)
- **RStudio** (recommended for better user experience)

## Step 1: Install Required R Packages
Before running any code, install all required packages by running this command in R:

```r
install.packages(c(
  "readxl", "dplyr", "tidyr", "forcats", "ggplot2", "ggthemes", 
  "scales", "corrplot", "ggridges", "fmsb", "networkD3", "stringr", 
  "grid", "shiny", "shinythemes", "shinydashboard", "RColorBrewer"
))
```

## Step 2: Prepare Your Data
1. **Create the data directory structure** (if it doesn't exist):
   ```
   Data-Visualization-Assignment/
   ├── data/
   │   └── raw/
   │       └── csisg_dataset.xlsx
   ```

2. **Place your Excel data file**:
   - Navigate to the `data/raw/` folder
   - Place your CSISG credit card dataset as `csisg_dataset.xlsx`

## Step 3: Run the Analysis Pipeline
Execute the following R scripts **in order**:

### 3.1 Data Cleaning
```r
source("src/data_cleaning.R")
```
- Loads the raw Excel data from `data/raw/csisg_dataset.xlsx`
- Performs initial data cleaning and variable selection
- Saves cleaned data as `data/cleaned_data.rds`

### 3.2 Data Preprocessing
```r
source("src/preprocessing.R")
```
- Further processes the cleaned data (binning, recoding, transformations)
- Creates derived variables and categories
- Saves processed data as `data/processed_data.rds`

### 3.3 Exploratory Data Analysis
```r
source("src/eda.R")
```
- Generates summary statistics and correlation analysis
- Creates exploratory plots and reports
- Saves outputs to `output/reports/`

### 3.4 Generate Static Visualizations
```r
source("src/visualizations.R")
```
- Creates all static plots and charts
- Generates various visualizations (bar charts, heatmaps, radar charts, etc.)
- Saves plots to `output/plots/`

## Step 4: Launch the Interactive Dashboard
```r
# Launch the dashboard
runApp("src/dashboard")
```

## Expected Output Structure
After running all scripts, your project should have this structure:
```
Data-Visualization-Assignment/
├── data/
│   ├── raw/
│   │   └── csisg_dataset.xlsx
│   ├── cleaned_data.rds
│   └── processed_data.rds
├── src/
│   ├── data_cleaning.R
│   ├── preprocessing.R
│   ├── eda.R
│   ├── visualizations.R
│   └── dashboard/
│       └── app.R
├── output/
│   ├── plots/
│   │   ├── brand_perception_radar.png
│   │   ├── brand_perceptions_heatmap.png
│   │   ├── competing_cards_histogram.png
│   │   └── [other visualization files]
│   └── reports/
│       ├── correlation_plot.png
│       └── summary_stats.csv
└── README.md
```
