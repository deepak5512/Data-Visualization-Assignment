# preprocessing.R
# Script for further data manipulation: binning, recoding, feature engineering.

library(dplyr)
library(tidyr)

# Load cleaned data
cleaned_data_path <- "data/cleaned_data.rds"
data <- readRDS(cleaned_data_path)

# Binning examples as needed for plots
# Bin age into groups
data <- data %>%
  mutate(
    age_group = case_when(
      age < 25 ~ "Under 25",
      age >= 25 & age < 35 ~ "25-34",
      age >= 35 & age < 45 ~ "35-44",
      age >= 45 & age < 55 ~ "45-54",
      age >= 55 ~ "55+"
    ),
    # Bin monthly spend (vn7002T23)
    spend_bin = cut(vn_7002_T23, breaks = c(0, 500, 1000, 2000, 5000, Inf), labels = c("<500", "500-1000", "1000-2000", "2000-5000", "5000+")),
    # Bin satisfaction for Sankey
    satis_bin = cut(satis, breaks = c(0, 5, 7, 8.5, 10), labels = c("Low", "Medium", "High", "Very High"))
  )

# Recode categorical if needed (e.g., Issuer mappings already in cleaning)

# Handle purchase categories: Pivot L1_1 to L1_12 into long form for Sankey if needed
purchase_long <- data %>%
  select(uid, L1_1:L1_12, satis_bin) %>%  # Assume uid is respondent ID
  pivot_longer(cols = L1_1:L1_12, names_to = "purchase_category", values_to = "indicator") %>%
  filter(indicator == 1) %>%  # Only where purchase occurred
  select(uid, purchase_category, satis_bin)

# But for now, keep wide; save long if needed separately

# Save processed data
processed_data_path <- "data/processed_data.rds"
saveRDS(data, processed_data_path)

print("Preprocessing complete. Processed data saved to ../data/processed_data.rds")