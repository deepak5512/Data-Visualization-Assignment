# data_cleaning.R
# Script to load raw Excel data, perform initial cleaning, and save cleaned RDS.

library(readxl)
library(dplyr)

# Set paths
raw_data_path <- "data/raw/csisg_dataset.xlsx"
cleaned_data_path <- "data/cleaned_data.rds"  # Assuming we create a data/ folder for cleaned if needed; adjust if necessary

# Load data (assume first sheet)
data <- read_excel(raw_data_path)

# Drop redundant variables as per report
data <- data %>%
  select(-Year, -`repur_recode`)  # Use backticks if names have spaces

# Handle missing values: Keep as-is for conditional fields like L1_12_Others, vn7002T11_12others
# For experience vars (vn7002T16, vn7002T18, vn7002T19), retain as they are conditional

# Consistency checks: Map CompanyCodes to Issuer (example, assuming columns exist)
# For simplicity, assume data is consistent; add mappings if needed
# e.g., data <- data %>% mutate(Issuer = ifelse(CompanyCodes %in% c("DBS CREDIT CARDS", "POSB CREDIT CARDS"), "DBS", Issuer))

# Rename columns for easier handling (remove spaces, use underscores)
names(data) <- gsub(" ", "_", names(data))
names(data) <- gsub("\\.", "_", names(data))  # If dots present

# Example renames for key vars (adjust based on actual names)
# Assume after gsub: vn_7002_T01 etc.
# data <- data %>%
#   rename(
#     Q34_Creditcard_code = Q34_Creditcard_code,
#     vn7002T01 = vn_7002_T01,
#     # Add all others similarly: vn7002T02 to vn7002T10, vn7002T11, vn7002T16, etc.
#     vn7002T20 = vn_7002_T20,
#     vn7002T23 = vn_7002_T23,
#     childsupp = childsupp
#     # etc.
#   )

# Save cleaned data as RDS for faster loading
saveRDS(data, cleaned_data_path)

print("Data cleaning complete. Cleaned data saved to ../data/cleaned_data.rds")