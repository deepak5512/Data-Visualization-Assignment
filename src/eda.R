# eda.R
# Script for Exploratory Data Analysis: summaries, correlations, basic plots.

library(dplyr)
library(ggplot2)
library(ggthemes)
library(corrplot)  # For correlations

# Load processed data
processed_data_path <- "data/processed_data.rds"
data <- readRDS(processed_data_path)

# Summary statistics
summary_stats <- summary(data)
write.csv(summary_stats, "output/reports/summary_stats.csv")

# Correlations for satisfaction drivers
cor_vars <- data %>% select(satis, vn_7002_T01:vn_7002_T10)
cor_matrix <- cor(cor_vars, use = "complete.obs")

# Save correlation plot
png("output/reports/correlation_plot.png")
corrplot(cor_matrix, method = "color", tl.cex = 0.8)
dev.off()

# Basic plots: e.g., distribution of satis
p <- ggplot(data, aes(x = satis)) +
  geom_histogram(bins = 20, fill = "blue") +
  theme_economist() +
  labs(title = "Satisfaction Distribution")

ggsave("output/plots/satis_histogram.png", p)

# More EDA as needed, e.g., boxplots by Issuer
p_box <- ggplot(data, aes(x = Issuer, y = satis)) +
  geom_boxplot() +
  theme_economist()

ggsave("output/plots/satis_by_issuer_box.png", p_box)

print("EDA complete. Outputs saved to output/reports and output/plots.")