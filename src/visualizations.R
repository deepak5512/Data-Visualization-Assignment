# visualizations.R
# Script to generate all planned static visualizations and save to output/plots.

library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggridges)  # For ridge plots
library(fmsb)  # For radar charts
library(networkD3)  # For Sankey, but here use for static; alternatively ggalluvial

# Load processed data
processed_data_path <- "data/processed_data.rds"
data <- readRDS(processed_data_path)

# Consistent theme
custom_theme <- theme_economist() + theme(legend.position = "bottom")

# 1. Stacked Bar: Market Share by Issuer and Card Type
p1 <- ggplot(data, aes(x = Issuer, fill = Q34_Creditcard_code)) +
  geom_bar(position = "stack") +
  custom_theme +
  labs(title = "Market Share by Issuer and Card Type")

ggsave("output/plots/market_share_stacked_bar.png", p1)

# 2. Histogram: Customer Satisfaction Distribution
p2 <- ggplot(data, aes(x = satis)) +
  geom_histogram(bins = 30) +
  custom_theme +
  labs(title = "Customer Satisfaction Distribution")

ggsave("output/plots/satis_histogram.png", p2)

# 3. Violin Plot: Satisfaction and Loyalty by Issuer
data_long <- data %>% pivot_longer(cols = c(satis, repur, recomm), names_to = "metric", values_to = "value")

p3 <- ggplot(data_long, aes(x = Issuer, y = value, fill = metric)) +
  geom_violin() +
  custom_theme +
  labs(title = "Satisfaction and Loyalty by Issuer")

ggsave("output/plots/satis_loyalty_violin.png", p3)

# 4. Faceted Bar: Monthly Spend Distribution by Issuer
p4 <- ggplot(data, aes(x = spend_bin)) +
  geom_bar() +
  facet_wrap(~Issuer) +
  custom_theme +
  labs(title = "Monthly Spend Distribution by Issuer")

ggsave("output/plots/monthly_spend_faceted_bar.png", p4)

# 5. Heatmap: Brand Perceptions vs. Satisfaction
cor_vars <- data %>% select(satis, vn_7002_T01:vn_7002_T10)
cor_matrix <- cor(cor_vars, use = "complete.obs")

# Use corrplot for heatmap
png("output/plots/brand_perceptions_heatmap.png")
corrplot(cor_matrix, method = "shade", tl.cex = 0.8)
dev.off()

# 6. Scatter Plot: Merchant Tie-Ups vs. Satisfaction
p6 <- ggplot(data, aes(x = vn_7002_T08, y = satis, color = Issuer)) +
  geom_point() +
  geom_smooth(method = "lm") +
  custom_theme +
  labs(title = "Merchant Tie-Ups vs. Satisfaction")

ggsave("output/plots/merchant_tieups_scatter.png", p6)

# 7. Radar Chart: Brand Perception Profile
radar_data <- data %>%
  group_by(Issuer) %>%
  summarise(across(vn_7002_T01:vn_7002_T10, mean, na.rm = TRUE)) %>%
  as.data.frame()

# Select only the numeric brand perception variables, excluding Issuer
radar_numeric <- radar_data[, -1]  # Remove Issuer column
issuers <- radar_data$Issuer

# Create max_min with 2 rows (max, min) and the same 10 columns as radar_numeric
max_min <- data.frame(
  vn_7002_T01 = c(10, 0),
  vn_7002_T02 = c(10, 0),
  vn_7002_T03 = c(10, 0),
  vn_7002_T04 = c(10, 0),
  vn_7002_T05 = c(10, 0),
  vn_7002_T06 = c(10, 0),
  vn_7002_T07 = c(10, 0),
  vn_7002_T08 = c(10, 0),
  vn_7002_T09 = c(10, 0),
  vn_7002_T10 = c(10, 0)
)
# Ensure max_min has row names for clarity (optional, but helps with radarchart)
rownames(max_min) <- c("Max", "Min")

# Combine max_min and radar_numeric
radar_combined <- rbind(max_min, radar_numeric[, colnames(max_min)])  # Match columns explicitly

png("output/plots/brand_perception_radar.png")
radarchart(radar_combined, axistype = 1, pcol = rainbow(nrow(radar_numeric)), plty = 1)
legend("topright", legend = issuers, col = rainbow(length(issuers)), lty = 1)
dev.off()

# 8. Bar Chart: Complaint Rates by Issuer
p8 <- ggplot(data, aes(x = Issuer, fill = factor(comp))) +
  geom_bar(position = "fill") +
  custom_theme +
  labs(title = "Complaint Rates by Issuer")

ggsave("output/plots/complaint_rates_bar.png", p8)

# 9. Dot Plot: Service Channel Ratings
channel_data <- data %>%
  select(Issuer, vn_7002_T16, vn_7002_T18, vn_7002_T19) %>%
  pivot_longer(cols = -Issuer, names_to = "channel", values_to = "rating") %>%
  group_by(Issuer, channel) %>%
  summarise(mean_rating = mean(rating, na.rm = TRUE))

p9 <- ggplot(channel_data, aes(x = channel, y = mean_rating, color = Issuer)) +
  geom_point(size = 4) +
  custom_theme +
  labs(title = "Service Channel Ratings")

ggsave("output/plots/service_channel_dot.png", p9)

# 10. Stacked Proportional Bar: Professional & Marital Profile by Issuer
p10 <- ggplot(data, aes(x = Issuer, fill = marital)) +
  geom_bar(position = "fill") +
  custom_theme +
  labs(title = "Marital Profile by Issuer")

ggsave("output/plots/marital_profile_stacked.png", p10)

# Similar for work or childsupp

# 11. Ridge Plot: Satisfaction by Age Group
p11 <- ggplot(data, aes(x = satis, y = age_group, fill = Issuer)) +
  geom_density_ridges() +
  custom_theme +
  labs(title = "Satisfaction by Age Group")

ggsave("output/plots/satis_age_ridge.png", p11)

# 12. Sankey Diagram: Purchase Categories to Satisfaction
# Use networkD3 for Sankey
# First, prepare nodes and links
purchase_long <- data %>%
  select(L1_1:L1_12, satis_bin) %>%
  pivot_longer(cols = L1_1:L1_12, names_to = "category", values_to = "value") %>%
  filter(value == 1) %>%
  group_by(category, satis_bin) %>%
  summarise(freq = n()) %>%
  ungroup()

nodes <- data.frame(name = c(unique(purchase_long$category), unique(purchase_long$satis_bin)))
links <- purchase_long %>%
  mutate(source = match(category, nodes$name) - 1,
         target = match(satis_bin, nodes$name) - 1,
         value = freq)

sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target", Value = "value", NodeID = "name")
# Save as HTML
htmlwidgets::saveWidget(sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target", Value = "value", NodeID = "name"),
                        "output/plots/purchase_satis_sankey.html")

# 13. Histogram: Number of Competing Cards
p13 <- ggplot(data, aes(x = vn_7002_T20, fill = Issuer)) +
  geom_histogram(position = "dodge") +
  custom_theme +
  labs(title = "Number of Competing Cards")

ggsave("output/plots/competing_cards_histogram.png", p13)

print("Visualizations generated and saved to output/plots.")