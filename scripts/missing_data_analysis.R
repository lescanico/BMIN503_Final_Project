# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Load merged dataset
merged_data <- readRDS("datasets/merged_data.rds")

# Analyze missing data
missing_summary <- merged_data %>%
  summarise(across(everything(), ~ sum(is.na(.)) / n() * 100)) %>%
  pivot_longer(everything(), names_to = "column", values_to = "missing_percentage")

# Plot missing data
ggplot(missing_summary, aes(x = reorder(column, -missing_percentage), y = missing_percentage)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Missing Data Analysis", x = "Columns", y = "Missing Percentage") +
  theme_minimal()

# Save plot
ggsave("figures/missing_data_analysis.png")

# Print summary
write.csv(missing_summary, "outputs/missing_data_summary.csv", row.names = FALSE)
cat("Missing data analysis complete. Results saved to 'outputs/' and visualized in 'figures/'.\n")