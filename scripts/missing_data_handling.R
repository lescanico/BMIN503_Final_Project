# Load required libraries
library(dplyr)
library(tidyr)
library(purrr)

# Load datasets
merged_data <- readRDS("datasets/merged_data.rds")
variable_type_lookup <- readRDS("datasets/variable_type_lookup.rds")

# Define missing data handling strategies
handle_missing_data <- function(data, variable_types) {
  data <- data %>%
    mutate(across(
      .cols = where(is.numeric),
      .fns = ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
    )) %>%
    mutate(across(
      .cols = where(is.character),
      .fns = ~ ifelse(is.na(.), "Unknown", .)
    )) %>%
    mutate(across(
      .cols = where(is.factor),
      .fns = ~ forcats::fct_explicit_na(., na_level = "Unknown")
    ))
  return(data)
}

# Apply handling strategies
cleaned_data <- handle_missing_data(merged_data, variable_type_lookup)

# Save cleaned dataset
saveRDS(cleaned_data, "datasets/cleaned_data.rds")

# Print confirmation
cat("Missing data handling complete. Cleaned dataset saved to 'datasets/cleaned_data.rds'.\n")