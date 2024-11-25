# Load required libraries
library(dplyr)
library(stringr)

# Load renamed datasets
patient_data <- readRDS("datasets/patient_data_renamed.rds")
visit_data <- readRDS("datasets/visit_data_renamed.rds")

# Merge datasets
merged_data <- visit_data %>%
  left_join(patient_data, by = "patient_id")

# Resolve duplicate columns
resolve_duplicate_columns <- function(data) {
  duplicate_cols <- names(data)[duplicated(names(data))]
  for (col in duplicate_cols) {
    data <- data %>%
      mutate(!!col := coalesce(get(paste0(col, ".x")), get(paste0(col, ".y")))) %>%
      select(-all_of(c(paste0(col, ".x"), paste0(col, ".y"))))
  }
  return(data)
}

# Apply function to merged data
merged_data <- resolve_duplicate_columns(merged_data)

# Save merged data
saveRDS(merged_data, "datasets/merged_data.rds")

# Print confirmation
cat("Integration complete. Merged dataset saved to 'datasets/merged_data.rds'.\n")