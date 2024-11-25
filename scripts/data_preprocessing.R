# Load required libraries
library(dplyr)
library(stringr)
library(readr)

# Load datasets
patient_data <- readRDS("datasets/patient_data_anonymized.rds")
visit_data <- readRDS("datasets/visit_data_anonymized.rds")

# Load variable type lookup
variable_type_lookup <- readRDS("datasets/variable_type_lookup.rds")

# Standardize column names
standardize_column_names <- function(dataset) {
  colnames(dataset) <- colnames(dataset) %>%
    str_to_lower() %>%
    str_replace_all("[\\s\\.\\/\\?\\-\\(\\)\\%\\$]+", "_") %>%
    str_replace_all("_+", "_") %>%
    str_replace_all("_$", "")
  dataset
}

# Apply standardization
patient_data <- standardize_column_names(patient_data)
visit_data <- standardize_column_names(visit_data)

# Save renamed datasets
saveRDS(patient_data, "datasets/patient_data_renamed.rds")
saveRDS(visit_data, "datasets/visit_data_renamed.rds")

# Print confirmation
cat("Preprocessing complete. Cleaned datasets saved to 'datasets/'.\n")