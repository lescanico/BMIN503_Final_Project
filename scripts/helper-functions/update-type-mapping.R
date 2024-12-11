# Load required libraries
library(stringdist)
library(dplyr)

# Function to update variable type mapping
update_type_mapping <- function(variable_type_mapping, dataset, threshold = 0.5) {
  # Validate inputs
  if (!all(c("Variable", "Type") %in% colnames(variable_type_mapping))) {
    stop("variable_type_mapping must have 'Variable' and 'Type' columns.")
  }
  
  # Extract dataset variables
  dataset_vars <- names(dataset)
  
  # Identify new variables
  new_vars <- dataset_vars[!dataset_vars %in% variable_type_mapping$Variable]
  
  if (length(new_vars) == 0) {
    cat("No new variables to assign.\n")
    return(variable_type_mapping)
  }
  
  # Extract unique existing types
  existing_types <- unique(variable_type_mapping$Type)
  
  # Initialize a data frame to hold new mappings
  new_variable_mapping <- tibble(Variable = character(), Type = character())
  
  # Process each new variable
  for (var in new_vars) {
    # Automatic suggestion using string similarity
    distances <- stringdist(var, variable_type_mapping$Variable, method = "jw")
    min_distance <- min(distances, na.rm = TRUE)
    auto_suggestion <- if (min_distance <= threshold) {
      variable_type_mapping$Type[which.min(distances)]
    } else {
      "unknown"
    }
    
    cat(sprintf("\nVariable: %s (Suggested: %s)\n", var, auto_suggestion))
    cat("Choose assignment method:\n")
    cat("[1] Automatic\n")
    cat("[2] Manual\n")
    method_choice <- readline(prompt = "Enter your choice (1 or 2): ")
    
    if (method_choice == "1") {
      # Automatic Assignment
      assigned_type <- auto_suggestion
      cat(sprintf("Automatically assigned type: %s\n", assigned_type))
    } else if (method_choice == "2") {
      # Manual Assignment
      cat("Existing Types:\n")
      cat(sprintf("[%d] %s\n", seq_along(existing_types), existing_types))
      cat(sprintf("[%d] Add Custom Type\n", length(existing_types) + 1))
      
      type_index <- readline(prompt = "Choose a type (enter number): ")
      
      assigned_type <- if (type_index %in% seq_along(existing_types)) {
        existing_types[as.numeric(type_index)]
      } else if (type_index == as.character(length(existing_types) + 1)) {
        readline(prompt = "Enter custom type: ")
      } else {
        "unknown"
      }
      
      cat(sprintf("Manually assigned type: %s\n", assigned_type))
    } else {
      assigned_type <- "unknown"
      cat("Invalid choice. Assigned type: unknown\n")
    }
    
    # Append to new variable mapping
    new_variable_mapping <- add_row(new_variable_mapping, Variable = var, Type = assigned_type)
  }
  
  # Combine with existing mapping
  final_mapping <- bind_rows(variable_type_mapping, new_variable_mapping) %>%
    arrange(Variable)
  
  return(final_mapping)
}