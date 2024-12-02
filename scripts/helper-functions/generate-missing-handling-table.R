# Load required libraries
library(dplyr)

# Define function to generate data handling table by variable types and missingness
generate_data_handling_table <- function(dataset, dataset_name = deparse(substitute(dataset))) {
  
  # Load variable type lookup
  variable_type_lookup <- readRDS("datasets/variable_type_lookup.rds")
  variable_types <- setNames(variable_type_lookup$Type, variable_type_lookup$Variable)
  
  # Prompt user for mode selection
  cat("Select function mode:\n")
  cat("1: Manually select handling strategy for each variable\n")
  cat("2: Print all handling strategy options for each variable\n")
  cat("3: Randomly select handling strategy for each variable (function testing)\n")
  
  repeat {
    selected_mode <- as.integer(readline("Enter your choice (1, 2, or 3): "))
    if (selected_mode %in% c(1, 2, 3)) break
    cat("Invalid choice. Try again.\n")
  }
  
  # Summarize missing data
  missing_data_summary <- dataset %>%
    summarise(across(everything(), ~ sum(is.na(.)) / n() * 100)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "missing_percentage") %>%
    filter(missing_percentage > 0) %>%
    rowwise() %>%
    mutate(
      type = case_when(
        grepl("numeric|integer|double", variable_types[variable], ignore.case = TRUE) ~ "Numeric",
        grepl("list", variable_types[variable], ignore.case = TRUE) ~ "List",
        grepl("factor", variable_types[variable], ignore.case = TRUE) ~ "Factor",
        grepl("logical", variable_types[variable], ignore.case = TRUE) ~ "Logical",
        grepl("character", variable_types[variable], ignore.case = TRUE) ~ "Character",
        grepl("date|posix|hms|difftime", variable_types[variable], ignore.case = TRUE) ~ "Date",
        TRUE ~ "Unsupported Type"
      ),
      missingness_level = case_when(
        missing_percentage <= 5 ~ "Low (0-5%)",
        missing_percentage > 5 & missing_percentage <= 30 ~ "Moderate (5-30%)",
        missing_percentage > 30 & missing_percentage <= 50 ~ "High (30-50%)",
        missing_percentage > 50 ~ "Very High (>50%)"
      )
    ) %>%
    ungroup()
  
  # Initialize an empty list to store handling strategies
  handling_decisions <- list()
  
  # Process each variable based on mode
  for (i in seq_len(nrow(missing_data_summary))) {
    row <- missing_data_summary[i, ]
    variable <- row$variable
    missing_level <- row$missingness_level
    var_type <- row$type
    
    # Fetch options for the current variable
    options <- missing_data_options[[missing_level]][[var_type]]
    
    # Check for valid options
    if (is.null(options) || length(options) == 0) {
      handling_decisions[[variable]] <- "No valid options available"
      next
    }
    
    # Handle mode-specific behavior
    if (selected_mode == 1) {
      # Manual selection mode with custom option
      cat(sprintf("Variable: %s (Type: %s, Missingness Level: %s)\n", variable, var_type, missing_level))
      cat("Available Handling Strategies:\n")
      for (j in seq_along(options)) {
        cat(sprintf("%d: %s\n", j, options[j]))
      }
      cat(sprintf("%d: Enter a custom strategy\n", length(options) + 1))
      
      repeat {
        choice <- suppressWarnings(as.integer(readline("Select a handling strategy by number: ")))
        if (!is.na(choice) && choice %in% seq_along(options)) {
          handling_decisions[[variable]] <- options[choice]
          break
        } else if (!is.na(choice) && choice == length(options) + 1) {
          repeat {
            custom_strategy <- readline("Enter your custom strategy (cannot be empty): ")
            if (nchar(trimws(custom_strategy)) > 0) {
              handling_decisions[[variable]] <- custom_strategy
              break
            } else {
              cat("Custom strategy cannot be empty. Please try again.\n")
            }
          }
          break
        } else {
          cat("Invalid choice. Please enter a valid number.\n")
        }
      }
    } else if (selected_mode == 3) {
      # Random selection mode
      random_choice <- sample(options, 1)
      handling_decisions[[variable]] <- random_choice
    } else if (selected_mode == 2) {
      # Print options only
      handling_decisions[[variable]] <- paste(sprintf("%d) %s", seq_along(options), options), collapse = " | ")
    }
  }
  
  # Compile results into a table
  handling_summary <- missing_data_summary %>%
    mutate(
      `Missing %` = sprintf("%.1f%%", missing_percentage),
      `Handling Strategy` = sapply(variable, function(v) handling_decisions[[v]])
    ) %>%
    select(variable, `Missing %`, type, `Handling Strategy`) %>%
    rename(
      Variable = variable,
      Type = type
    ) %>%
    arrange(as.numeric(gsub("%", "", `Missing %`)))
  
  return(handling_summary)
}