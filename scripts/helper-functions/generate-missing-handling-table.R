# Define missing data handling options
missing_data_options <- list(
  "Low (0-5%)" = list(
    Numeric = c(
      "Predictive Modeling (Regression)",
      "Predictive Modeling (KNN)",
      "Multiple Imputation (MICE)",
      "Mean Imputation",
      "Median Imputation",
      "Linear Interpolation",
      "Spline Interpolation",
      "Indicator Variable + Mean",
      "Indicator Variable + Median"
    ),
    Factor = c(
      "Predictive Modeling (Decision Tree)",
      "Predictive Modeling (Random Forest)",
      "Frequency-Based Imputation (Weighted by Subgroup Frequency)",
      "Mode Imputation",
      "Add 'Unknown' Category",
      "Add 'Other' Category"
    ),
    Logical = c(
      "Predictive Modeling (Logistic Regression)",
      "Mode Imputation",
      "Add Indicator for Missingness",
      "Replace with Most Frequent Value",
      "Assume FALSE (if reasonable)",
      "Assume TRUE (if reasonable)"
    ),
    Date = c(
      "Median Date Imputation",
      "Forward Fill (if sequential)",
      "Backward Fill (if sequential)",
      "Linear Interpolation",
      "Spline Interpolation",
      "Use Most Frequent Date",
      "Use Previous Valid Value",
      "Use Subsequent Valid Value"
    ),
    hms = c(
      "Median Timestamp Imputation",
      "Forward Fill (if sequential)",
      "Backward Fill (if sequential)",
      "Linear Interpolation",
      "Spline Interpolation",
      "Use Most Frequent Timestamp",
      "Use Previous Valid Timestamp",
      "Use Subsequent Valid Timestamp"
    ),
    List = c(
      "Predictive Modeling (List Similarity-Based)",
      "Weighted Average List Imputation",
      "Replace with Most Frequent List",
      "Replace with Proxy List",
      "Replace with Empty List",
      "Add Indicator for Missingness"
    )
  ),
  "Moderate (5-30%)" = list(
    Numeric = c(
      "Multiple Imputation (MICE)",
      "Predictive Imputation (Regression)",
      "Predictive Imputation (KNN)",
      "Linear Interpolation",
      "Spline Interpolation",
      "Mean Imputation",
      "Median Imputation",
      "Indicator Variable + Mean",
      "Indicator Variable + Median"
    ),
    Factor = c(
      "Predictive Modeling (Random Forest)",
      "Predictive Modeling (Decision Tree)",
      "Frequency-Based Imputation (Weighted by Subgroup Frequency)",
      "Add 'Unknown' Category",
      "Add 'Other' Category",
      "Mode Imputation"
    ),
    Logical = c(
      "Predictive Imputation (Logistic Regression)",
      "Add Indicator for Missingness",
      "Mode Imputation",
      "Replace with Most Frequent Value",
      "Assume FALSE",
      "Assume TRUE"
    ),
    Date = c(
      "Forward Fill (if sequential)",
      "Backward Fill (if sequential)",
      "Linear Interpolation",
      "Spline Interpolation",
      "Indicator + Median Date Imputation",
      "Use Previous Valid Value",
      "Use Subsequent Valid Value"
    ),
    hms = c(
      "Forward Fill (if sequential)",
      "Backward Fill (if sequential)",
      "Linear Interpolation",
      "Spline Interpolation",
      "Indicator + Median Timestamp Imputation",
      "Use Previous Valid Timestamp",
      "Use Subsequent Valid Timestamp"
    ),
    List = c(
      "Predictive Modeling (Clustering-Based List Imputation)",
      "Weighted Average List Imputation",
      "Replace with Most Frequent List",
      "Replace with Proxy List",
      "Replace with Empty List",
      "Create Synthetic List"
    )
  ),
  "High (30-50%)" = list(
    Numeric = c(
      "Indicator Variable + Multiple Imputation (MICE)",
      "Indicator Variable + Predictive Modeling (Regression)",
      "Indicator Variable + Predictive Modeling (KNN)",
      "Linear Interpolation",
      "Spline Interpolation",
      "Mean Imputation",
      "Median Imputation"
    ),
    Factor = c(
      "Predictive Modeling (Random Forest)",
      "Predictive Modeling (Decision Tree)",
      "Add 'Unknown' Category",
      "Add 'Other' Category",
      "Frequency-Based Imputation (Weighted by Subgroup Frequency)",
      "Use Most Frequent Category"
    ),
    Logical = c(
      "Indicator Variable + Predictive Imputation",
      "Add Indicator for Missingness",
      "Mode Imputation",
      "Replace with Most Frequent Value",
      "Assume FALSE",
      "Assume TRUE"
    ),
    Date = c(
      "Indicator Variable + Median Date Imputation",
      "Forward Fill (if sequential)",
      "Backward Fill (if sequential)",
      "Linear Interpolation",
      "Spline Interpolation",
      "Use Placeholder Dates"
    ),
    hms = c(
      "Indicator Variable + Median Timestamp Imputation",
      "Forward Fill (if sequential)",
      "Backward Fill (if sequential)",
      "Linear Interpolation",
      "Spline Interpolation",
      "Use Placeholder Timestamps"
    ),
    List = c(
      "Add Indicator Variable for Missingness",
      "Replace with Most Frequent List",
      "Replace with Proxy List",
      "Replace with Empty List",
      "Create Synthetic List (via Sampling or Clustering)"
    )
  ),
  "Very High (>50%)" = list(
    Numeric = c(
      "Indicator Variable + Rough Imputation (e.g., Overall Mean)",
      "Indicator Variable + Multiple Imputation (MICE)",
      "Add 'Unknown' Category",
      "Drop Variable (if non-critical)"
    ),
    Factor = c(
      "Add 'Unknown' Category",
      "Add 'Other' Category",
      "Use Most Frequent Category",
      "Frequency-Based Imputation (Weighted by Subgroup Frequency)",
      "Drop Variable (if non-critical)"
    ),
    Logical = c(
      "Indicator Variable + Assume FALSE",
      "Replace with Most Frequent Value",
      "Assume TRUE",
      "Drop Variable (if non-critical)"
    ),
    Date = c(
      "Use Placeholder Dates",
      "Indicator Variable + Rough Date Imputation",
      "Forward Fill (if sequential)",
      "Backward Fill (if sequential)",
      "Drop Variable (if non-critical)"
    ),
    hms = c(
      "Use Placeholder Timestamps",
      "Indicator Variable + Rough Timestamp Imputation",
      "Forward Fill (if sequential)",
      "Backward Fill (if sequential)",
      "Drop Variable (if non-critical)"
    ),
    List = c(
      "Add Indicator for Missingness",
      "Replace with Empty List",
      "Replace with Proxy List",
      "Create Synthetic List",
      "Drop Variable (if non-critical)"
    )
  )
)

generate_data_handling_table <- function(dataset, dataset_name = deparse(substitute(dataset))) {
  
  # Source the helper function for summary statistics
  source("scripts/helper-functions/calculate-summary-stats.R")
  
  # Load variable type lookup
  variable_type_lookup <- readRDS("datasets/mappings/types.rds")
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
      cat(sprintf("Summary Statistics:\n%s\n", calculate_summary_stats(dataset[[variable]], var_type)$description))
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
      handling_decisions[[variable]] <- paste(sprintf("%d) %s", seq_along(options), options), collapse = "\n")
    }
  }
  
  # Add summary statistics column
  summary_stats <- sapply(missing_data_summary$variable, function(v) {
    calculate_summary_stats(dataset[[v]], missing_data_summary$type[missing_data_summary$variable == v])$description
  })
  
  # Compile results into a table
  handling_summary <- missing_data_summary %>%
    mutate(
      `Missing %` = sprintf("%.1f%%", missing_percentage),
      `Handling Strategy` = sapply(variable, function(v) handling_decisions[[v]]),
      `Summary Statistics` = summary_stats
    ) %>%
    select(variable, `Missing %`, type, `Summary Statistics`, `Handling Strategy`) %>%
    rename(
      Variable = variable,
      Type = type
    ) %>%
    arrange(as.numeric(gsub("%", "", `Missing %`)))
  
  return(handling_summary)
}