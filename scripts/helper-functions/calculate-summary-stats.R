# Load required libraries
library(moments)

# Define function to calculate summary statistics 
calculate_summary_stats <- function(variable_data, type) {
  # Normalize type for cases like "hms, difftime"
  type <- strsplit(type, ",\\s*")[[1]][1]  # Take the first type if combined
  
  # Convert type to lowercase for consistent handling
  type <- tolower(type)
  
  if (all(is.na(variable_data))) {
    return(list(description = "All values are missing"))
  }
  
  if (type %in% c("numeric", "integer", "double")) {
    # Numeric summary
    stats <- summary(variable_data)
    sd_value <- sd(variable_data, na.rm = TRUE)
    skewness_value <- moments::skewness(variable_data, na.rm = TRUE)
    kurtosis_value <- moments::kurtosis(variable_data, na.rm = TRUE)
    range_values <- range(variable_data, na.rm = TRUE)
    
    description <- sprintf(
      "Range: %.1f to %.1f\n Median: %.1f\n Mean: %.1f\n SD: %.2f\n Skewness: %.2f\n Kurtosis: %.2f",
      range_values[1], range_values[2], stats["Median"], stats["Mean"], 
      sd_value, skewness_value, kurtosis_value
    )
    
  } else if (type == "factor") {
    # Factor summary
    levels_count <- if (!is.null(levels(variable_data))) nlevels(variable_data) else 0
    freq_table <- sort(table(variable_data), decreasing = TRUE)
    most_common <- if (length(freq_table) > 0) names(freq_table[1]) else "None"
    most_common_count <- if (length(freq_table) > 0) freq_table[1] else 0
    missing_levels <- sum(is.na(variable_data))
    
    description <- sprintf(
      "Levels: %d\n Most Common: %s (%d occurrences)\n Missing Levels: %d",
      levels_count, most_common, most_common_count, missing_levels
    )
    
  } else if (type == "logical") {
    # Logical summary
    true_percent <- if (length(variable_data[!is.na(variable_data)]) > 0) {
      mean(variable_data, na.rm = TRUE) * 100
    } else {
      0
    }
    false_percent <- 100 - true_percent
    
    description <- sprintf(
      "TRUE: %.1f%%\n FALSE: %.1f%%",
      true_percent, false_percent
    )
    
  } else if (type %in% c("date", "posixct", "hms", "difftime")) {
    # Date/Time summary
    range_values <- range(variable_data, na.rm = TRUE)
    missing_count <- sum(is.na(variable_data))
    
    description <- sprintf(
      "Range: %s to %s\n Missing: %d",
      format(range_values[1]), format(range_values[2]), missing_count
    )
    
  } else if (type == "list") {
    # List summary
    lengths <- sapply(variable_data, length)
    range_lengths <- range(lengths, na.rm = TRUE)
    median_length <- median(lengths, na.rm = TRUE)
    mean_length <- mean(lengths, na.rm = TRUE)
    
    description <- sprintf(
      "Range Lengths: %d to %d\n Median Length: %.1f\n Mean Length: %.1f",
      range_lengths[1], range_lengths[2], median_length, mean_length
    )
    
  } else if (type == "character") {
    # Character summary
    unique_values <- length(unique(variable_data[!is.na(variable_data)]))
    most_common <- if (length(variable_data[!is.na(variable_data)]) > 0) {
      names(sort(table(variable_data), decreasing = TRUE))[1]
    } else {
      "None"
    }
    missing_count <- sum(is.na(variable_data))
    
    description <- sprintf(
      "Unique Values: %d\n Most Common: %s\n Missing: %d",
      unique_values, most_common, missing_count
    )
    
  } else {
    # Unsupported type
    description <- sprintf("Unsupported type: %s", type)
  }
  
  return(list(description = description))
}