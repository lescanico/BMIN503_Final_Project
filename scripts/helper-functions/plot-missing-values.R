# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Define function to plot missing values
plot_missing_values_by_type <- function(dataset, min_missing, max_missing, file_name = NULL) {
  # Get dataset name dynamically
  dataset_name <- deparse(substitute(dataset))
  
  # Automatically generate a default file name if none is provided
  if (is.null(file_name)) {
    file_name <- sprintf("figures/plots/%s_%d-%d_missing_plot.png", dataset_name, min_missing, max_missing)
  }
  
  # Create a dynamic title
  plot_title <- sprintf("Missing Values (%d-%d%%) for %s", min_missing, max_missing, dataset_name)
  
  # Ensure column types are matched with the lookup dataframe
  variable_types <- setNames(variable_type_mapping$Type, variable_type_mapping$Variable)
  
  # Create missing data summary
  missing_summary <- dataset %>%
    summarise(across(everything(), ~ sum(is.na(.)) / n() * 100)) %>%
    pivot_longer(everything(), names_to = "column", values_to = "missing_percentage") %>%
    mutate(type = variable_types[column]) %>%
    filter(missing_percentage > min_missing & missing_percentage <= max_missing)
  
  if (nrow(missing_summary) > 0) {
    threshold_inside <- max_missing * 0.8
    p <- ggplot(missing_summary, aes(x = reorder(column, -missing_percentage), y = missing_percentage, fill = type)) +
      geom_bar(stat = "identity", position = position_dodge2(width = 0.9)) +
      geom_text(data = missing_summary %>% filter(missing_percentage > 0), aes(
        label = ifelse(missing_percentage < 0.01, "<0.01", sprintf("%.2f", missing_percentage)),
        hjust = ifelse(missing_percentage > threshold_inside, 1.2, -0.2)
      ), size = 3, color = ifelse(missing_summary$missing_percentage > threshold_inside, "white", "black")) +
      coord_flip(clip = "off") +
      labs(
        title = plot_title, # Dynamic title
        x = "Column", 
        y = "Missing Percentage"
      ) +
      scale_fill_manual(values = c(
        "Date" = "#1f77b4",
        "hms" = "#ffcc00",
        "factor" = "#76c77c",
        "numeric" = "#d62728",
        "logical" = "#a564c9",
        "list_as_character" = "#b07d5b"
      )) +
      theme_minimal() +
      theme(
        legend.position = "top", 
        legend.title = element_blank(), 
        legend.direction = "horizontal",
        axis.text.y = element_text(size = 7, hjust = 1),
        plot.title = element_text(size = 14, face = "bold", hjust = 0),
        plot.margin = margin(20, 20, 20, 20),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)
      )
    
    # Save the plot to the file
    dir.create(dirname(file_name), showWarnings = FALSE, recursive = TRUE)
    ggsave(file_name, plot = p, width = 8, height = 6, bg = "white")
    message("Plot saved to: ", file_name)
  } else {
    message("No columns with missing data in the specified range.")
  }
}