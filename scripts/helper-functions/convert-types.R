# Load required libraries
library(dplyr)
library(lubridate)
library(hms)

# Function to convert data types
convert_types <- function(df) {
  for (i in seq_len(nrow(group_summary))) {
    vars <- group_summary$Variables[[i]]
    type <- group_summary$Type[i]
    
    # Only apply transformations to variables that exist in the dataset
    vars <- intersect(vars, names(df))
    if (length(vars) == 0) next
    
    df <- df |>
      mutate(across(any_of(vars), ~ {
        if (type == "numeric") {
          suppressWarnings(as.numeric(as.character(.)))
        } else if (type == "list") {
          tryCatch(
            strsplit(iconv(as.character(.), from = "", to = "UTF-8"), ","),
            error = function(e) { message("Error in list conversion: ", e); NA }
          )
        } else if (type == "factor") {
          as.factor(as.character(.))
        } else if (type == "logical") {
          case_when(
            . == 1 ~ TRUE,
            . == 0 ~ FALSE,
            is.na(.) ~ NA,
            TRUE ~ NA
          )
        } else if (type == "Date") {
          tryCatch(
            as.Date(parse_date_time(., orders = c("ymd", "dmy", "mdy"), quiet = TRUE)),
            error = function(e) { message("Error in Date conversion: ", e); NA }
          )
        } else if (type == "hms") {
          tryCatch(as_hms(as.numeric(.)), error = function(e) { message("Error in hms conversion: ", e); NA })
        } else if (type == "identifier") {
          as.character(.)
        } else if (type == "list_as_character") {
          as.character(.)
        } else {
          .
        }
      }))
  }
  return(df)
}