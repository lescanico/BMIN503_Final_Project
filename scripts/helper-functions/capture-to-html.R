# Load required libraries
library(dplyr)
library(base64enc)

# Define function to capture output to HTML file
capture_output_to_html <- function(file, ...) {
  # Ensure file is provided
  if (missing(file)) stop("A file name must be provided.")
  
  # Define the output path
  file <- file.path("outputs", file)
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  
  # Open the file for writing
  con <- file(file, "w")
  
  # Write HTML header
  cat("<html>\n<head>\n", file = con)
  cat("<link rel=\"stylesheet\" href=\"../style.css\">\n", file = con)
  cat("</head>\n<body>\n", file = con)
  
  # Iterate through arguments
  args <- list(...)
  for (i in seq_along(args)) {
    if (is.null(names(args)[i]) || names(args)[i] == "") {
      # Unnamed argument: Render as <h5>
      cat(sprintf("<h5>%s</h5>\n", args[[i]]), file = con)
    } else {
      # Named argument: Render as <h6> + Content
      cat(sprintf("<h6>%s</h6>\n", names(args)[i]), file = con)
      
      # Check content type
      if (is.character(args[[i]])) {
        # Render character vectors as preformatted text
        cat('<div class="scrollable-inner-container">\n', file = con)
        cat(sprintf("<pre>%s</pre>\n", paste(args[[i]], collapse = "\n")), file = con)
        cat("</div>\n", file = con)
      } else if (is.data.frame(args[[i]])) {
        # Render data frames as tables
        cat('<div class="scrollable-inner-container">\n', file = con)
        cat("<table>\n<tr>", file = con)
        for (col_name in names(args[[i]])) {
          cat(sprintf("<th>%s</th>", col_name), file = con)
        }
        cat("</tr>\n", file = con)
        for (row in seq_len(nrow(args[[i]]))) {
          cat("<tr>", file = con)
          for (col in names(args[[i]])) {
            value <- as.character(args[[i]][row, col])
            value <- gsub("<", "&lt;", value)
            value <- gsub(">", "&gt;", value)
            cat(sprintf("<td>%s</td>", value), file = con)
          }
          cat("</tr>\n", file = con)
        }
        cat("</table>\n</div>\n", file = con)
      } else {
        # Render other objects as preformatted text
        cat('<div class="scrollable-inner-container">\n', file = con)
        cat("<pre>\n", file = con)
        tryCatch(
          {
            output <- capture.output(print(args[[i]]))
            cat(paste(output, collapse = "\n"), "\n", file = con)
          },
          error = function(e) {
            cat(sprintf("Error rendering section '%s': %s", names(args)[i], e$message), file = con)
          }
        )
        cat("</pre>\n</div>\n", file = con)
      }
    }
  }
  
  # Close HTML tags
  cat("</body>\n</html>\n", file = con)
  close(con)
  
  # Inform the user
  message("HTML saved to: ", file)
}