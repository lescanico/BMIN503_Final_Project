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
  cat("<meta charset=\"UTF-8\">\n", file = con)
  cat("<link rel=\"stylesheet\" href=\"../style.css\">\n", file = con)
  cat("</head>\n<body>\n", file = con)
  
  # Open outer scrollable container
  cat('<div class="scrollable-container">\n', file = con)
  
  # Iterate through arguments
  args <- list(...)
  for (i in seq_along(args)) {
    if (is.null(names(args)[i]) || names(args)[i] == "") {
      # Unnamed argument: Render as <h5>
      content <- gsub("\n", "<br>", as.character(args[[i]]))
      cat(sprintf("<h5>%s</h5>\n", content), file = con)
    } else {
      # Named argument: Render as <h6> + Content
      cat(sprintf("<h6>%s</h6>\n", names(args)[i]), file = con)
      
      if (is.character(args[[i]])) {
        # Render character vectors as preformatted text
        content <- gsub("\n", "<br>", paste(args[[i]], collapse = "\n"))
        cat('<div class="scrollable-inner-container">\n', file = con)
        cat(sprintf("<pre>%s</pre>\n", content), file = con)
        cat("</div>\n", file = con)
      } else if (is.data.frame(args[[i]])) {
        # Convert all columns to character
        df <- args[[i]]
        df <- lapply(df, as.character)
        df <- as.data.frame(df, stringsAsFactors = FALSE)
        
        # Render data frames as tables
        cat('<div class="scrollable-inner-container">\n', file = con)
        cat("<table>\n<tr>", file = con)
        for (col_name in names(df)) {
          cat(sprintf("<th>%s</th>", col_name), file = con)
        }
        cat("</tr>\n", file = con)
        for (row in seq_len(nrow(df))) {
          cat("<tr>", file = con)
          for (col in names(df)) {
            value <- as.character(df[row, col])
            value <- iconv(value, to = "UTF-8", sub = "byte")
            value <- gsub("\r\n", "<br>", value)
            value <- gsub("\n", "<br>", value)
            value <- gsub("(\\d+)\\)", "\\1&#41;", value)
            value <- gsub("^0\\.0%$", "<0.1%", value)
            value <- gsub("^0\\.00%$", "<0.01%", value)
            cat(sprintf("<td><div class='content-wrapper'>%s</div></td>", value), file = con)
          }
          cat("</tr>\n", file = con)
        }
        cat("</table>\n</div>\n", file = con)
      } else if (is.list(args[[i]])) {
        # Check for images
        images <- args[[i]]
        cat('<div class="scrollable-inner-container">\n', file = con)
        for (img_path in images) {
          if (file.exists(img_path)) {
            img_data <- base64enc::base64encode(img_path)
            cat(sprintf("<img src=\"data:image/png;base64,%s\" alt=\"Image\">\n", img_data), file = con)
          } else {
            warning(sprintf("Image file not found: %s", img_path))
          }
        }
        cat("</div>\n", file = con) 
      } else {
        # Render other objects as preformatted text
        content <- gsub("\n", "<br>", paste(capture.output(print(args[[i]])), collapse = "\n"))
        cat(sprintf("<pre>%s</pre>\n", content), file = con)
      }
    }
  }
  
  # Close outer scrollable container
  cat("</div>\n", file = con)
  
  # Close HTML tags
  cat("</body>\n</html>\n", file = con)
  close(con)
  
  # Inform result
  message("HTML saved to: ", file)
}