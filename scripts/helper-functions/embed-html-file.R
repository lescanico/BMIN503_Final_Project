# Helper function to read and embed HTML content
embed_html <- function(file_path) {
  if (file.exists(file_path)) {
    html_content <- paste(readLines(file_path, warn = FALSE), collapse = "\n")
    cat(sprintf("<div class='scrollable-container' style='height:600px; overflow:auto;'>%s</div>", html_content))
  } else {
    cat(sprintf("<p>Error: File '%s' not found.</p>", file_path))
  }
}