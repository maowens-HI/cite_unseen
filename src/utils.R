# ==============================================================================
# UTILITIES
# Simple helper functions
# ==============================================================================

# ==============================================================================
# FILE I/O
# ==============================================================================

#' Read a file, trying different encodings
read_file_safe <- function(path) {
  for (enc in c("UTF-8", "latin1", "windows-1252")) {
    result <- tryCatch({
      paste(readLines(path, encoding = enc, warn = FALSE), collapse = "\n")
    }, error = function(e) NULL)
    if (!is.null(result)) return(result)
  }
  stop("Could not read file: ", path)
}

#' Save data to CSV
save_csv <- function(data, path) {
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  write_csv(data, path)
  message("Saved: ", path)
}

# ==============================================================================
# TEXT CLEANING
# ==============================================================================

#' Clean up text (standardize quotes, remove junk, trim)
clean_text <- function(x) {
  x %>%
    str_remove_all("[^[:print:]]") %>%
    str_replace_all("[\u2018\u2019]", "'") %>%
    str_replace_all("[\u201C\u201D]", '"') %>%
    str_replace_all("[\u2013\u2014]", "-") %>%
    str_squish()
}

#' Extract DOI from citation text
extract_doi <- function(citation) {
  str_extract(citation, "10\\.\\d{4,}/[^\\s]+")
}
