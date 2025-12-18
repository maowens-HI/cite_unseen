# ==============================================================================
# UTILITIES
# Simple helper functions for the cite_unseen project
# ==============================================================================

# ------------------------------------------------------------------------------
# FILE READING
# ------------------------------------------------------------------------------

#' Read a file, trying different encodings if needed
#'
#' Sometimes files have weird encoding (especially older academic papers).
#' This tries UTF-8 first, then falls back to other common encodings.
read_file_safe <- function(path) {
  for (enc in c("UTF-8", "latin1", "windows-1252")) {
    result <- tryCatch({
      content <- readLines(path, encoding = enc, warn = FALSE)
      paste(content, collapse = "\n")
    }, error = function(e) NULL)

    if (!is.null(result)) return(result)
  }
  stop("Could not read file: ", path)
}

#' Save data to CSV, creating the directory if needed
save_csv <- function(data, path) {
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  write_csv(data, path)
  message("Saved: ", path)
}

#' Make sure a directory exists (create if it doesn't)
ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    message("Created directory: ", path)
  }
  invisible(path)
}

# ------------------------------------------------------------------------------
# TEXT CLEANING
# ------------------------------------------------------------------------------

#' Clean up whitespace in text
#' Removes extra spaces, trims leading/trailing whitespace
clean_whitespace <- function(x) {
  str_trim(str_squish(x))
}

#' Convert fancy quotes and dashes to normal ones
#' Academic PDFs often have these unicode characters
standardize_quotes <- function(x) {
  x %>%
    str_replace_all("[\u2018\u2019\u201A\u201B]", "'") %>%   # smart single quotes
    str_replace_all("[\u201C\u201D\u201E\u201F]", '"') %>%   # smart double quotes
    str_replace_all("[\u2013\u2014]", "-")                   # em/en dashes
}

#' Remove non-printable characters from text
remove_junk_chars <- function(x) {
  str_remove_all(x, "[^[:print:]]")
}

#' Full text cleanup pipeline
clean_text <- function(x) {
  x %>%
    remove_junk_chars() %>%
    standardize_quotes() %>%
    clean_whitespace()
}

# ------------------------------------------------------------------------------
# CITATION UTILITIES
# ------------------------------------------------------------------------------

#' Extract a DOI from citation text
#' DOIs look like: 10.1234/something
extract_doi <- function(citation) {
  str_extract(citation, "10\\.\\d{4,}/[^\\s]+")
}

#' Guess what type of citation this is
#' Returns: "journal", "book", "working_paper", or "other"
guess_citation_type <- function(citation) {
  cit_lower <- str_to_lower(citation)

  if (str_detect(cit_lower, "working paper|nber|ssrn")) return("working_paper")
  if (str_detect(cit_lower, "press|publisher|edition"))  return("book")
  if (str_detect(cit_lower, "journal|review|quarterly")) return("journal")
  return("other")
}

# ------------------------------------------------------------------------------
# SIMPLE PROGRESS REPORTING
# ------------------------------------------------------------------------------

#' Print a simple progress message
progress_msg <- function(current, total, what = "items") {
  pct <- round(100 * current / total)
  message(sprintf("  %d/%d %s (%d%%)", current, total, what, pct))
}

# Print message when loaded
if (sys.nframe() == 0) {
  message("Utils loaded. Key functions: read_file_safe, save_csv, clean_text, extract_doi")
}
