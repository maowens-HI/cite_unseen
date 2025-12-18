# ==============================================================================
# TEXT UTILITIES
# Helper functions for text processing
# ==============================================================================

# ------------------------------------------------------------------------------
# STRING CLEANING
# ------------------------------------------------------------------------------

#' Remove extra whitespace from string
#'
#' @param x Character vector
#' @return Cleaned character vector
clean_whitespace <- function(x) {
  x %>%
    str_trim() %>%
    str_squish()
}

#' Remove non-printable characters
#'
#' @param x Character vector
#' @return Cleaned character vector
remove_non_printable <- function(x) {
  str_remove_all(x, "[^[:print:]]")
}

#' Standardize quotes and apostrophes
#'
#' @param x Character vector
#' @return Standardized character vector
standardize_quotes <- function(x) {
  x %>%
    # Smart quotes to straight quotes
    str_replace_all("[\u2018\u2019\u201A\u201B]", "'") %>%
    str_replace_all("[\u201C\u201D\u201E\u201F]", '"') %>%
    # Fancy dashes to regular dash
    str_replace_all("[\u2013\u2014]", "-")
}

# ------------------------------------------------------------------------------
# UNICODE HANDLING
# ------------------------------------------------------------------------------

#' Transliterate Unicode to ASCII
#'
#' @param x Character vector
#' @return ASCII-safe character vector
transliterate_to_ascii <- function(x) {
  if (requireNamespace("stringi", quietly = TRUE)) {
    stringi::stri_trans_general(x, "Latin-ASCII")
  } else {
    # Fallback: just remove non-ASCII
    iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  }
}

#' Check if string contains non-ASCII characters
#'
#' @param x Character vector
#' @return Logical vector
has_non_ascii <- function(x) {
  str_detect(x, "[^\x00-\x7F]")
}

# ------------------------------------------------------------------------------
# CITATION-SPECIFIC UTILITIES
# ------------------------------------------------------------------------------
#' Extract DOI from citation text
#'
#' @param citation Character string
#' @return DOI string or NA
extract_doi <- function(citation) {
  doi_pattern <- "10\\.\\d{4,}/[^\\s]+"
  str_extract(citation, doi_pattern)
}

#' Check if citation looks complete
#'
#' @param citation Character string
#' @return Logical
is_complete_citation <- function(citation) {
  # Check for basic components
  has_author <- str_detect(citation, "^[A-Z][a-z]+")
  has_year <- str_detect(citation, "\\(\\d{4}\\)")
  has_title <- nchar(citation) > 30

  has_author & has_year & has_title
}

#' Estimate citation type (journal, book, etc.)
#'
#' @param citation Character string
#' @return Character: "journal", "book", "chapter", "other"
classify_citation_type <- function(citation) {
  citation_lower <- str_to_lower(citation)

  case_when(
    str_detect(citation_lower, "journal|review|quarterly") ~ "journal",
    str_detect(citation_lower, "press|publisher|edition") ~ "book",
    str_detect(citation_lower, "chapter|in:|eds?\\.") ~ "chapter",
    str_detect(citation_lower, "working paper|nber|ssrn") ~ "working_paper",
    TRUE ~ "other"
  )
}

# ------------------------------------------------------------------------------
# BATCH TEXT OPERATIONS
# ------------------------------------------------------------------------------

#' Apply multiple cleaning steps to text
#'
#' @param x Character vector
#' @return Cleaned character vector
clean_text_pipeline <- function(x) {
  x %>%
    remove_non_printable() %>%
    standardize_quotes() %>%
    clean_whitespace()
}

#' Safe string operation wrapper
#'
#' @param x Input value
#' @param fn Function to apply
#' @param default Default value on error
#' @return Result of fn(x) or default
safe_string_op <- function(x, fn, default = NA_character_) {
  tryCatch(
    fn(x),
    error = function(e) default
  )
}

# ------------------------------------------------------------------------------
# VALIDATION HELPERS
# ------------------------------------------------------------------------------

#' Check string lengths and report
#'
#' @param x Character vector
#' @param name Name for reporting
#' @return Invisible original vector
report_string_lengths <- function(x, name = "strings") {

  lengths <- nchar(x)

  message(sprintf(
    "%s length stats: min=%d, median=%d, max=%d, NA=%d",
    name,
    min(lengths, na.rm = TRUE),
    median(lengths, na.rm = TRUE),
    max(lengths, na.rm = TRUE),
    sum(is.na(x))
  ))

  invisible(x)
}

# ------------------------------------------------------------------------------
# MAIN EXECUTION (if run as script)
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  message("Text utilities loaded.")
  message("Available functions:")
  message("  - clean_whitespace()")
  message("  - remove_non_printable()")
  message("  - standardize_quotes()")
  message("  - transliterate_to_ascii()")
  message("  - extract_doi()")
  message("  - is_complete_citation()")
  message("  - classify_citation_type()")
  message("  - clean_text_pipeline()")
}
