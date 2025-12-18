# ==============================================================================
# TEXT NORMALIZATION
# Standardize citation text for fuzzy matching
# ==============================================================================
#
# The key idea: convert citations to a simple form for comparison.
#
# Example:
#   "Smith, J. (2020). Machine Learning. Journal, 34(2)."
#   becomes:
#   "smithj2020machinelearningjournal342"
#
# This makes fuzzy matching more reliable by removing punctuation, case, etc.
#
# ==============================================================================

if (!exists("config")) source("src/00_config.R")

# ------------------------------------------------------------------------------
# CORE FUNCTION
# ------------------------------------------------------------------------------

#' Normalize a citation string for matching
#'
#' Converts to lowercase and keeps only letters and numbers.
#'
#' @param citation The citation text
#' @param keep_numbers Keep numbers? (default TRUE, useful for years)
#' @return Normalized string
normalize_citation <- function(citation, keep_numbers = TRUE) {

  if (is.na(citation) || nchar(citation) == 0) return(NA_character_)

  # Convert to lowercase
  result <- str_to_lower(citation)

  # Keep only letters (and optionally numbers)
  if (keep_numbers) {
    result <- str_remove_all(result, "[^a-z0-9]")
  } else {
    result <- str_remove_all(result, "[^a-z]")
  }

  return(result)
}

#' Normalize a vector of citations
#'
#' @param citations Vector of citation strings
#' @param keep_numbers Keep numbers?
#' @return Vector of normalized strings
normalize_citations <- function(citations, keep_numbers = TRUE) {

  if (length(citations) == 0) return(character())

  message("Normalizing ", length(citations), " citations...")

  # Vectorized for speed
  result <- citations %>%
    str_to_lower()

  if (keep_numbers) {
    result <- str_remove_all(result, "[^a-z0-9]")
  } else {
    result <- str_remove_all(result, "[^a-z]")
  }

  # Preserve NAs
  result[is.na(citations)] <- NA_character_

  n_valid <- sum(!is.na(result) & nchar(result) > 0)
  message("  Done. ", n_valid, " valid normalized citations.")

  return(result)
}

# ------------------------------------------------------------------------------
# BATCH PROCESSING
# ------------------------------------------------------------------------------

#' Add normalized_reference column to WOS data
#'
#' @param wos_data Tibble with 'reference' column
#' @return Tibble with added 'normalized_reference' column
normalize_wos_corpus <- function(wos_data) {

  if (!"reference" %in% names(wos_data)) {
    stop("WOS data must have 'reference' column")
  }

  message("Normalizing WOS references...")

  wos_data %>%
    mutate(normalized_reference = normalize_citations(reference))
}

#' Add normalized_citation column to NBER data
#'
#' @param nber_citations Tibble with 'raw_citation' column
#' @return Tibble with added 'normalized_citation' column
normalize_nber_citations <- function(nber_citations) {

  if (!"raw_citation" %in% names(nber_citations)) {
    stop("NBER data must have 'raw_citation' column")
  }

  message("Normalizing NBER citations...")

  nber_citations %>%
    mutate(normalized_citation = normalize_citations(raw_citation))
}

# ------------------------------------------------------------------------------
# RUN AS SCRIPT
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  message("\n=== Text Normalization ===\n")

  # Test examples
  examples <- c(
    "Smith, J. (2020). Machine Learning. Journal, 34(2).",
    "O'Brien & Mueller (2019). AI & Law. Tech Rev."
  )

  message("Examples:")
  for (ex in examples) {
    message("  Input:  ", ex)
    message("  Output: ", normalize_citation(ex))
  }
}
