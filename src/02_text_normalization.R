# ==============================================================================
# TEXT NORMALIZATION
# Simple text normalization for fuzzy matching
# ==============================================================================
#
# What this does:
#   "Smith, J. (2020). Machine Learning. Journal, 34(2)."
#   becomes:
#   "smithj2020machinelearningjournal342"
#
# ==============================================================================

if (!exists("config")) source("src/00_config.R")

#' Normalize citation text for matching
#'
#' Converts to lowercase and keeps only letters and numbers.
#' Works on single strings or vectors.
#'
#' @param x Citation text (string or vector)
#' @return Normalized string(s)
#'
#' @examples
#' normalize("Smith, J. (2020)")  # returns "smithj2020"
#' normalize(c("One", "Two"))     # returns c("one", "two")
normalize <- function(x) {
  if (length(x) == 0) return(character())

  result <- x %>%
    str_to_lower() %>%
    str_remove_all("[^a-z0-9]")

  # Keep NAs as NAs
  result[is.na(x) | nchar(trimws(x)) == 0] <- NA_character_

  result
}

# ==============================================================================
# RUN AS SCRIPT
# ==============================================================================

if (sys.nframe() == 0) {
  message("\n=== Text Normalization ===\n")

  examples <- c(
    "Smith, J. (2020). Machine Learning. Journal, 34(2).",
    "O'Brien & Mueller (2019). AI & Law. Tech Rev."
  )

  for (ex in examples) {
    message("Input:  ", ex)
    message("Output: ", normalize(ex), "\n")
  }
}
