# ==============================================================================
# FUZZY MATCHING
# Match NBER citations against WOS corpus using Levenshtein distance
# ==============================================================================
#
# The idea: For each NBER citation, find the closest WOS reference.
# If the distance is too large, the citation might be hallucinated.
#
# Distance = 0 means identical
# Distance > threshold (e.g., 0.30) suggests possible hallucination
#
# ==============================================================================

if (!exists("config")) source("src/00_config.R")

# ------------------------------------------------------------------------------
# DISTANCE FUNCTIONS
# ------------------------------------------------------------------------------

#' Calculate normalized Levenshtein distance between two strings
#'
#' @param a First string (already normalized)
#' @param b Second string (already normalized)
#' @return Distance in range [0, 1] where 0 = identical
normalized_distance <- function(a, b) {

  if (is.na(a) || is.na(b)) return(NA_real_)
  if (a == b) return(0)
  if (a == "" || b == "") return(1)

  # Raw Levenshtein distance
  raw_dist <- stringdist(a, b, method = "lv")

  # Normalize by max length
  max_len <- max(nchar(a), nchar(b))

  return(raw_dist / max_len)
}

#' Find the best match for a query in a corpus
#'
#' @param query Normalized query string
#' @param corpus Vector of normalized corpus strings
#' @param corpus_ids Optional IDs for corpus entries
#' @return List with: best_match, distance, match_id
find_best_match <- function(query, corpus, corpus_ids = NULL) {

  if (is.na(query) || nchar(query) == 0 || length(corpus) == 0) {
    return(list(best_match = NA, distance = NA, match_id = NA))
  }

  # Calculate all distances (vectorized for speed)
  raw_dists <- stringdist(query, corpus, method = "lv")
  max_lens <- pmax(nchar(query), nchar(corpus))
  distances <- raw_dists / max_lens

  # Find minimum
  best_idx <- which.min(distances)

  list(
    best_match = corpus[best_idx],
    distance = distances[best_idx],
    match_id = if (!is.null(corpus_ids)) corpus_ids[best_idx] else NA
  )
}

# ------------------------------------------------------------------------------
# BATCH MATCHING
# ------------------------------------------------------------------------------

#' Match multiple NBER citations against WOS corpus
#'
#' @param queries Vector of normalized NBER citations
#' @param corpus Vector of normalized WOS references
#' @param corpus_ids Optional WOS identifiers
#' @param threshold Distance above which = potential hallucination
#' @return Tibble with query, best_match, distance, is_hallucination
fuzzy_match <- function(queries, corpus, corpus_ids = NULL,
                        threshold = config$hallucination_threshold) {

  message("Matching ", length(queries), " citations against ",
          length(corpus), " WOS references...")

  # Match each query
  results <- map(queries, function(q) {
    find_best_match(q, corpus, corpus_ids)
  }, .progress = TRUE)

  # Build result tibble
  result_df <- tibble(
    query = queries,
    best_match = map_chr(results, "best_match"),
    distance = map_dbl(results, "distance"),
    match_id = map_chr(results, "match_id"),
    is_hallucination = map_dbl(results, "distance") > threshold
  )

  # Report summary
  n_exact <- sum(result_df$distance == 0, na.rm = TRUE)
  n_close <- sum(result_df$distance < 0.10, na.rm = TRUE)
  n_halluc <- sum(result_df$is_hallucination, na.rm = TRUE)

  message("\nResults:")
  message("  Exact matches: ", n_exact)
  message("  Close matches (d < 0.10): ", n_close)
  message("  Potential hallucinations (d > ", threshold, "): ", n_halluc)

  return(result_df)
}

# ------------------------------------------------------------------------------
# CONVENIENCE FUNCTION
# ------------------------------------------------------------------------------

#' Run full matching pipeline on normalized data
#'
#' @param nber_normalized Tibble with normalized_citation column
#' @param wos_normalized Tibble with normalized_reference column
#' @return Tibble with match results
run_matching <- function(nber_normalized, wos_normalized) {

  # Get the normalized strings
  queries <- nber_normalized$normalized_citation
  corpus <- wos_normalized$normalized_reference
  corpus_ids <- wos_normalized$wos_id

  # Run matching
  matches <- fuzzy_match(queries, corpus, corpus_ids)

  # Combine with original NBER data
  result <- bind_cols(
    nber_normalized %>% select(-any_of("normalized_citation")),
    matches
  )

  return(result)
}

# ------------------------------------------------------------------------------
# RUN AS SCRIPT
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  message("\n=== Fuzzy Matching ===\n")

  # Demo
  corpus <- c(
    "smithjmachinelearningineconomics",
    "johnsondeeplearniningapplications"
  )

  queries <- c(
    "smithjmachinelearningineconomics",  # exact
    "smithjmachinelearningineconomcis",  # typo
    "unknownauthorfakepaper2025"         # no match
  )

  result <- fuzzy_match(queries, corpus)
  print(result)
}
