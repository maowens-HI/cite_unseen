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
source("src/02_text_normalization.R")

# ==============================================================================
# DISTANCE CALCULATION
# ==============================================================================

#' Find best match for a query in a corpus
#'
#' @param query Normalized query string
#' @param corpus Vector of normalized corpus strings
#' @param corpus_ids Optional IDs for corpus entries
#' @return List with: best_match, distance, match_id
find_best_match <- function(query, corpus, corpus_ids = NULL) {
  if (is.na(query) || nchar(query) == 0 || length(corpus) == 0) {
    return(list(best_match = NA, distance = NA, match_id = NA))
  }

  # Calculate normalized Levenshtein distances
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

# ==============================================================================
# BATCH MATCHING
# ==============================================================================

#' Match NBER citations against WOS corpus
#'
#' @param nber_citations Vector of raw NBER citation strings
#' @param wos_references Vector of raw WOS reference strings
#' @param wos_ids Optional WOS IDs
#' @param threshold Distance above which = potential hallucination
#' @return Tibble with query, best_match, distance, is_hallucination
fuzzy_match <- function(nber_citations, wos_references, wos_ids = NULL,
                        threshold = config$hallucination_threshold) {

  message("Normalizing citations...")
  queries <- normalize(nber_citations)
  corpus <- normalize(wos_references)

  message("Matching ", length(queries), " citations against ",
          length(corpus), " WOS references...")

  # Match each query
  results <- map(queries, function(q) {
    find_best_match(q, corpus, wos_ids)
  }, .progress = TRUE)

  # Build result tibble
  tibble(
    query = nber_citations,
    best_match = map_chr(results, "best_match"),
    distance = map_dbl(results, "distance"),
    match_id = map_chr(results, ~ as.character(.x$match_id)),
    is_hallucination = map_dbl(results, "distance") > threshold
  )
}

# ==============================================================================
# TEST
# ==============================================================================

if (sys.nframe() == 0) {
  message("\n=== Fuzzy Matching ===\n")

  corpus <- c(
    "Smith, J. Machine Learning in Economics",
    "Johnson, A. Deep Learning Applications"
  )

  queries <- c(
    "Smith, J. Machine Learning in Economics",
    "Smith, J. Machine Learning in Economcis",  # typo
    "Unknown Author, Fake Paper 2025"            # no match
  )

  result <- fuzzy_match(queries, corpus)
  print(result)
}
