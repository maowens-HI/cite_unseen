# ==============================================================================
# FUZZY MATCHING
# Calculate Levenshtein distances between NBER citations and WOS corpus
# ==============================================================================

# Load configuration and dependencies
if (!exists("config")) source("src/00_config.R")

# ------------------------------------------------------------------------------
# DISTANCE CALCULATION
# ------------------------------------------------------------------------------

#' Calculate normalized Levenshtein distance between two strings
#'
#' @param a First string (normalized)
#' @param b Second string (normalized)
#' @param method Distance method: "lv" (Levenshtein), "dl" (Damerau-Levenshtein)
#' @return Numeric distance in range [0, 1] where 0 = identical
#'
#' @examples
#' normalized_distance("machinelearning", "machinelearninng")
#' # Returns small value ~0.06
normalized_distance <- function(a, b, method = config$matching$method) {

  # Handle edge cases
  if (is.na(a) || is.na(b)) return(NA_real_)
  if (a == "" && b == "") return(0)
  if (a == "" || b == "") return(1)
  if (a == b) return(0)

  # Calculate raw distance
  raw_dist <- stringdist(a, b, method = method)

  # Normalize by maximum length
  max_len <- max(nchar(a), nchar(b))
  normalized <- raw_dist / max_len

  return(normalized)
}

#' Calculate distances between one query and multiple corpus entries
#'
#' @param query Single normalized query string
#' @param corpus Vector of normalized corpus strings
#' @param method Distance method
#' @return Numeric vector of distances
calculate_distances <- function(query, corpus, method = config$matching$method) {

  if (is.na(query) || length(corpus) == 0) {
    return(rep(NA_real_, length(corpus)))
  }

  # Use vectorized stringdist for efficiency
  raw_distances <- stringdist(query, corpus, method = method)

  # Normalize each distance
  max_lens <- pmax(nchar(query), nchar(corpus))
  normalized <- raw_distances / max_lens

  return(normalized)
}

# ------------------------------------------------------------------------------
# BEST MATCH FINDING
# ------------------------------------------------------------------------------

#' Find the best matching WOS entry for a single NBER citation
#'
#' @param query Normalized NBER citation
#' @param corpus Vector of normalized WOS references
#' @param corpus_ids Optional vector of WOS identifiers
#' @param method Distance method
#' @param return_top_n Return top N matches instead of just best
#' @return List with best_match, distance, and match_id
#'
#' @examples
#' find_best_match("smithjmachinelearning", wos_normalized)
find_best_match <- function(query,
                            corpus,
                            corpus_ids = NULL,
                            method = config$matching$method,
                            return_top_n = 1) {

  # Handle edge cases
  if (is.na(query) || nchar(query) == 0) {
    return(list(
      best_match = NA_character_,
      distance = NA_real_,
      match_id = NA_character_
    ))
  }

  if (length(corpus) == 0) {
    return(list(
      best_match = NA_character_,
      distance = NA_real_,
      match_id = NA_character_
    ))
  }

  # Calculate all distances
  distances <- calculate_distances(query, corpus, method)

  # Find best match(es)
  if (return_top_n == 1) {
    best_idx <- which.min(distances)

    return(list(
      best_match = corpus[best_idx],
      distance = distances[best_idx],
      match_id = if (!is.null(corpus_ids)) corpus_ids[best_idx] else NA_character_
    ))
  } else {
    # Return top N matches
    top_indices <- order(distances)[1:min(return_top_n, length(distances))]

    return(list(
      matches = corpus[top_indices],
      distances = distances[top_indices],
      match_ids = if (!is.null(corpus_ids)) corpus_ids[top_indices] else rep(NA, length(top_indices))
    ))
  }
}

# ------------------------------------------------------------------------------
# BATCH PROCESSING
# ------------------------------------------------------------------------------

#' Match multiple NBER citations against WOS corpus
#'
#' @param queries Vector of normalized NBER citations
#' @param corpus Vector of normalized WOS references
#' @param corpus_ids Optional WOS identifiers
#' @param method Distance method
#' @param parallel Use parallel processing?
#' @param n_workers Number of parallel workers (NULL = auto)
#' @param progress Show progress bar?
#' @return Tibble with query, best_match, distance columns
batch_fuzzy_match <- function(queries,
                              corpus,
                              corpus_ids = NULL,
                              method = config$matching$method,
                              parallel = TRUE,
                              n_workers = config$processing$n_workers,
                              progress = TRUE) {

  n_queries <- length(queries)
  n_corpus <- length(corpus)
  n_comparisons <- n_queries * n_corpus

  message("Fuzzy matching ", n_queries, " queries against ",
          n_corpus, " corpus entries")
  message("Total comparisons: ", format(n_comparisons, big.mark = ","))

  # Decide on processing strategy
  use_parallel <- parallel &&
                  n_comparisons > 1e6 &&
                  requireNamespace("furrr", quietly = TRUE)

  if (use_parallel) {
    # Set up parallel processing
    if (is.null(n_workers)) {
      n_workers <- max(1, parallel::detectCores() - 1)
    }

    message("Using ", n_workers, " parallel workers")
    future::plan(future::multisession, workers = n_workers)

    # Parallel mapping
    results <- furrr::future_map(
      queries,
      function(q) find_best_match(q, corpus, corpus_ids, method),
      .progress = progress,
      .options = furrr::furrr_options(seed = TRUE)
    )

    # Reset to sequential
    future::plan(future::sequential)

  } else {
    # Sequential processing
    results <- map(
      queries,
      function(q) find_best_match(q, corpus, corpus_ids, method),
      .progress = progress
    )
  }

  # Convert to tibble
  result_df <- tibble(
    query = queries,
    best_match = map_chr(results, "best_match"),
    match_distance = map_dbl(results, "distance"),
    match_id = map_chr(results, "match_id")
  )

  # Add hallucination classification
  threshold <- config$matching$hallucination_threshold
  result_df <- result_df %>%
    mutate(
      potential_hallucination = match_distance > threshold
    )

  # Report summary
  message("\nMatching complete:")
  message("  Exact matches (d=0): ", sum(result_df$match_distance == 0, na.rm = TRUE))
  message("  High confidence (d<0.05): ",
          sum(result_df$match_distance < 0.05, na.rm = TRUE))
  message("  Potential hallucinations (d>", threshold, "): ",
          sum(result_df$potential_hallucination, na.rm = TRUE))

  return(result_df)
}

# ------------------------------------------------------------------------------
# HALLUCINATION CLASSIFICATION
# ------------------------------------------------------------------------------

#' Classify match scores into categories
#'
#' @param distance Numeric distance score
#' @return Factor with classification
classify_match <- function(distance) {

  thresholds <- config$matching$thresholds

  case_when(
    is.na(distance) ~ "unknown",
    distance <= thresholds$exact_match ~ "exact",
    distance <= thresholds$high_confidence ~ "high_confidence",
    distance <= thresholds$medium_confidence ~ "medium_confidence",
    distance <= thresholds$low_confidence ~ "low_confidence",
    TRUE ~ "no_match"
  ) %>%
    factor(levels = c("exact", "high_confidence", "medium_confidence",
                      "low_confidence", "no_match", "unknown"))
}

#' Add match classification to results
#'
#' @param match_results Tibble from batch_fuzzy_match
#' @return Tibble with added match_class column
add_match_classification <- function(match_results) {

  match_results %>%
    mutate(
      match_class = classify_match(match_distance)
    )
}

# ------------------------------------------------------------------------------
# CHECKPOINTING FOR LONG JOBS
# ------------------------------------------------------------------------------

#' Batch match with checkpointing for large datasets
#'
#' @param queries Vector of normalized queries
#' @param corpus Vector of normalized corpus
#' @param batch_size Number of queries per batch
#' @param checkpoint_dir Directory to save checkpoints
#' @param resume Resume from existing checkpoints?
#' @return Tibble with all match results
batch_fuzzy_match_with_checkpoints <- function(queries,
                                                corpus,
                                                batch_size = config$processing$batch_size,
                                                checkpoint_dir = config$paths$intermediate,
                                                resume = TRUE) {

  n_queries <- length(queries)
  n_batches <- ceiling(n_queries / batch_size)

  message("Processing ", n_queries, " queries in ", n_batches, " batches")

  # Check for existing checkpoints
  existing_checkpoints <- list.files(
    checkpoint_dir,
    pattern = "checkpoint_batch_\\d+\\.rds",
    full.names = TRUE
  )

  if (resume && length(existing_checkpoints) > 0) {
    message("Found ", length(existing_checkpoints), " existing checkpoints")
    completed_batches <- as.integer(
      str_extract(basename(existing_checkpoints), "\\d+")
    )
    start_batch <- max(completed_batches) + 1
  } else {
    start_batch <- 1
    completed_batches <- integer()
  }

  # Process remaining batches
  for (batch_num in start_batch:n_batches) {

    # Get batch indices
    start_idx <- (batch_num - 1) * batch_size + 1
    end_idx <- min(batch_num * batch_size, n_queries)

    message("\nBatch ", batch_num, "/", n_batches,
            " (queries ", start_idx, "-", end_idx, ")")

    # Process batch
    batch_queries <- queries[start_idx:end_idx]
    batch_results <- batch_fuzzy_match(
      batch_queries,
      corpus,
      parallel = TRUE,
      progress = TRUE
    )

    # Add original indices
    batch_results$query_idx <- start_idx:end_idx

    # Save checkpoint
    checkpoint_file <- file.path(
      checkpoint_dir,
      sprintf("checkpoint_batch_%04d.rds", batch_num)
    )
    saveRDS(batch_results, checkpoint_file)
    message("Saved checkpoint: ", basename(checkpoint_file))
  }

  # Combine all results
  message("\nCombining all batches...")
  all_checkpoints <- list.files(
    checkpoint_dir,
    pattern = "checkpoint_batch_\\d+\\.rds",
    full.names = TRUE
  )

  results <- map(all_checkpoints, readRDS) %>%
    bind_rows() %>%
    arrange(query_idx)

  return(results)
}

# ------------------------------------------------------------------------------
# MAIN EXECUTION (if run as script)
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  message("\n", strrep("=", 60))
  message("Fuzzy Matching Module")
  message(strrep("=", 60), "\n")

  # Demo with sample data
  sample_corpus <- c(
    "smithjmachinelearningineconomics",
    "johnsondeeplearniningapplications",
    "brownartificialintelligencelaw",
    "williamsdatascience2020"
  )

  sample_queries <- c(
    "smithjmachinelearningineconomics",  # Exact match
    "smithjmachinelearningineconomcis",  # Typo
    "unknownauthorfakepaper2025"          # No match
  )

  # Test single match
  result <- find_best_match(sample_queries[2], sample_corpus)
  message("\nSingle match test:")
  message("  Query: ", sample_queries[2])
  message("  Best match: ", result$best_match)
  message("  Distance: ", round(result$distance, 4))

  # Test batch matching
  message("\nBatch match test:")
  batch_results <- batch_fuzzy_match(
    sample_queries,
    sample_corpus,
    parallel = FALSE,
    progress = FALSE
  )
  print(batch_results)

  message("\nFuzzy matching module loaded successfully.")
}
