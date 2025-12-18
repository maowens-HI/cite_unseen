# ==============================================================================
# SCORE ANALYSIS
# Summary statistics and visualizations of match results
# ==============================================================================
#
# After matching citations, we need to understand:
# 1. Distribution of match scores
# 2. How many potential hallucinations we found
# 3. Pre vs post treatment comparison
#
# ==============================================================================

if (!exists("config")) source("src/00_config.R")

# ------------------------------------------------------------------------------
# SUMMARY STATISTICS
# ------------------------------------------------------------------------------

#' Calculate summary statistics for match results
#'
#' @param match_results Tibble from fuzzy matching (needs: distance, is_hallucination)
#' @return List of summary statistics
summarize_matches <- function(match_results) {

  list(
    n_total = nrow(match_results),
    n_matched = sum(!is.na(match_results$distance)),

    # Distance stats
    mean_distance = mean(match_results$distance, na.rm = TRUE),
    median_distance = median(match_results$distance, na.rm = TRUE),

    # Counts by category
    n_exact = sum(match_results$distance == 0, na.rm = TRUE),
    n_close = sum(match_results$distance < 0.10, na.rm = TRUE),
    n_hallucination = sum(match_results$is_hallucination, na.rm = TRUE),

    # Rate
    hallucination_rate = mean(match_results$is_hallucination, na.rm = TRUE)
  )
}

#' Print a nice summary
print_summary <- function(stats) {
  message("\n=== Match Score Summary ===")
  message("Total: ", stats$n_total, " citations")
  message("Mean distance: ", round(stats$mean_distance, 3))
  message("Median distance: ", round(stats$median_distance, 3))
  message("Exact matches: ", stats$n_exact, " (", round(100*stats$n_exact/stats$n_total, 1), "%)")
  message("Potential hallucinations: ", stats$n_hallucination,
          " (", round(100*stats$hallucination_rate, 1), "%)")
}

# ------------------------------------------------------------------------------
# VISUALIZATION
# ------------------------------------------------------------------------------

#' Histogram of match scores
plot_score_distribution <- function(match_results, threshold = config$hallucination_threshold) {

  ggplot(match_results, aes(x = distance)) +
    geom_histogram(bins = 50, fill = "steelblue", color = "white", alpha = 0.8) +
    geom_vline(xintercept = threshold, color = "red", linetype = "dashed", size = 1) +
    labs(
      title = "Distribution of Citation Match Scores",
      subtitle = "Red line = hallucination threshold",
      x = "Normalized Levenshtein Distance",
      y = "Count"
    ) +
    theme_minimal()
}

#' Pre vs post comparison density plot
plot_pre_post <- function(match_results, paper_dates, treatment_date = config$treatment_date) {

  # Add period indicator
  data <- match_results %>%
    left_join(paper_dates, by = "paper_id") %>%
    mutate(period = ifelse(pub_date >= treatment_date, "Post-ChatGPT", "Pre-ChatGPT"))

  ggplot(data, aes(x = distance, fill = period)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("Pre-ChatGPT" = "steelblue", "Post-ChatGPT" = "coral")) +
    labs(
      title = "Match Scores: Before vs After ChatGPT",
      x = "Normalized Distance",
      y = "Density",
      fill = "Period"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

#' Monthly time series
plot_time_series <- function(match_results, paper_dates, treatment_date = config$treatment_date) {

  # Aggregate by month
  monthly <- match_results %>%
    left_join(paper_dates, by = "paper_id") %>%
    mutate(month = floor_date(pub_date, "month")) %>%
    group_by(month) %>%
    summarize(
      mean_distance = mean(distance, na.rm = TRUE),
      halluc_rate = mean(is_hallucination, na.rm = TRUE),
      n = n()
    )

  ggplot(monthly, aes(x = month, y = mean_distance)) +
    geom_line(color = "steelblue", size = 1) +
    geom_point(aes(size = n), color = "steelblue", alpha = 0.7) +
    geom_vline(xintercept = as.numeric(treatment_date), color = "red", linetype = "dashed") +
    labs(
      title = "Average Match Scores Over Time",
      x = "Month",
      y = "Mean Distance",
      size = "Citations"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# ------------------------------------------------------------------------------
# SENSITIVITY ANALYSIS
# ------------------------------------------------------------------------------

#' How does hallucination rate change with threshold?
sensitivity_by_threshold <- function(match_results, thresholds = seq(0.1, 0.5, by = 0.05)) {

  map_dfr(thresholds, function(t) {
    tibble(
      threshold = t,
      halluc_rate = mean(match_results$distance > t, na.rm = TRUE),
      n_halluc = sum(match_results$distance > t, na.rm = TRUE)
    )
  })
}

# ------------------------------------------------------------------------------
# RUN AS SCRIPT
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  message("\n=== Score Analysis ===\n")

  # Demo data
  set.seed(42)
  demo_results <- tibble(
    paper_id = rep(paste0("w", 31001:31010), each = 20),
    distance = c(rbeta(100, 2, 10), rbeta(100, 2, 8)),  # Pre vs post
    is_hallucination = NA
  ) %>%
    mutate(is_hallucination = distance > 0.30)

  stats <- summarize_matches(demo_results)
  print_summary(stats)

  message("\nSensitivity analysis:")
  print(sensitivity_by_threshold(demo_results))
}
