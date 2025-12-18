# ==============================================================================
# SCORE ANALYSIS
# Summary statistics and visualizations of match results
# ==============================================================================
#
# After matching citations, we want to understand:
# 1. Distribution of match scores
# 2. How many potential hallucinations we found
# 3. Pre vs post treatment comparison
#
# ==============================================================================

if (!exists("config")) source("src/00_config.R")

# ==============================================================================
# SUMMARY STATISTICS
# ==============================================================================

#' Calculate summary statistics
#'
#' @param match_results Tibble from fuzzy matching
#' @return Named list of statistics
summarize_matches <- function(match_results) {
  list(
    n_total = nrow(match_results),
    mean_distance = mean(match_results$distance, na.rm = TRUE),
    median_distance = median(match_results$distance, na.rm = TRUE),
    n_exact = sum(match_results$distance == 0, na.rm = TRUE),
    n_hallucination = sum(match_results$is_hallucination, na.rm = TRUE),
    hallucination_rate = mean(match_results$is_hallucination, na.rm = TRUE)
  )
}

#' Print summary
print_summary <- function(stats) {
  message("\n=== Match Summary ===")
  message("Total: ", stats$n_total, " citations")
  message("Mean distance: ", round(stats$mean_distance, 3))
  message("Exact matches: ", stats$n_exact)
  message("Potential hallucinations: ", stats$n_hallucination,
          " (", round(100 * stats$hallucination_rate, 1), "%)")
}

# ==============================================================================
# VISUALIZATION
# ==============================================================================

#' Plot score distribution
plot_scores <- function(match_results, threshold = config$hallucination_threshold) {
  ggplot(match_results, aes(x = distance)) +
    geom_histogram(bins = 50, fill = "steelblue", color = "white") +
    geom_vline(xintercept = threshold, color = "red", linetype = "dashed") +
    labs(title = "Match Score Distribution",
         subtitle = "Red line = hallucination threshold",
         x = "Distance", y = "Count") +
    theme_minimal()
}

#' Plot pre vs post comparison
plot_pre_post <- function(match_results, paper_dates,
                          treatment_date = config$treatment_date) {
  data <- match_results %>%
    left_join(paper_dates, by = "paper_id") %>%
    mutate(period = ifelse(pub_date >= treatment_date, "Post", "Pre"))

  ggplot(data, aes(x = distance, fill = period)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("Pre" = "steelblue", "Post" = "coral")) +
    labs(title = "Match Scores: Before vs After ChatGPT",
         x = "Distance", y = "Density") +
    theme_minimal()
}

#' Plot monthly time series
plot_monthly <- function(match_results, paper_dates,
                         treatment_date = config$treatment_date) {
  monthly <- match_results %>%
    left_join(paper_dates, by = "paper_id") %>%
    mutate(month = floor_date(pub_date, "month")) %>%
    group_by(month) %>%
    summarize(mean_distance = mean(distance, na.rm = TRUE), n = n())

  ggplot(monthly, aes(x = month, y = mean_distance)) +
    geom_line(color = "steelblue") +
    geom_point(aes(size = n), color = "steelblue") +
    geom_vline(xintercept = as.numeric(treatment_date), color = "red",
               linetype = "dashed") +
    labs(title = "Average Match Scores Over Time",
         x = "Month", y = "Mean Distance") +
    theme_minimal()
}

# ==============================================================================
# TEST
# ==============================================================================

if (sys.nframe() == 0) {
  message("\n=== Score Analysis ===\n")

  # Demo data
  set.seed(42)
  demo <- tibble(
    paper_id = rep(paste0("w", 31001:31010), each = 20),
    distance = rbeta(200, 2, 10),
    is_hallucination = NA
  ) %>%
    mutate(is_hallucination = distance > 0.30)

  stats <- summarize_matches(demo)
  print_summary(stats)
}
