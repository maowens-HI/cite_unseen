# ==============================================================================
# SCORE ANALYSIS
# Descriptive statistics and visualization of match scores
# ==============================================================================

# Load configuration and dependencies
if (!exists("config")) source("src/00_config.R")

# ------------------------------------------------------------------------------
# SUMMARY STATISTICS
# ------------------------------------------------------------------------------

#' Generate summary statistics for match results
#'
#' @param match_results Tibble from fuzzy matching
#' @return List with summary statistics
summarize_match_scores <- function(match_results) {

  stats <- list(
    # Overall
    n_citations = nrow(match_results),
    n_matched = sum(!is.na(match_results$match_distance)),

    # Distance distribution
    mean_distance = mean(match_results$match_distance, na.rm = TRUE),
    median_distance = median(match_results$match_distance, na.rm = TRUE),
    sd_distance = sd(match_results$match_distance, na.rm = TRUE),

    # Percentiles
    percentiles = quantile(
      match_results$match_distance,
      probs = c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99),
      na.rm = TRUE
    ),

    # Classification counts
    exact_matches = sum(match_results$match_distance == 0, na.rm = TRUE),
    high_confidence = sum(match_results$match_distance <= 0.05, na.rm = TRUE),
    potential_hallucinations = sum(match_results$potential_hallucination, na.rm = TRUE)
  )

  # Hallucination rate
  stats$hallucination_rate <- stats$potential_hallucinations / stats$n_matched

  return(stats)
}

#' Print summary statistics
#'
#' @param stats List from summarize_match_scores
print_summary <- function(stats) {

  message("\n", strrep("=", 50))
  message("MATCH SCORE SUMMARY")
  message(strrep("=", 50))

  message("\nOverall:")
  message("  Total citations: ", format(stats$n_citations, big.mark = ","))
  message("  Successfully matched: ", format(stats$n_matched, big.mark = ","))

  message("\nDistance Distribution:")
  message("  Mean: ", round(stats$mean_distance, 4))
  message("  Median: ", round(stats$median_distance, 4))
  message("  Std Dev: ", round(stats$sd_distance, 4))

  message("\nPercentiles:")
  for (i in seq_along(stats$percentiles)) {
    message("  ", names(stats$percentiles)[i], ": ",
            round(stats$percentiles[i], 4))
  }

  message("\nClassification:")
  message("  Exact matches: ", format(stats$exact_matches, big.mark = ","),
          " (", round(100 * stats$exact_matches / stats$n_matched, 1), "%)")
  message("  High confidence: ", format(stats$high_confidence, big.mark = ","),
          " (", round(100 * stats$high_confidence / stats$n_matched, 1), "%)")
  message("  Potential hallucinations: ", format(stats$potential_hallucinations, big.mark = ","),
          " (", round(100 * stats$hallucination_rate, 1), "%)")

  message(strrep("=", 50), "\n")
}

# ------------------------------------------------------------------------------
# PRE/POST TREATMENT COMPARISON
# ------------------------------------------------------------------------------

#' Compare statistics before and after treatment date
#'
#' @param match_results Tibble with match results
#' @param paper_dates Tibble mapping paper_id to publication date
#' @param treatment_date Treatment date (default from config)
#' @return Tibble with pre/post comparison
compare_pre_post <- function(match_results,
                             paper_dates,
                             treatment_date = config$dates$treatment_date) {

  # Join with dates
  results_with_dates <- match_results %>%
    left_join(paper_dates, by = "paper_id") %>%
    mutate(
      post_treatment = pub_date >= treatment_date
    )

  # Calculate statistics by period
  comparison <- results_with_dates %>%
    group_by(post_treatment) %>%
    summarize(
      n_papers = n_distinct(paper_id),
      n_citations = n(),
      mean_distance = mean(match_distance, na.rm = TRUE),
      median_distance = median(match_distance, na.rm = TRUE),
      hallucination_rate = mean(potential_hallucination, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      period = if_else(post_treatment, "Post-ChatGPT", "Pre-ChatGPT")
    )

  return(comparison)
}

# ------------------------------------------------------------------------------
# VISUALIZATION
# ------------------------------------------------------------------------------

#' Create histogram of match score distribution
#'
#' @param match_results Tibble with match results
#' @param output_path Optional path to save figure
#' @return ggplot object
plot_score_distribution <- function(match_results,
                                    output_path = NULL) {

  p <- ggplot(match_results, aes(x = match_distance)) +
    geom_histogram(
      bins = 50,
      fill = "steelblue",
      color = "white",
      alpha = 0.8
    ) +
    geom_vline(
      xintercept = config$matching$hallucination_threshold,
      color = "red",
      linetype = "dashed",
      linewidth = 1
    ) +
    annotate(
      "text",
      x = config$matching$hallucination_threshold + 0.02,
      y = Inf,
      label = "Hallucination\nthreshold",
      hjust = 0,
      vjust = 1.5,
      color = "red",
      size = 3
    ) +
    labs(
      title = "Distribution of Citation Match Scores",
      subtitle = "Lower scores indicate better matches",
      x = "Normalized Levenshtein Distance",
      y = "Count"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  if (!is.null(output_path)) {
    ggsave(
      output_path,
      p,
      width = config$output$fig_width,
      height = config$output$fig_height,
      dpi = config$output$fig_dpi
    )
    message("Saved: ", output_path)
  }

  return(p)
}

#' Create pre/post comparison plot
#'
#' @param match_results Tibble with match results
#' @param paper_dates Tibble with publication dates
#' @param output_path Optional path to save figure
#' @return ggplot object
plot_pre_post_comparison <- function(match_results,
                                      paper_dates,
                                      output_path = NULL) {

  # Prepare data
  results_with_dates <- match_results %>%
    left_join(paper_dates, by = "paper_id") %>%
    mutate(
      period = if_else(
        pub_date >= config$dates$treatment_date,
        "Post-ChatGPT",
        "Pre-ChatGPT"
      )
    )

  p <- ggplot(results_with_dates, aes(x = match_distance, fill = period)) +
    geom_density(alpha = 0.5) +
    geom_vline(
      xintercept = config$matching$hallucination_threshold,
      color = "red",
      linetype = "dashed"
    ) +
    scale_fill_manual(values = c("Pre-ChatGPT" = "steelblue", "Post-ChatGPT" = "coral")) +
    labs(
      title = "Match Score Distribution: Pre vs Post ChatGPT",
      x = "Normalized Levenshtein Distance",
      y = "Density",
      fill = "Period"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )

  if (!is.null(output_path)) {
    ggsave(
      output_path,
      p,
      width = config$output$fig_width,
      height = config$output$fig_height,
      dpi = config$output$fig_dpi
    )
    message("Saved: ", output_path)
  }

  return(p)
}

#' Create time series of average match scores
#'
#' @param match_results Tibble with match results
#' @param paper_dates Tibble with publication dates
#' @param output_path Optional path to save figure
#' @return ggplot object
plot_time_series <- function(match_results,
                             paper_dates,
                             output_path = NULL) {

  # Aggregate by month
  monthly_scores <- match_results %>%
    left_join(paper_dates, by = "paper_id") %>%
    mutate(month = floor_date(pub_date, "month")) %>%
    group_by(month) %>%
    summarize(
      mean_score = mean(match_distance, na.rm = TRUE),
      median_score = median(match_distance, na.rm = TRUE),
      hallucination_rate = mean(potential_hallucination, na.rm = TRUE),
      n_citations = n(),
      .groups = "drop"
    )

  p <- ggplot(monthly_scores, aes(x = month, y = mean_score)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(aes(size = n_citations), color = "steelblue", alpha = 0.7) +
    geom_vline(
      xintercept = as.numeric(config$dates$treatment_date),
      color = "red",
      linetype = "dashed",
      linewidth = 1
    ) +
    annotate(
      "text",
      x = config$dates$treatment_date,
      y = max(monthly_scores$mean_score, na.rm = TRUE),
      label = "ChatGPT\nRelease",
      hjust = -0.1,
      vjust = 1,
      color = "red",
      size = 3
    ) +
    scale_size_continuous(name = "Citations") +
    labs(
      title = "Average Match Scores Over Time",
      subtitle = "Higher values suggest more potential hallucinations",
      x = "Publication Month",
      y = "Mean Normalized Distance"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )

  if (!is.null(output_path)) {
    ggsave(
      output_path,
      p,
      width = config$output$fig_width,
      height = config$output$fig_height,
      dpi = config$output$fig_dpi
    )
    message("Saved: ", output_path)
  }

  return(p)
}

# ------------------------------------------------------------------------------
# SENSITIVITY ANALYSIS
# ------------------------------------------------------------------------------

#' Calculate hallucination rates at different thresholds
#'
#' @param match_results Tibble with match results
#' @param thresholds Vector of threshold values to test
#' @return Tibble with threshold and corresponding rates
sensitivity_analysis <- function(match_results,
                                 thresholds = seq(0.1, 0.5, by = 0.05)) {

  results <- map_dfr(thresholds, function(thresh) {
    tibble(
      threshold = thresh,
      hallucination_rate = mean(match_results$match_distance > thresh, na.rm = TRUE),
      n_hallucinations = sum(match_results$match_distance > thresh, na.rm = TRUE)
    )
  })

  return(results)
}

#' Plot sensitivity analysis results
#'
#' @param sensitivity_results Output from sensitivity_analysis
#' @param output_path Optional path to save figure
#' @return ggplot object
plot_sensitivity <- function(sensitivity_results, output_path = NULL) {

  p <- ggplot(sensitivity_results, aes(x = threshold, y = hallucination_rate)) +
    geom_line(linewidth = 1.2, color = "steelblue") +
    geom_point(size = 3, color = "steelblue") +
    geom_vline(
      xintercept = config$matching$hallucination_threshold,
      color = "red",
      linetype = "dashed"
    ) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = "Sensitivity Analysis: Hallucination Rate by Threshold",
      x = "Classification Threshold",
      y = "Hallucination Rate"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))

  if (!is.null(output_path)) {
    ggsave(
      output_path,
      p,
      width = config$output$fig_width,
      height = config$output$fig_height,
      dpi = config$output$fig_dpi
    )
    message("Saved: ", output_path)
  }

  return(p)
}

# ------------------------------------------------------------------------------
# EXPORT FUNCTIONS
# ------------------------------------------------------------------------------

#' Export summary statistics to CSV
#'
#' @param stats Summary statistics list
#' @param output_path File path for CSV
export_summary_csv <- function(stats, output_path) {

  # Convert to tibble format
  summary_df <- tibble(
    metric = c(
      "Total citations",
      "Successfully matched",
      "Mean distance",
      "Median distance",
      "Std deviation",
      "Exact matches",
      "High confidence matches",
      "Potential hallucinations",
      "Hallucination rate"
    ),
    value = c(
      stats$n_citations,
      stats$n_matched,
      round(stats$mean_distance, 4),
      round(stats$median_distance, 4),
      round(stats$sd_distance, 4),
      stats$exact_matches,
      stats$high_confidence,
      stats$potential_hallucinations,
      round(stats$hallucination_rate, 4)
    )
  )

  write_csv(summary_df, output_path)
  message("Exported: ", output_path)
}

# ------------------------------------------------------------------------------
# MAIN EXECUTION (if run as script)
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  message("\n", strrep("=", 60))
  message("Score Analysis Module")
  message(strrep("=", 60), "\n")

  # Create sample data for demonstration
  set.seed(42)
  sample_results <- tibble(
    paper_id = rep(paste0("w", 31000:31019), each = 10),
    citation_num = rep(1:10, 20),
    query = paste0("citation", 1:200),
    best_match = paste0("wos_match", 1:200),
    match_distance = c(
      rbeta(100, 2, 10),  # Pre-treatment (lower scores)
      rbeta(100, 2, 8)    # Post-treatment (slightly higher)
    ),
    potential_hallucination = NA
  ) %>%
    mutate(
      potential_hallucination = match_distance > config$matching$hallucination_threshold
    )

  # Calculate and display summary
  stats <- summarize_match_scores(sample_results)
  print_summary(stats)

  # Run sensitivity analysis
  sensitivity <- sensitivity_analysis(sample_results)
  print(sensitivity)

  message("\nScore analysis module loaded successfully.")
}
