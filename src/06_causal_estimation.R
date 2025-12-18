# ==============================================================================
# CAUSAL ESTIMATION
# Difference-in-differences and regression discontinuity analysis
# ==============================================================================

# Load configuration and dependencies
if (!exists("config")) source("src/00_config.R")

# ------------------------------------------------------------------------------
# DATA PREPARATION FOR CAUSAL ANALYSIS
# ------------------------------------------------------------------------------

#' Prepare data for causal analysis
#'
#' @param match_results Tibble with match results
#' @param paper_dates Tibble mapping paper_id to publication date
#' @param treatment_date Date of treatment (ChatGPT release)
#' @return Tibble ready for regression analysis
prepare_causal_data <- function(match_results,
                                paper_dates,
                                treatment_date = config$dates$treatment_date) {

  # Join and create treatment indicators
  analysis_data <- match_results %>%
    left_join(paper_dates, by = "paper_id") %>%
    filter(!is.na(pub_date)) %>%
    mutate(
      # Treatment indicator
      post_treatment = as.integer(pub_date >= treatment_date),

      # Time variables for RDD
      days_from_treatment = as.numeric(pub_date - treatment_date),
      weeks_from_treatment = days_from_treatment / 7,
      months_from_treatment = days_from_treatment / 30.44,

      # Calendar controls
      year = year(pub_date),
      month = month(pub_date),
      quarter = quarter(pub_date),

      # Paper-level aggregates (if needed)
      year_month = floor_date(pub_date, "month")
    )

  return(analysis_data)
}

#' Aggregate data to paper level
#'
#' @param analysis_data Citation-level analysis data
#' @return Paper-level tibble
aggregate_to_paper_level <- function(analysis_data) {

  paper_data <- analysis_data %>%
    group_by(paper_id, pub_date, post_treatment, days_from_treatment,
             year, month, quarter) %>%
    summarize(
      n_citations = n(),
      mean_distance = mean(match_distance, na.rm = TRUE),
      median_distance = median(match_distance, na.rm = TRUE),
      max_distance = max(match_distance, na.rm = TRUE),
      n_hallucinations = sum(potential_hallucination, na.rm = TRUE),
      hallucination_rate = mean(potential_hallucination, na.rm = TRUE),
      .groups = "drop"
    )

  return(paper_data)
}

# ------------------------------------------------------------------------------
# DIFFERENCE-IN-DIFFERENCES
# ------------------------------------------------------------------------------

#' Run simple DiD regression
#'
#' @param analysis_data Prepared analysis data
#' @param outcome Outcome variable name (default: "match_distance")
#' @param controls Vector of control variable names
#' @return lm model object
run_did_regression <- function(analysis_data,
                               outcome = "match_distance",
                               controls = NULL) {

  # Build formula
  base_formula <- paste(outcome, "~ post_treatment")

  if (!is.null(controls) && length(controls) > 0) {
    control_terms <- paste(controls, collapse = " + ")
    formula_str <- paste(base_formula, "+", control_terms)
  } else {
    formula_str <- base_formula
  }

  formula_obj <- as.formula(formula_str)

  # Run regression
  model <- lm(formula_obj, data = analysis_data)

  return(model)
}

#' Run DiD with time fixed effects
#'
#' @param analysis_data Prepared analysis data
#' @param outcome Outcome variable name
#' @return lm model object
run_did_with_fe <- function(analysis_data, outcome = "match_distance") {

  formula_obj <- as.formula(
    paste(outcome, "~ post_treatment + factor(month) + factor(year)")
  )

  model <- lm(formula_obj, data = analysis_data)

  return(model)
}

#' Summarize regression results
#'
#' @param model lm model object
#' @return Tibble with coefficient summary
summarize_regression <- function(model) {

  summary_obj <- summary(model)
  coef_table <- as.data.frame(summary_obj$coefficients)

  tibble(
    term = rownames(coef_table),
    estimate = coef_table[, 1],
    std_error = coef_table[, 2],
    t_statistic = coef_table[, 3],
    p_value = coef_table[, 4],
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      p_value < 0.1 ~ ".",
      TRUE ~ ""
    )
  ) %>%
    mutate(
      ci_lower = estimate - 1.96 * std_error,
      ci_upper = estimate + 1.96 * std_error
    )
}

# ------------------------------------------------------------------------------
# REGRESSION DISCONTINUITY
# ------------------------------------------------------------------------------

#' Run RDD analysis
#'
#' @param analysis_data Prepared analysis data
#' @param outcome Outcome variable name
#' @param bandwidth Bandwidth in days around treatment date
#' @param polynomial_order Order of polynomial for running variable
#' @return lm model object
run_rdd <- function(analysis_data,
                    outcome = "match_distance",
                    bandwidth = 180,  # 6 months
                    polynomial_order = 1) {

  # Filter to bandwidth
  rdd_data <- analysis_data %>%
    filter(abs(days_from_treatment) <= bandwidth)

  # Build polynomial terms
  if (polynomial_order == 1) {
    formula_str <- paste(
      outcome,
      "~ post_treatment + days_from_treatment + post_treatment:days_from_treatment"
    )
  } else if (polynomial_order == 2) {
    formula_str <- paste(
      outcome,
      "~ post_treatment + days_from_treatment + I(days_from_treatment^2) +",
      "post_treatment:days_from_treatment + post_treatment:I(days_from_treatment^2)"
    )
  } else {
    stop("Only polynomial orders 1 and 2 are implemented")
  }

  model <- lm(as.formula(formula_str), data = rdd_data)

  return(model)
}

#' Plot RDD visualization
#'
#' @param analysis_data Prepared analysis data
#' @param bandwidth Bandwidth in days
#' @param output_path Optional path to save figure
#' @return ggplot object
plot_rdd <- function(analysis_data,
                     bandwidth = 180,
                     output_path = NULL) {

  # Aggregate to daily averages
  daily_data <- analysis_data %>%
    filter(abs(days_from_treatment) <= bandwidth) %>%
    group_by(days_from_treatment, post_treatment) %>%
    summarize(
      mean_distance = mean(match_distance, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )

  p <- ggplot(daily_data, aes(x = days_from_treatment, y = mean_distance)) +
    geom_point(aes(size = n), alpha = 0.5) +
    geom_smooth(
      aes(color = factor(post_treatment)),
      method = "lm",
      se = TRUE
    ) +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
    scale_color_manual(
      values = c("0" = "steelblue", "1" = "coral"),
      labels = c("Pre-ChatGPT", "Post-ChatGPT"),
      name = "Period"
    ) +
    scale_size_continuous(name = "Citations") +
    labs(
      title = "Regression Discontinuity: Match Scores Around ChatGPT Release",
      x = "Days from Treatment (Nov 30, 2022)",
      y = "Mean Match Distance"
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
# ROBUSTNESS CHECKS
# ------------------------------------------------------------------------------

#' Run placebo tests with fake treatment dates
#'
#' @param analysis_data Prepared analysis data
#' @param placebo_dates Vector of fake treatment dates
#' @param outcome Outcome variable
#' @return Tibble with placebo test results
run_placebo_tests <- function(analysis_data,
                              placebo_dates,
                              outcome = "match_distance") {

  results <- map_dfr(placebo_dates, function(fake_date) {

    # Create fake treatment indicator
    placebo_data <- analysis_data %>%
      mutate(fake_post = as.integer(pub_date >= fake_date))

    # Run regression
    model <- lm(as.formula(paste(outcome, "~ fake_post")), data = placebo_data)
    coef_summary <- summary(model)$coefficients

    tibble(
      placebo_date = fake_date,
      estimate = coef_summary["fake_post", "Estimate"],
      std_error = coef_summary["fake_post", "Std. Error"],
      p_value = coef_summary["fake_post", "Pr(>|t|)"]
    )
  })

  return(results)
}

#' Run bandwidth sensitivity analysis
#'
#' @param analysis_data Prepared analysis data
#' @param bandwidths Vector of bandwidth values (in days)
#' @param outcome Outcome variable
#' @return Tibble with results at each bandwidth
bandwidth_sensitivity <- function(analysis_data,
                                  bandwidths = c(30, 60, 90, 120, 180, 365),
                                  outcome = "match_distance") {

  results <- map_dfr(bandwidths, function(bw) {

    model <- run_rdd(analysis_data, outcome = outcome, bandwidth = bw)
    coef_summary <- summary(model)$coefficients

    tibble(
      bandwidth_days = bw,
      estimate = coef_summary["post_treatment", "Estimate"],
      std_error = coef_summary["post_treatment", "Std. Error"],
      p_value = coef_summary["post_treatment", "Pr(>|t|)"],
      n_obs = nrow(model$model)
    )
  })

  return(results)
}

# ------------------------------------------------------------------------------
# EXPORT RESULTS
# ------------------------------------------------------------------------------

#' Export regression results to LaTeX table
#'
#' @param models List of lm model objects
#' @param output_path File path for .tex output
#' @param model_names Names for each model column
export_regression_latex <- function(models,
                                    output_path,
                                    model_names = NULL) {

  if (is.null(model_names)) {
    model_names <- paste0("Model ", seq_along(models))
  }

  # Build coefficient table manually
  all_terms <- unique(unlist(lapply(models, function(m) names(coef(m)))))

  coef_matrix <- matrix(
    NA,
    nrow = length(all_terms) * 2,  # coefficient and SE rows
    ncol = length(models)
  )

  for (j in seq_along(models)) {
    summary_j <- summary(models[[j]])
    coefs <- summary_j$coefficients

    for (i in seq_along(all_terms)) {
      term <- all_terms[i]
      if (term %in% rownames(coefs)) {
        coef_matrix[(i-1)*2 + 1, j] <- round(coefs[term, "Estimate"], 4)
        coef_matrix[(i-1)*2 + 2, j] <- round(coefs[term, "Std. Error"], 4)
      }
    }
  }

  # Create LaTeX output
  latex_lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\caption{Regression Results}",
    paste0("\\begin{tabular}{l", paste(rep("c", length(models)), collapse = ""), "}"),
    "\\toprule",
    paste(c("", model_names), collapse = " & ") %>% paste0(" \\\\"),
    "\\midrule"
  )

  for (i in seq_along(all_terms)) {
    coef_row <- coef_matrix[(i-1)*2 + 1, ]
    se_row <- coef_matrix[(i-1)*2 + 2, ]

    coef_str <- paste(
      ifelse(is.na(coef_row), "", sprintf("%.4f", coef_row)),
      collapse = " & "
    )
    se_str <- paste(
      ifelse(is.na(se_row), "", sprintf("(%.4f)", se_row)),
      collapse = " & "
    )

    latex_lines <- c(
      latex_lines,
      paste(all_terms[i], "&", coef_str, "\\\\"),
      paste("", "&", se_str, "\\\\")
    )
  }

  latex_lines <- c(
    latex_lines,
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}"
  )

  writeLines(latex_lines, output_path)
  message("Exported: ", output_path)
}

#' Export results summary to CSV
#'
#' @param results_list List of analysis results
#' @param output_path File path for CSV
export_results_csv <- function(results_list, output_path) {

  write_csv(results_list, output_path)
  message("Exported: ", output_path)
}

# ------------------------------------------------------------------------------
# MAIN EXECUTION (if run as script)
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  message("\n", strrep("=", 60))
  message("Causal Estimation Module")
  message(strrep("=", 60), "\n")

  # Create sample data for demonstration
  set.seed(42)

  # Generate fake dates around treatment
  treatment_date <- config$dates$treatment_date
  fake_dates <- seq(
    treatment_date - 365,
    treatment_date + 365,
    by = "day"
  )

  sample_data <- tibble(
    paper_id = paste0("w", sample(31000:32000, 1000, replace = TRUE)),
    pub_date = sample(fake_dates, 1000, replace = TRUE),
    match_distance = NA_real_
  ) %>%
    mutate(
      post_treatment = as.integer(pub_date >= treatment_date),
      days_from_treatment = as.numeric(pub_date - treatment_date),
      # Simulate effect: slightly higher scores post-treatment
      match_distance = 0.15 + 0.02 * post_treatment +
                       rnorm(n(), 0, 0.08),
      potential_hallucination = match_distance > 0.3
    )

  # Run DiD
  message("Running Difference-in-Differences analysis...")
  did_model <- run_did_regression(sample_data)
  message("\nDiD Results:")
  print(summarize_regression(did_model))

  # Run RDD
  message("\nRunning Regression Discontinuity analysis...")
  rdd_model <- run_rdd(sample_data, bandwidth = 180)
  message("\nRDD Results:")
  print(summarize_regression(rdd_model))

  message("\nCausal estimation module loaded successfully.")
}
