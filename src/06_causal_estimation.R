# ==============================================================================
# CAUSAL ESTIMATION
# Difference-in-Differences and Regression Discontinuity Analysis
# ==============================================================================
#
# Key question: Did hallucination rates increase after ChatGPT release?
#
# Approach:
# 1. DiD: Compare pre vs post, controlling for trends
# 2. RDD: Look at discontinuity right at treatment date
#
# Treatment date: November 30, 2022 (ChatGPT release)
#
# ==============================================================================

if (!exists("config")) source("src/00_config.R")

# ------------------------------------------------------------------------------
# DATA PREP
# ------------------------------------------------------------------------------

#' Prepare data for causal analysis
#'
#' @param match_results Match results with distance and is_hallucination
#' @param paper_dates Tibble with paper_id and pub_date
#' @return Analysis-ready tibble
prep_causal_data <- function(match_results, paper_dates,
                             treatment_date = config$treatment_date) {

  match_results %>%
    left_join(paper_dates, by = "paper_id") %>%
    filter(!is.na(pub_date)) %>%
    mutate(
      # Key treatment indicator
      post = as.integer(pub_date >= treatment_date),

      # Running variable for RDD
      days_from_treatment = as.numeric(pub_date - treatment_date),

      # Time controls
      year = year(pub_date),
      month = month(pub_date)
    )
}

# ------------------------------------------------------------------------------
# DIFFERENCE-IN-DIFFERENCES
# ------------------------------------------------------------------------------

#' Simple DiD regression
#'
#' Model: distance ~ post_treatment
#'
#' @param data Prepared analysis data
#' @return lm model
run_did <- function(data) {
  lm(distance ~ post, data = data)
}

#' DiD with time fixed effects
#'
#' Model: distance ~ post_treatment + month FE + year FE
#'
#' @param data Prepared analysis data
#' @return lm model
run_did_with_fe <- function(data) {
  lm(distance ~ post + factor(month) + factor(year), data = data)
}

# ------------------------------------------------------------------------------
# REGRESSION DISCONTINUITY
# ------------------------------------------------------------------------------

#' RDD regression around the cutoff
#'
#' Model: distance ~ post + days + post*days
#'
#' @param data Prepared analysis data
#' @param bandwidth Days around treatment to include (default 180 = 6 months)
#' @return lm model
run_rdd <- function(data, bandwidth = 180) {

  # Filter to bandwidth
  rdd_data <- data %>%
    filter(abs(days_from_treatment) <= bandwidth)

  # Linear RDD
  lm(distance ~ post + days_from_treatment + post:days_from_treatment,
     data = rdd_data)
}

#' Plot RDD
plot_rdd <- function(data, bandwidth = 180, treatment_date = config$treatment_date) {

  # Aggregate to daily means
  daily <- data %>%
    filter(abs(days_from_treatment) <= bandwidth) %>%
    group_by(days_from_treatment, post) %>%
    summarize(mean_dist = mean(distance, na.rm = TRUE), n = n(), .groups = "drop")

  ggplot(daily, aes(x = days_from_treatment, y = mean_dist)) +
    geom_point(aes(size = n), alpha = 0.5) +
    geom_smooth(aes(color = factor(post)), method = "lm", se = TRUE) +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
    scale_color_manual(values = c("0" = "steelblue", "1" = "coral"),
                       labels = c("Pre", "Post"), name = "") +
    labs(
      title = "RDD: Match Scores Around ChatGPT Release",
      x = "Days from November 30, 2022",
      y = "Mean Match Distance"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# ------------------------------------------------------------------------------
# ROBUSTNESS
# ------------------------------------------------------------------------------

#' Test different bandwidths for RDD
bandwidth_sensitivity <- function(data, bandwidths = c(30, 60, 90, 120, 180, 365)) {

  map_dfr(bandwidths, function(bw) {
    model <- run_rdd(data, bandwidth = bw)
    coefs <- summary(model)$coefficients

    tibble(
      bandwidth = bw,
      estimate = coefs["post", "Estimate"],
      se = coefs["post", "Std. Error"],
      pvalue = coefs["post", "Pr(>|t|)"],
      n = nrow(model$model)
    )
  })
}

#' Placebo tests with fake treatment dates
placebo_test <- function(data, fake_dates) {

  map_dfr(fake_dates, function(fake_date) {
    data_fake <- data %>%
      mutate(fake_post = as.integer(pub_date >= fake_date))

    model <- lm(distance ~ fake_post, data = data_fake)
    coefs <- summary(model)$coefficients

    tibble(
      fake_date = fake_date,
      estimate = coefs["fake_post", "Estimate"],
      pvalue = coefs["fake_post", "Pr(>|t|)"]
    )
  })
}

# ------------------------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------------------------

#' Nice summary of regression results
summarize_model <- function(model) {
  s <- summary(model)
  coefs <- as.data.frame(s$coefficients)

  tibble(
    term = rownames(coefs),
    estimate = coefs[,1],
    se = coefs[,2],
    pvalue = coefs[,4],
    stars = case_when(
      pvalue < 0.001 ~ "***",
      pvalue < 0.01  ~ "**",
      pvalue < 0.05  ~ "*",
      pvalue < 0.1   ~ ".",
      TRUE ~ ""
    )
  )
}

# ------------------------------------------------------------------------------
# RUN AS SCRIPT
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  message("\n=== Causal Estimation ===\n")

  # Demo data
  set.seed(42)
  treatment_date <- config$treatment_date
  dates <- seq(treatment_date - 365, treatment_date + 365, by = "day")

  demo_data <- tibble(
    paper_id = paste0("w", sample(31000:32000, 500, replace = TRUE)),
    pub_date = sample(dates, 500, replace = TRUE)
  ) %>%
    mutate(
      post = as.integer(pub_date >= treatment_date),
      days_from_treatment = as.numeric(pub_date - treatment_date),
      # Simulate small treatment effect
      distance = 0.15 + 0.02 * post + rnorm(n(), 0, 0.08)
    )

  message("DiD results:")
  did <- run_did(demo_data)
  print(summarize_model(did))

  message("\nRDD results (180-day bandwidth):")
  rdd <- run_rdd(demo_data, bandwidth = 180)
  print(summarize_model(rdd))

  message("\nBandwidth sensitivity:")
  print(bandwidth_sensitivity(demo_data))
}
