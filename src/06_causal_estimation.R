# ==============================================================================
# CAUSAL ESTIMATION
# Difference-in-Differences and Regression Discontinuity Analysis
# ==============================================================================
#
# Key question: Did hallucination rates increase after ChatGPT release?
#
# Two approaches:
# 1. DiD: Compare pre vs post, controlling for trends
# 2. RDD: Look at discontinuity right at treatment date
#
# Treatment date: November 30, 2022 (ChatGPT release)
#
# ==============================================================================

if (!exists("config")) source("src/00_config.R")

# ==============================================================================
# DATA PREPARATION
# ==============================================================================

#' Prepare data for causal analysis
#'
#' @param match_results Match results with distance
#' @param paper_dates Tibble with paper_id and pub_date
#' @return Analysis-ready tibble
prepare_data <- function(match_results, paper_dates,
                         treatment_date = config$treatment_date) {
  match_results %>%
    left_join(paper_dates, by = "paper_id") %>%
    filter(!is.na(pub_date)) %>%
    mutate(
      post = as.integer(pub_date >= treatment_date),
      days_from_treatment = as.numeric(pub_date - treatment_date),
      year = year(pub_date),
      month = month(pub_date)
    )
}

# ==============================================================================
# DIFFERENCE-IN-DIFFERENCES
# ==============================================================================

#' Simple DiD regression: distance ~ post
run_did <- function(data) {
  lm(distance ~ post, data = data)
}

#' DiD with fixed effects: distance ~ post + month FE + year FE
run_did_with_controls <- function(data) {
  lm(distance ~ post + factor(month) + factor(year), data = data)
}

# ==============================================================================
# REGRESSION DISCONTINUITY
# ==============================================================================

#' RDD regression around the cutoff
#'
#' @param data Prepared data
#' @param bandwidth Days around treatment to include (default 180 = 6 months)
run_rdd <- function(data, bandwidth = 180) {
  rdd_data <- filter(data, abs(days_from_treatment) <= bandwidth)
  lm(distance ~ post + days_from_treatment + post:days_from_treatment,
     data = rdd_data)
}

#' Plot RDD
plot_rdd <- function(data, bandwidth = 180) {
  daily <- data %>%
    filter(abs(days_from_treatment) <= bandwidth) %>%
    group_by(days_from_treatment, post) %>%
    summarize(mean_dist = mean(distance, na.rm = TRUE), n = n(), .groups = "drop")

  ggplot(daily, aes(x = days_from_treatment, y = mean_dist)) +
    geom_point(aes(size = n), alpha = 0.5) +
    geom_smooth(aes(color = factor(post)), method = "lm") +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
    scale_color_manual(values = c("0" = "steelblue", "1" = "coral"),
                       labels = c("Pre", "Post")) +
    labs(title = "RDD: Match Scores Around ChatGPT Release",
         x = "Days from Nov 30, 2022", y = "Mean Distance") +
    theme_minimal()
}

# ==============================================================================
# ROBUSTNESS CHECKS
# ==============================================================================

#' Test different bandwidths
bandwidth_sensitivity <- function(data, bandwidths = c(30, 60, 90, 120, 180)) {
  map_dfr(bandwidths, function(bw) {
    model <- run_rdd(data, bandwidth = bw)
    coefs <- summary(model)$coefficients
    tibble(
      bandwidth = bw,
      estimate = coefs["post", "Estimate"],
      se = coefs["post", "Std. Error"],
      pvalue = coefs["post", "Pr(>|t|)"]
    )
  })
}

#' Placebo test with fake treatment dates
placebo_test <- function(data, fake_dates) {
  map_dfr(fake_dates, function(fake_date) {
    data_fake <- mutate(data, fake_post = as.integer(pub_date >= fake_date))
    model <- lm(distance ~ fake_post, data = data_fake)
    coefs <- summary(model)$coefficients
    tibble(
      fake_date = fake_date,
      estimate = coefs["fake_post", "Estimate"],
      pvalue = coefs["fake_post", "Pr(>|t|)"]
    )
  })
}

# ==============================================================================
# HELPERS
# ==============================================================================

#' Extract model coefficients as tibble
model_summary <- function(model) {
  coefs <- as.data.frame(summary(model)$coefficients)
  tibble(
    term = rownames(coefs),
    estimate = coefs[, 1],
    se = coefs[, 2],
    pvalue = coefs[, 4]
  )
}

# ==============================================================================
# TEST
# ==============================================================================

if (sys.nframe() == 0) {
  message("\n=== Causal Estimation ===\n")

  # Demo data
  set.seed(42)
  treatment_date <- config$treatment_date
  dates <- seq(treatment_date - 365, treatment_date + 365, by = "day")

  demo <- tibble(
    paper_id = paste0("w", sample(31000:32000, 500, replace = TRUE)),
    pub_date = sample(dates, 500, replace = TRUE)
  ) %>%
    mutate(
      post = as.integer(pub_date >= treatment_date),
      days_from_treatment = as.numeric(pub_date - treatment_date),
      distance = 0.15 + 0.02 * post + rnorm(n(), 0, 0.08)
    )

  message("DiD results:")
  print(model_summary(run_did(demo)))

  message("\nRDD results (180-day bandwidth):")
  print(model_summary(run_rdd(demo, 180)))
}
