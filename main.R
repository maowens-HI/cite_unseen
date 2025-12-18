# ==============================================================================
# CITE UNSEEN - MAIN ANALYSIS SCRIPT
# Detecting LLM-Hallucinated References in NBER Papers
# ==============================================================================
#
# This script runs the full analysis pipeline:
#   1. Load data (WOS corpus + NBER papers)
#   2. Extract and normalize citations
#   3. Match NBER citations against WOS
#   4. Analyze results
#   5. Run causal estimation (DiD / RDD)
#
# To run: source("main.R") or Rscript main.R
#
# ==============================================================================

message("\n========================================")
message("CITE UNSEEN ANALYSIS")
message("========================================\n")

# ==============================================================================
# SETUP
# ==============================================================================

message("Step 0: Loading configuration and packages...\n")
source("src/00_config.R")
source("src/01_data_ingestion.R")
source("src/04_fuzzy_matching.R")
source("src/05_score_analysis.R")
source("src/06_causal_estimation.R")

# ==============================================================================
# STEP 1: LOAD DATA
# ==============================================================================

message("\n----------------------------------------")
message("Step 1: Loading data")
message("----------------------------------------\n")

# Load WOS corpus (ground truth)
wos_data <- load_wos_corpus()
check_data(wos_data, "WOS corpus")

# Load and parse NBER citations
nber_citations <- load_nber_citations()
check_data(nber_citations, "NBER citations")

# ==============================================================================
# STEP 2: FUZZY MATCHING
# ==============================================================================

message("\n----------------------------------------")
message("Step 2: Matching citations")
message("----------------------------------------\n")

# Match NBER citations against WOS references
match_results <- fuzzy_match(
  nber_citations = nber_citations$raw_citation,
  wos_references = wos_data$reference,
  wos_ids = wos_data$wos_id
)

# Add paper_id back to results
match_results <- bind_cols(
  select(nber_citations, paper_id),
  match_results
)

# ==============================================================================
# STEP 3: ANALYZE RESULTS
# ==============================================================================

message("\n----------------------------------------")
message("Step 3: Analyzing results")
message("----------------------------------------\n")

stats <- summarize_matches(match_results)
print_summary(stats)

# Save results
save_csv(match_results, file.path(config$paths$processed, "match_results.csv"))

# ==============================================================================
# STEP 4: CAUSAL ESTIMATION (if paper dates available)
# ==============================================================================

# To run causal analysis, you need paper publication dates.
# Create a tibble with paper_id and pub_date columns, then:
#
# paper_dates <- tibble(
#   paper_id = c("w31001", "w31002", ...),
#   pub_date = as.Date(c("2022-01-15", "2022-03-20", ...))
# )
#
# analysis_data <- prepare_data(match_results, paper_dates)
# did_model <- run_did(analysis_data)
# model_summary(did_model)

message("\n----------------------------------------")
message("Analysis complete!")
message("----------------------------------------\n")
message("Results saved to: ", config$paths$processed)
message("\nTo run causal estimation, add paper publication dates.")
