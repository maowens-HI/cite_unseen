# ==============================================================================
# PROJECT CONFIGURATION
# Cite Unseen: Detecting LLM-Hallucinated References in NBER Papers
# ==============================================================================

# Load packages ----------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(stringdist)  # For fuzzy matching
  library(stringi)     # For text processing
})

# Load parallel processing if available
if (requireNamespace("furrr", quietly = TRUE)) {
  library(furrr)
  library(future)
}

# Configuration ----------------------------------------------------------------
# These are the key settings you might want to change

config <- list(
  # The key date: ChatGPT 3.0 was released Nov 30, 2022
  # Papers after this date could potentially use LLM-generated citations
  treatment_date = as.Date("2022-11-30"),

  # Where data lives
  paths = list(
    wos_raw     = "data/raw/wos",       # Web of Science exports
    nber_raw    = "data/raw/nber",      # NBER paper texts
    processed   = "data/processed",     # Cleaned/parsed data
    output      = "output"              # Results, figures, tables
  ),

  # Fuzzy matching threshold
  # A match distance above this = likely hallucination
  # 0.30 means citations must be ~70% similar to be considered "real"
  hallucination_threshold = 0.30,

  # For reproducibility
  seed = 42
)

# Set random seed
set.seed(config$seed)

# Create directories if they don't exist ---------------------------------------
for (path in config$paths) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

# Done -------------------------------------------------------------------------
message("Config loaded. Treatment date: ", config$treatment_date)
