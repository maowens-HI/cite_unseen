# ==============================================================================
# PROJECT CONFIGURATION
# Cite Unseen: Detecting LLM-Hallucinated References
# ==============================================================================

# Load required packages -------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(stringdist)
  library(stringi)
})

# Optional packages (load if available) ----------------------------------------
if (requireNamespace("arrow", quietly = TRUE)) {
  library(arrow)
  message("Arrow package loaded for Parquet support")
}

if (requireNamespace("furrr", quietly = TRUE)) {
  library(furrr)
  library(future)
  message("Parallel processing packages loaded")
}

# Project paths ----------------------------------------------------------------
# All paths relative to project root

config <- list(
  # Project metadata
  project_name = "cite_unseen",
  version = "0.1.0",


  # Key dates
  dates = list(
    treatment_date = as.Date("2022-11-30"),  # ChatGPT 3.0 release
    analysis_start = as.Date("2020-01-01"),  # Start of analysis window
    analysis_end = as.Date("2024-12-31")     # End of analysis window
  ),

  # File paths
  paths = list(
    # Raw data
    wos_raw = "data/raw/wos",
    nber_raw = "data/raw/nber",

    # Processed data
    processed = "data/processed",
    intermediate = "data/intermediate",

    # Output
    figures = "output/figures",
    tables = "output/tables",
    reports = "output/reports",

    # Source code
    src = "src",
    utils = "src/utils"
  ),

  # Text normalization parameters
  normalization = list(
    # Remove numbers from normalized text?
    remove_numbers = FALSE,

    # Characters to keep (regex pattern)
    # Default: lowercase letters only
    keep_pattern = "[^a-z]",

    # Minimum length for valid normalized citation
    min_length = 10
  ),

  # Fuzzy matching parameters
  matching = list(
    # Distance method: "lv" (Levenshtein), "dl" (Damerau-Levenshtein),
    #                  "jw" (Jaro-Winkler), "cosine"
    method = "lv",

    # Thresholds for hallucination classification
    # Lower = stricter matching requirement
    thresholds = list(
      exact_match = 0.00,        # Perfect match
      high_confidence = 0.05,    # Very likely same reference
      medium_confidence = 0.15,  # Probably same reference
      low_confidence = 0.30,     # Possibly same reference
      no_match = 0.50            # Likely hallucination above this
    ),

    # Default threshold for binary classification
    hallucination_threshold = 0.30,

    # Maximum comparisons before switching to approximate methods
    max_exact_comparisons = 1e7
  ),

  # Processing parameters
  processing = list(
    # Batch size for NBER paper processing
    batch_size = 100,

    # Checkpoint interval
    checkpoint_every = 500,

    # Number of parallel workers (NULL = auto-detect)
    n_workers = NULL,

    # Random seed for reproducibility
    seed = 42
  ),

  # Output parameters
  output = list(
    # Figure dimensions (inches)
    fig_width = 8,
    fig_height = 6,

    # Figure resolution (DPI)
    fig_dpi = 300,

    # Default figure format
    fig_format = "png"
  )
)

# Set random seed for reproducibility -----------------------------------------
set.seed(config$processing$seed)

# Helper function to get full path ---------------------------------------------
#' Get full path from relative project path
#' @param relative_path Path relative to project root
#' @return Absolute path
get_path <- function(relative_path) {
  # Assumes working directory is project root
  file.path(getwd(), relative_path)
}

# Validate configuration -------------------------------------------------------
.validate_config <- function() {
  # Check that key directories exist
  required_dirs <- c(
    config$paths$wos_raw,
    config$paths$nber_raw,
    config$paths$processed,
    config$paths$intermediate
  )

  missing <- required_dirs[!dir.exists(required_dirs)]

  if (length(missing) > 0) {
    warning(
      "Missing directories (will be created): \n",
      paste("  -", missing, collapse = "\n")
    )

    # Create missing directories
    for (d in missing) {
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }
  }

  invisible(TRUE)
}

# Run validation on load
.validate_config()

# Print configuration summary --------------------------------------------------
message("\n", strrep("=", 60))
message("Cite Unseen Configuration Loaded")
message(strrep("=", 60))
message("Project: ", config$project_name, " v", config$version)
message("Treatment date: ", config$dates$treatment_date)
message("Hallucination threshold: ", config$matching$hallucination_threshold)
message(strrep("=", 60), "\n")
