# ==============================================================================
# DATA MODELS
# Simple tibble schemas for WOS and NBER data
# ==============================================================================

if (!exists("config")) source("src/00_config.R")

# ==============================================================================
# WOS (Web of Science) DATA
# ==============================================================================
# Each row is a reference from Web of Science.

#' Create empty WOS tibble
create_wos_tibble <- function() {
  tibble(
    wos_id    = character(),
    author    = character(),
    year      = integer(),
    journal   = character(),
    reference = character()
  )
}

# ==============================================================================
# NBER CITATION DATA
# ==============================================================================
# Each row is a citation from an NBER paper's bibliography.

#' Create empty NBER citations tibble
create_nber_tibble <- function() {
  tibble(
    paper_id     = character(),
    citation_num = integer(),
    authors      = character(),
    year         = integer(),
    title        = character(),
    publication  = character(),
    raw_citation = character()
  )
}

# ==============================================================================
# MATCH RESULTS
# ==============================================================================
# Results from matching NBER citations against WOS references.

#' Create empty match results tibble
create_match_tibble <- function() {
  tibble(
    paper_id         = character(),
    raw_citation     = character(),
    best_wos_id      = character(),
    distance         = double(),
    is_hallucination = logical()
  )
}

#' Quick sanity check on data
check_data <- function(data, name = "data") {
  message(sprintf("%s: %d rows, %d columns", name, nrow(data), ncol(data)))
}
