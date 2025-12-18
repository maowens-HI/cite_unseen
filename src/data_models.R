# ==============================================================================
# DATA MODELS
# Simple tibble schemas for WOS and NBER data
# ==============================================================================

# Load config if not already loaded
if (!exists("config")) source("src/00_config.R")

# ==============================================================================
# WOS (Web of Science) DATA
# ==============================================================================
# Each row is a reference from Web of Science.
# Fields:
#   wos_id    - WOS identifier (e.g., "WOS:000060312800003")
#   author    - Author name(s)
#   year      - Publication year
#   title     - Article title
#   journal   - Journal name
#   reference - Original full reference string

#' Create empty WOS tibble
create_wos_tibble <- function() {
  tibble(
    wos_id    = character(),
    author    = character(),
    year      = integer(),
    title     = character(),
    journal   = character(),
    reference = character()
  )
}

# ==============================================================================
# NBER CITATION DATA
# ==============================================================================
# Each row is a citation from an NBER paper's bibliography.
# Fields:
#   paper_id     - NBER paper ID (e.g., "w31280")
#   citation_num - Which citation in the paper (1, 2, 3, ...)
#   authors      - Author names
#   year         - Publication year
#   title        - Citation title
#   publication  - Journal or publisher
#   raw_citation - Original citation text

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
# Fields:
#   nber_paper_id  - Source NBER paper
#   nber_citation  - The NBER citation text (normalized)
#   best_wos_id    - Best matching WOS reference ID
#   match_distance - How different they are (0 = identical, 1 = totally different)
#   is_hallucination - TRUE if likely hallucinated (distance > threshold)

#' Create empty match results tibble
create_match_tibble <- function() {
  tibble(
    nber_paper_id    = character(),
    nber_citation    = character(),
    best_wos_id      = character(),
    match_distance   = double(),
    is_hallucination = logical()
  )
}

# Quick sanity check on data
check_data <- function(data, name = "data") {
  message(sprintf("%s: %d rows, %d columns", name, nrow(data), ncol(data)))
  message("  Columns: ", paste(names(data), collapse = ", "))
  if (nrow(data) > 0) {
    na_pct <- round(100 * sum(is.na(data)) / (nrow(data) * ncol(data)), 1)
    message(sprintf("  NA values: %.1f%%", na_pct))
  }
}
