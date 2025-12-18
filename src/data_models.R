# ==============================================================================
# DATA MODELS
# Schema definitions and validation for WOS and NBER data sources
# ==============================================================================

# Load configuration
if (!exists("config")) source("src/00_config.R")

# ------------------------------------------------------------------------------
# WOS DATA MODEL
# ------------------------------------------------------------------------------
# Web of Science reference data structure
#
# Input Format (tab-separated):
#   ID<TAB>Author Year Page Citations Title Journal
#
# Example:
#   WOS:000060312800003	Diepenmaat-Wolters, MGE 1997 147 55 High-performance...
#
# Parsed Fields:
#   - wos_id:     WOS identifier (e.g., "WOS:000060312800003")
#   - author:     Author name(s) (e.g., "Diepenmaat-Wolters, MGE")
#   - year:       Publication year (integer, e.g., 1997)
#   - page:       Starting page number (integer, e.g., 147)
#   - citations:  Citation count (integer, e.g., 55)
#   - title:      Article title (character)
#   - journal:    Journal name, typically uppercase (character)
#   - reference:  Original full reference string for normalization
# ------------------------------------------------------------------------------

#' Create empty WOS tibble with correct schema
#'
#' @return Empty tibble with WOS column structure
create_wos_schema <- function() {
  tibble(
    wos_id = character(),
    author = character(),
    year = integer(),
    page = integer(),
    citations = integer(),
    title = character(),
    journal = character(),
    reference = character(),
    parse_status = character()
  )
}

#' Validate WOS parsed data
#'
#' @param data WOS tibble to validate
#' @return List with validation results
validate_wos_schema <- function(data) {

  required_cols <- c("wos_id", "author", "year", "title", "journal", "reference")

  # Check columns exist
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    return(list(
      valid = FALSE,
      error = paste("Missing columns:", paste(missing_cols, collapse = ", ")),
      warnings = character()
    ))
  }

  warnings <- character()

  # Check for WOS ID format
  invalid_ids <- sum(!str_detect(data$wos_id, "^WOS:\\d+$"), na.rm = TRUE)
  if (invalid_ids > 0) {
    warnings <- c(warnings, sprintf("%d records have non-standard WOS ID format", invalid_ids))
  }

  # Check year range
  invalid_years <- sum(data$year < 1800 | data$year > as.integer(format(Sys.Date(), "%Y")) + 1, na.rm = TRUE)
  if (invalid_years > 0) {
    warnings <- c(warnings, sprintf("%d records have invalid year values", invalid_years))
  }

  # Check for missing critical fields
  na_counts <- sapply(data[required_cols], function(x) sum(is.na(x)))
  for (col in names(na_counts[na_counts > 0])) {
    warnings <- c(warnings, sprintf("%d NA values in %s column", na_counts[col], col))
  }

  return(list(
    valid = TRUE,
    n_records = nrow(data),
    warnings = warnings,
    na_summary = na_counts
  ))
}


# ------------------------------------------------------------------------------
# NBER DATA MODEL
# ------------------------------------------------------------------------------
# NBER working paper citation data structure
#
# Input Format (bibliography style):
#   Authors (Year): "Title", Publication [, Volume, Pages].
#
# Examples:
#   Auerbach, A. J. and L. H. Summers (1979): "The Investment Tax Credit: An
#     Evaluation," National Bureau of Economic Research Working Paper Series.
#
#   Barro, R. and J. Furman (2018): "Macroeconomic Effects of the 2017 Tax
#     Reform," Brookings Papers on Economic Activity, 49, 257–345.
#
# Parsed Fields:
#   - paper_id:       NBER paper ID source (e.g., "w31280")
#   - citation_num:   Sequential citation number within paper
#   - authors:        List of author names
#   - year:           Publication year (integer)
#   - title:          Article/paper title (without quotes)
#   - publication:    Journal/publisher name
#   - volume:         Volume number (if available)
#   - pages:          Page range (if available)
#   - citation_type:  Classification (journal_article, working_paper, book, etc.)
#   - raw_citation:   Original full citation string
# ------------------------------------------------------------------------------

#' Create empty NBER citation tibble with correct schema
#'
#' @return Empty tibble with NBER column structure
create_nber_schema <- function() {
  tibble(
    paper_id = character(),
    citation_num = integer(),
    authors = list(),
    authors_string = character(),
    year = integer(),
    title = character(),
    publication = character(),
    volume = character(),
    pages = character(),
    citation_type = character(),
    raw_citation = character(),
    parse_status = character()
  )
}

#' Validate NBER parsed citation data
#'
#' @param data NBER citations tibble to validate
#' @return List with validation results
validate_nber_schema <- function(data) {

  required_cols <- c("paper_id", "citation_num", "year", "title", "raw_citation")

  # Check columns exist
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    return(list(
      valid = FALSE,
      error = paste("Missing columns:", paste(missing_cols, collapse = ", ")),
      warnings = character()
    ))
  }

  warnings <- character()

  # Check paper_id format
  invalid_ids <- sum(!str_detect(data$paper_id, "^w\\d+$"), na.rm = TRUE)
  if (invalid_ids > 0) {
    warnings <- c(warnings, sprintf("%d records have non-standard paper_id format", invalid_ids))
  }

  # Check year range
  invalid_years <- sum(data$year < 1900 | data$year > as.integer(format(Sys.Date(), "%Y")) + 1, na.rm = TRUE)
  if (invalid_years > 0) {
    warnings <- c(warnings, sprintf("%d records have invalid year values", invalid_years))
  }

  # Check citation type distribution
  type_counts <- table(data$citation_type, useNA = "ifany")

  # Check for missing critical fields
  na_counts <- sapply(data[required_cols], function(x) sum(is.na(x)))
  for (col in names(na_counts[na_counts > 0])) {
    warnings <- c(warnings, sprintf("%d NA values in %s column", na_counts[col], col))
  }

  return(list(
    valid = TRUE,
    n_records = nrow(data),
    n_papers = n_distinct(data$paper_id),
    type_distribution = as.list(type_counts),
    warnings = warnings,
    na_summary = na_counts
  ))
}


# ------------------------------------------------------------------------------
# NORMALIZED CITATION MODEL
# ------------------------------------------------------------------------------
# Unified format for cross-source comparison
#
# Fields:
#   - source:            Data source ("wos" or "nber")
#   - source_id:         Original identifier
#   - authors_normalized: Normalized author string
#   - year:              Publication year
#   - title_normalized:  Normalized title
#   - full_normalized:   Full normalized reference for matching
# ------------------------------------------------------------------------------

#' Create empty normalized citation tibble
#'
#' @return Empty tibble with normalized schema
create_normalized_schema <- function() {
  tibble(
    source = character(),
    source_id = character(),
    authors_normalized = character(),
    year = integer(),
    title_normalized = character(),
    full_normalized = character()
  )
}


# ------------------------------------------------------------------------------
# MATCH RESULT MODEL
# ------------------------------------------------------------------------------
# Results from fuzzy matching between NBER and WOS
#
# Fields:
#   - nber_paper_id:      NBER source paper
#   - nber_citation_num:  Citation number within paper
#   - nber_normalized:    Normalized NBER citation
#   - wos_id:             Best matching WOS ID
#   - wos_normalized:     Matching WOS normalized reference
#   - match_distance:     Levenshtein distance (0-1 normalized)
#   - match_class:        Classification category
#   - is_hallucination:   Boolean classification
# ------------------------------------------------------------------------------

#' Create empty match results tibble
#'
#' @return Empty tibble with match result schema
create_match_schema <- function() {
  tibble(
    nber_paper_id = character(),
    nber_citation_num = integer(),
    nber_normalized = character(),
    wos_id = character(),
    wos_normalized = character(),
    match_distance = double(),
    match_class = character(),
    is_hallucination = logical()
  )
}


# ------------------------------------------------------------------------------
# FIELD TYPE DEFINITIONS
# ------------------------------------------------------------------------------

#' Get expected column types for WOS data
#'
#' @return Named list of column types for readr
wos_col_types <- function() {
  cols(
    ID = col_character(),
    reference = col_character()
  )
}

#' Get expected column types for parsed WOS data
#'
#' @return Column specification
wos_parsed_col_types <- function() {
  cols(
    wos_id = col_character(),
    author = col_character(),
    year = col_integer(),
    page = col_integer(),
    citations = col_integer(),
    title = col_character(),
    journal = col_character(),
    reference = col_character(),
    parse_status = col_character()
  )
}


# ------------------------------------------------------------------------------
# DOCUMENTATION / SCHEMA REPORT
# ------------------------------------------------------------------------------

#' Print schema documentation
#'
#' @param schema Character: "wos", "nber", "normalized", or "match"
print_schema_docs <- function(schema = "all") {

  schemas <- list(
    wos = list(
      name = "WOS (Web of Science) Reference Schema",
      fields = c(
        "wos_id     - WOS identifier (WOS:NNNNNNNNNNNN)",
        "author     - Author name(s) with initials",
        "year       - Publication year (4-digit integer)",
        "page       - Starting page number",
        "citations  - Citation count from WOS",
        "title      - Full article title",
        "journal    - Journal name (typically uppercase)",
        "reference  - Original full reference string",
        "parse_status - 'complete', 'partial', or 'failed'"
      )
    ),
    nber = list(
      name = "NBER Citation Schema",
      fields = c(
        "paper_id      - NBER paper identifier (wNNNNN)",
        "citation_num  - Sequential number within paper",
        "authors       - List of author names",
        "authors_string - Authors as single string",
        "year          - Publication year",
        "title         - Citation title (without quotes)",
        "publication   - Journal or publisher name",
        "volume        - Volume number (if present)",
        "pages         - Page range (if present)",
        "citation_type - journal_article/working_paper/book/other",
        "raw_citation  - Original citation text",
        "parse_status  - 'complete', 'partial', or 'failed'"
      )
    ),
    normalized = list(
      name = "Normalized Citation Schema",
      fields = c(
        "source           - 'wos' or 'nber'",
        "source_id        - Original identifier",
        "authors_normalized - Lowercase, punctuation-free authors",
        "year             - Publication year",
        "title_normalized - Lowercase, punctuation-free title",
        "full_normalized  - Full reference for matching"
      )
    ),
    match = list(
      name = "Match Result Schema",
      fields = c(
        "nber_paper_id    - Source NBER paper",
        "nber_citation_num - Citation number",
        "nber_normalized  - Normalized NBER citation",
        "wos_id           - Best matching WOS ID",
        "wos_normalized   - Matching WOS reference",
        "match_distance   - Normalized distance (0-1)",
        "match_class      - Classification category",
        "is_hallucination - Boolean flag"
      )
    )
  )

  if (schema == "all") {
    for (s in names(schemas)) {
      cat("\n", strrep("=", 60), "\n")
      cat(schemas[[s]]$name, "\n")
      cat(strrep("-", 60), "\n")
      cat(paste(" -", schemas[[s]]$fields), sep = "\n")
    }
  } else if (schema %in% names(schemas)) {
    cat("\n", schemas[[schema]]$name, "\n")
    cat(strrep("-", 60), "\n")
    cat(paste(" -", schemas[[schema]]$fields), sep = "\n")
  } else {
    stop("Unknown schema: ", schema, ". Use 'wos', 'nber', 'normalized', 'match', or 'all'")
  }

  invisible(NULL)
}


# ------------------------------------------------------------------------------
# MAIN EXECUTION (if run as script)
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  message("\n", strrep("=", 60))
  message("Data Models Module")
  message(strrep("=", 60), "\n")

  message("Available schema functions:")
  message("  - create_wos_schema()")
  message("  - create_nber_schema()")
  message("  - create_normalized_schema()")
  message("  - create_match_schema()")
  message("  - validate_wos_schema()")
  message("  - validate_nber_schema()")
  message("  - print_schema_docs()")

  message("\nPrint all schema documentation:")
  print_schema_docs("all")
}
