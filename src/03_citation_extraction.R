# ==============================================================================
# CITATION EXTRACTION
# Extract bibliographic citations from NBER working paper text
# ==============================================================================

# Load configuration and dependencies
if (!exists("config")) source("src/00_config.R")

# ------------------------------------------------------------------------------
# BIBLIOGRAPHY SECTION DETECTION
# ------------------------------------------------------------------------------

#' Detect the start of bibliography/references section
#'
#' @param text Full paper text
#' @return Integer position of section start, or NA if not found
detect_bibliography_start <- function(text) {

  # Common section headers (case insensitive)
  patterns <- c(
    "(?i)\\n\\s*references\\s*\\n",
    "(?i)\\n\\s*bibliography\\s*\\n",
    "(?i)\\n\\s*works cited\\s*\\n",
    "(?i)\\n\\s*literature cited\\s*\\n",
    "(?i)\\n\\s*citations\\s*\\n",
    "(?i)\\n\\s*sources\\s*\\n"
  )

  # Try each pattern
  for (pattern in patterns) {
    match <- str_locate(text, pattern)
    if (!is.na(match[1, "start"])) {
      return(match[1, "end"])
    }
  }

  return(NA_integer_)
}

#' Extract bibliography section from paper text
#'
#' @param text Full paper text
#' @return Character string of bibliography section
extract_bibliography_section <- function(text) {

  bib_start <- detect_bibliography_start(text)

  if (is.na(bib_start)) {
    warning("Could not detect bibliography section")
    return(NA_character_)
  }

  # Extract from bibliography start to end of document
  # (or to next major section like "Appendix")
  bib_text <- str_sub(text, bib_start)

  # Try to detect end of bibliography (start of appendix, etc.)
  end_patterns <- c(
    "(?i)\\n\\s*appendix",
    "(?i)\\n\\s*supplementary",
    "(?i)\\n\\s*tables\\s*\\n",
    "(?i)\\n\\s*figures\\s*\\n"
  )

  for (pattern in end_patterns) {
    match <- str_locate(bib_text, pattern)
    if (!is.na(match[1, "start"]) && match[1, "start"] > 100) {
      bib_text <- str_sub(bib_text, 1, match[1, "start"] - 1)
      break
    }
  }

  return(bib_text)
}

# ------------------------------------------------------------------------------
# CITATION PARSING
# ------------------------------------------------------------------------------

#' Parse individual citations from bibliography text
#'
#' @param bib_text Bibliography section text
#' @return Character vector of individual citations
parse_citations <- function(bib_text) {

  if (is.na(bib_text) || nchar(bib_text) < 10) {
    return(character())
  }

  # Strategy 1: Split on common citation patterns
  # Look for patterns like "Author, A. (YYYY)" at start of lines

  # First, try to split on year patterns
  # Pattern: newline followed by author-like text and year in parentheses
  citations <- bib_text %>%
    # Normalize line breaks
    str_replace_all("\\r\\n", "\n") %>%
    # Split on what looks like new citation starts
    str_split("\\n(?=[A-Z][a-z]+,?\\s+[A-Z]\\.?[^\\n]*\\(\\d{4}\\))") %>%
    unlist() %>%
    # Clean up each citation
    str_trim() %>%
    # Remove empty entries
    .[nchar(.) > 20]

  # If that didn't work well, try simpler paragraph splitting
  if (length(citations) < 3) {
    citations <- bib_text %>%
      str_split("\\n{2,}") %>%
      unlist() %>%
      str_trim() %>%
      .[nchar(.) > 20]
  }

  return(citations)
}

#' Extract year from citation text
#'
#' @param citation Citation string
#' @return Integer year or NA
extract_citation_year <- function(citation) {
  # Look for 4-digit year in parentheses
  year_match <- str_extract(citation, "\\(\\d{4}[a-z]?\\)")

  if (is.na(year_match)) {
    # Try without parentheses
    year_match <- str_extract(citation, "\\b(19|20)\\d{2}\\b")
  }

  if (is.na(year_match)) {
    return(NA_integer_)
  }

  year <- as.integer(str_extract(year_match, "\\d{4}"))

  # Validate reasonable year range
  if (year < 1800 || year > as.integer(format(Sys.Date(), "%Y")) + 1) {
    return(NA_integer_)
  }

  return(year)
}

#' Extract author names from citation
#'
#' @param citation Citation string
#' @return Character string of author portion
extract_citation_authors <- function(citation) {
  # Get text before the year in parentheses
  author_part <- str_extract(citation, "^[^(]+(?=\\(\\d{4})")

  if (is.na(author_part)) {
    # Fallback: first 50 characters or until period
    author_part <- str_extract(citation, "^.{1,50}")
  }

  return(str_trim(author_part))
}

# ------------------------------------------------------------------------------
# MAIN EXTRACTION PIPELINE
# ------------------------------------------------------------------------------

#' Extract all citations from a single NBER paper
#'
#' @param paper_id NBER paper ID
#' @param text Full paper text
#' @return Tibble with extracted citations
extract_paper_citations <- function(paper_id, text) {

  # Extract bibliography section
  bib_text <- extract_bibliography_section(text)

  if (is.na(bib_text)) {
    return(tibble(
      paper_id = paper_id,
      citation_num = integer(),
      citation = character(),
      citation_year = integer(),
      citation_authors = character()
    ))
  }

  # Parse individual citations
  citations <- parse_citations(bib_text)

  if (length(citations) == 0) {
    return(tibble(
      paper_id = paper_id,
      citation_num = integer(),
      citation = character(),
      citation_year = integer(),
      citation_authors = character()
    ))
  }

  # Build result tibble
  tibble(
    paper_id = paper_id,
    citation_num = seq_along(citations),
    citation = citations,
    citation_year = map_int(citations, extract_citation_year),
    citation_authors = map_chr(citations, extract_citation_authors)
  )
}

#' Extract citations from multiple NBER papers
#'
#' @param nber_data Tibble with paper_id and text columns
#' @param progress Show progress? Default TRUE
#' @return Tibble with all extracted citations
extract_citations <- function(nber_data, progress = TRUE) {

  if (!all(c("paper_id", "text") %in% names(nber_data))) {
    stop("Data must have 'paper_id' and 'text' columns")
  }

  n_papers <- nrow(nber_data)
  message("Extracting citations from ", n_papers, " papers...")

  # Process each paper
  results <- map2(
    nber_data$paper_id,
    nber_data$text,
    function(pid, txt) {
      tryCatch(
        extract_paper_citations(pid, txt),
        error = function(e) {
          warning("Failed to extract from ", pid, ": ", e$message)
          tibble(
            paper_id = pid,
            citation_num = integer(),
            citation = character(),
            citation_year = integer(),
            citation_authors = character()
          )
        }
      )
    },
    .progress = progress
  ) %>%
    bind_rows()

  message("Extracted ", nrow(results), " citations from ",
          n_distinct(results$paper_id), " papers")
  message("Average citations per paper: ",
          round(nrow(results) / n_distinct(results$paper_id), 1))

  return(results)
}

# ------------------------------------------------------------------------------
# VALIDATION
# ------------------------------------------------------------------------------

#' Validate extracted citations
#'
#' @param citations Tibble of extracted citations
#' @return Validation report as list
validate_citations <- function(citations) {

  report <- list(
    total_citations = nrow(citations),
    total_papers = n_distinct(citations$paper_id),
    citations_per_paper = nrow(citations) / n_distinct(citations$paper_id),

    year_coverage = mean(!is.na(citations$citation_year)),
    author_coverage = mean(!is.na(citations$citation_authors)),

    median_citation_length = median(nchar(citations$citation)),
    short_citations = sum(nchar(citations$citation) < 30),
    long_citations = sum(nchar(citations$citation) > 500)
  )

  message("\nCitation Validation Report:")
  message("  Total citations: ", report$total_citations)
  message("  Papers processed: ", report$total_papers)
  message("  Avg citations/paper: ", round(report$citations_per_paper, 1))
  message("  Year extraction rate: ", round(100 * report$year_coverage, 1), "%")
  message("  Author extraction rate: ", round(100 * report$author_coverage, 1), "%")

  if (report$short_citations > 0) {
    warning(report$short_citations, " citations may be incomplete (< 30 chars)")
  }

  return(invisible(report))
}

# ------------------------------------------------------------------------------
# MAIN EXECUTION (if run as script)
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  message("\n", strrep("=", 60))
  message("Citation Extraction Module")
  message(strrep("=", 60), "\n")

  # Demo with sample text
  sample_text <- "
  This is a sample paper introduction.

  The methodology section goes here.

  References

  Smith, J. (2020). Machine Learning in Economics. Journal of Economic
  Perspectives, 34(2), 87-108.

  Johnson, A. & Williams, B. (2019). Deep Learning Applications.
  Nature Machine Intelligence, 1, 15-25.

  Brown, C. (2021). AI and Society. Tech Review, 100, 50-75.
  "

  # Test extraction
  result <- extract_paper_citations("w99999", sample_text)
  print(result)

  message("\nCitation extraction module loaded successfully.")
}
