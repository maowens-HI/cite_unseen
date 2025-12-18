# ==============================================================================
# NBER CITATION PARSER
# Parse bibliography citations from NBER working papers
# ==============================================================================
#
# NBER citations look like:
#   Barro, R. and J. Furman (2018): "Title Here," Journal Name, 49, 257-345.
#
# We need to extract: authors, year, title, publication
#
# ==============================================================================

if (!exists("config")) source("src/00_config.R")
source("src/data_models.R")

# ------------------------------------------------------------------------------
# MAIN PARSING FUNCTION
# ------------------------------------------------------------------------------

#' Parse a single NBER citation
#'
#' @param citation The full citation text
#' @return Named list with: authors, year, title, publication
parse_nber_citation <- function(citation) {

  # Default: all NA
  result <- list(authors = NA, year = NA, title = NA, publication = NA)

  if (is.na(citation) || nchar(trimws(citation)) == 0) return(result)

  citation <- trimws(citation)

  # --- Extract year (usually in parentheses) ---
  year_match <- str_match(citation, "\\((\\d{4})[a-z]?\\)")
  if (!is.na(year_match[1, 2])) {
    result$year <- as.integer(year_match[1, 2])
  } else {
    # Fallback: any 4-digit year
    year_match <- str_extract(citation, "\\b(19|20)\\d{2}\\b")
    if (!is.na(year_match)) result$year <- as.integer(year_match)
  }

  # --- Extract authors (text before the year) ---
  if (!is.na(result$year)) {
    author_match <- str_match(citation, paste0("^(.+?)\\s*\\(", result$year))
    if (!is.na(author_match[1, 2])) {
      result$authors <- trimws(author_match[1, 2])
    }
  }

  # --- Extract title (usually in quotes) ---
  title_match <- str_match(citation, '"([^"]+)"')
  if (!is.na(title_match[1, 2])) {
    result$title <- trimws(title_match[1, 2])
  } else {
    # Try curly quotes
    title_match <- str_match(citation, '[\u201C]([^\u201D]+)[\u201D]')
    if (!is.na(title_match[1, 2])) {
      result$title <- trimws(title_match[1, 2])
    }
  }

  # --- Extract publication (after the title) ---
  pub_match <- str_match(citation, '",?\\s*([^,\\d]+)')
  if (!is.na(pub_match[1, 2])) {
    pub <- trimws(pub_match[1, 2])
    pub <- str_remove(pub, "[.,;:]+$")  # Remove trailing punctuation
    if (nchar(pub) > 3) result$publication <- pub
  }

  return(result)
}

# ------------------------------------------------------------------------------
# BIBLIOGRAPHY EXTRACTION
# ------------------------------------------------------------------------------

#' Find and extract the bibliography section from paper text
#'
#' @param text Full paper text
#' @return The bibliography section as a string, or NA
extract_bibliography <- function(text) {

  if (is.na(text) || nchar(text) < 100) return(NA)

  # Look for section headers
  patterns <- c(
    "(?i)\\n\\s*references\\s*\\n",
    "(?i)\\n\\s*bibliography\\s*\\n",
    "(?i)\\n\\s*works cited\\s*\\n"
  )

  bib_start <- NA
  for (pattern in patterns) {
    match <- str_locate(text, pattern)
    if (!is.na(match[1, "end"])) {
      bib_start <- match[1, "end"]
      break
    }
  }

  if (is.na(bib_start)) return(NA)

  # Get text from references to end (or to appendix)
  bib_text <- str_sub(text, bib_start)

  # Cut off at appendix/tables if present
  for (pattern in c("(?i)\\n\\s*appendix", "(?i)\\n\\s*tables?\\s*\\n")) {
    match <- str_locate(bib_text, pattern)
    if (!is.na(match[1, "start"]) && match[1, "start"] > 100) {
      bib_text <- str_sub(bib_text, 1, match[1, "start"] - 1)
      break
    }
  }

  return(trimws(bib_text))
}

#' Split bibliography text into individual citations
#'
#' @param bib_text The bibliography section
#' @return Character vector of citations
split_bibliography <- function(bib_text) {

  if (is.na(bib_text) || nchar(bib_text) < 20) return(character())

  # Split on newline followed by author pattern (Name, Initial)
  citations <- bib_text %>%
    str_replace_all("\\r\\n", "\n") %>%
    str_split("\\n(?=[A-Z][a-z]+,?\\s+[A-Z]\\.?[^\\n]*\\(\\d{4})") %>%
    unlist() %>%
    str_trim()

  # Keep only entries that look like citations (>20 chars)
  citations <- citations[nchar(citations) > 20]

  # If that didn't work well, try splitting on double newlines
  if (length(citations) < 3) {
    citations <- bib_text %>%
      str_split("\\n{2,}") %>%
      unlist() %>%
      str_trim()
    citations <- citations[nchar(citations) > 20]
  }

  # Clean up whitespace
  citations <- str_replace_all(citations, "\\s+", " ")

  return(citations)
}

# ------------------------------------------------------------------------------
# PROCESS A FULL PAPER
# ------------------------------------------------------------------------------

#' Extract and parse all citations from an NBER paper
#'
#' @param paper_id The NBER paper ID (e.g., "w31280")
#' @param text The full paper text
#' @return Tibble with parsed citations
parse_nber_paper <- function(paper_id, text) {

  # Extract bibliography section
  bib_text <- extract_bibliography(text)

  if (is.na(bib_text)) {
    warning("No bibliography found in ", paper_id)
    return(create_nber_tibble())
  }

  # Split into individual citations
  citations <- split_bibliography(bib_text)

  if (length(citations) == 0) {
    warning("No citations extracted from ", paper_id)
    return(create_nber_tibble())
  }

  message("  ", paper_id, ": ", length(citations), " citations")

  # Parse each citation
  parsed <- map(citations, parse_nber_citation)

  # Build result tibble
  tibble(
    paper_id     = paper_id,
    citation_num = seq_along(citations),
    authors      = map_chr(parsed, ~ .x$authors %||% NA_character_),
    year         = map_int(parsed, ~ .x$year %||% NA_integer_),
    title        = map_chr(parsed, ~ .x$title %||% NA_character_),
    publication  = map_chr(parsed, ~ .x$publication %||% NA_character_),
    raw_citation = citations
  )
}

# ------------------------------------------------------------------------------
# TEST
# ------------------------------------------------------------------------------

test_nber_parser <- function() {
  samples <- c(
    'Barro, R. and J. Furman (2018): "Macroeconomic Effects," Brookings Papers, 49, 257-345.',
    'Chetty, R. (2009): "Is the Taxable Income Elasticity Sufficient?" American Economic Journal, 1, 31-52.'
  )

  message("Testing NBER parser...")
  for (s in samples) {
    result <- parse_nber_citation(s)
    message("  year=", result$year, ", authors=", result$authors)
  }
}

if (sys.nframe() == 0) test_nber_parser()
