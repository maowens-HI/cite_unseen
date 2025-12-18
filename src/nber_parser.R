# ==============================================================================
# NBER CITATION PARSER
# Extract citations from NBER working paper bibliographies
# ==============================================================================
#
# NBER citations typically look like:
#   Barro, R. and J. Furman (2018): "Title Here," Journal Name, 49, 257-345.
#
# This script finds the References section and parses individual citations.
#
# ==============================================================================

if (!exists("config")) source("src/00_config.R")
source("src/data_models.R")

# ==============================================================================
# PARSE A SINGLE CITATION
# ==============================================================================

#' Parse one citation into its parts
#'
#' @param citation Full citation text
#' @return List with: authors, year, title, publication
parse_citation <- function(citation) {
  result <- list(authors = NA, year = NA, title = NA, publication = NA)

  if (is.na(citation) || nchar(trimws(citation)) == 0) return(result)
  citation <- trimws(citation)

  # --- Year: look for (2018) or just 2018 ---
  year_match <- str_match(citation, "\\((\\d{4})[a-z]?\\)")
  if (!is.na(year_match[1, 2])) {
    result$year <- as.integer(year_match[1, 2])
  } else {
    # Fallback: any 4-digit year starting with 19 or 20
    year_match <- str_extract(citation, "\\b(19|20)\\d{2}\\b")
    if (!is.na(year_match)) result$year <- as.integer(year_match)
  }

  # --- Authors: text before the year ---
  if (!is.na(result$year)) {
    author_match <- str_match(citation, paste0("^(.+?)\\s*\\(", result$year))
    if (!is.na(author_match[1, 2])) {
      result$authors <- trimws(author_match[1, 2])
    }
  }

  # --- Title: text in quotes ---
  title_match <- str_match(citation, '"([^"]+)"')
  if (!is.na(title_match[1, 2])) {
    result$title <- trimws(title_match[1, 2])
  }

  # --- Publication: text after the quoted title ---
  pub_match <- str_match(citation, '",?\\s*([^,\\d]+)')
  if (!is.na(pub_match[1, 2])) {
    pub <- trimws(pub_match[1, 2])
    pub <- str_remove(pub, "[.,;:]+$")
    if (nchar(pub) > 3) result$publication <- pub
  }

  result
}

# ==============================================================================
# FIND THE BIBLIOGRAPHY SECTION
# ==============================================================================

#' Find and extract the References section from paper text
#'
#' @param text Full paper text
#' @return Bibliography text, or NA if not found
find_bibliography <- function(text) {
  if (is.na(text) || nchar(text) < 100) return(NA)

  # Look for common bibliography headers
  for (header in c("references", "bibliography", "works cited")) {
    pattern <- paste0("(?i)\\n\\s*", header, "\\s*\\n")
    match <- str_locate(text, pattern)
    if (!is.na(match[1, "end"])) {
      bib_text <- str_sub(text, match[1, "end"])

      # Cut off at appendix if present
      appendix <- str_locate(bib_text, "(?i)\\n\\s*appendix")
      if (!is.na(appendix[1, "start"]) && appendix[1, "start"] > 100) {
        bib_text <- str_sub(bib_text, 1, appendix[1, "start"] - 1)
      }

      return(trimws(bib_text))
    }
  }

  NA
}

#' Split bibliography into individual citations
#'
#' @param bib_text Bibliography section text
#' @return Vector of citation strings
split_citations <- function(bib_text) {
  if (is.na(bib_text) || nchar(bib_text) < 20) return(character())

  # Try splitting on "Author, X. (Year)" pattern
  citations <- bib_text %>%
    str_replace_all("\\r\\n", "\n") %>%
    str_split("\\n(?=[A-Z][a-z]+,?\\s+[A-Z]\\.?[^\\n]*\\(\\d{4})") %>%
    unlist() %>%
    str_trim()

  citations <- citations[nchar(citations) > 20]

  # If that didn't work, try double newlines
  if (length(citations) < 3) {
    citations <- bib_text %>%
      str_split("\\n{2,}") %>%
      unlist() %>%
      str_trim()
    citations <- citations[nchar(citations) > 20]
  }

  str_replace_all(citations, "\\s+", " ")
}

# ==============================================================================
# MAIN FUNCTION: PARSE AN ENTIRE PAPER
# ==============================================================================

#' Extract and parse all citations from an NBER paper
#'
#' @param paper_id Paper ID (e.g., "w31280")
#' @param text Full paper text
#' @return Tibble with parsed citations
parse_nber_paper <- function(paper_id, text) {
  bib_text <- find_bibliography(text)

  if (is.na(bib_text)) {
    warning("No bibliography in ", paper_id)
    return(create_nber_tibble())
  }

  citations <- split_citations(bib_text)

  if (length(citations) == 0) {
    warning("No citations in ", paper_id)
    return(create_nber_tibble())
  }

  message("  ", paper_id, ": ", length(citations), " citations")

  # Parse each citation
  parsed <- map(citations, parse_citation)

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

# ==============================================================================
# TEST
# ==============================================================================

if (sys.nframe() == 0) {
  message("Testing NBER parser...")

  samples <- c(
    'Barro, R. (2018): "Macroeconomic Effects," Brookings Papers, 49, 257-345.',
    'Chetty, R. (2009): "Taxable Income Elasticity," American Economic Journal, 1, 31-52.'
  )

  for (s in samples) {
    result <- parse_citation(s)
    message("  ", result$authors, " (", result$year, "): ", result$title)
  }
}
