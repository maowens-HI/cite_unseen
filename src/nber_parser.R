# ==============================================================================
# NBER CITATION PARSER
# Parse bibliography-style citations from NBER working papers
# ==============================================================================

# Load configuration and data models
if (!exists("config")) source("src/00_config.R")
source("src/data_models.R")

# ------------------------------------------------------------------------------
# AUTHOR PARSING
# ------------------------------------------------------------------------------

#' Parse author string into list of individual authors
#'
#' Handles formats:
#'   - "Smith, A. and B. Jones"
#'   - "Smith, A., B. Jones, and C. Brown"
#'   - "Smith, A.; Jones, B."
#'
#' @param author_string Raw author string
#' @return Character vector of individual author names
#'
#' @examples
#' parse_nber_authors("Auerbach, A. J. and L. H. Summers")
#' # Returns: c("Auerbach, A. J.", "L. H. Summers")
parse_nber_authors <- function(author_string) {

  if (is.na(author_string) || nchar(trimws(author_string)) == 0) {
    return(NA_character_)
  }

  author_string <- trimws(author_string)

  # Split on common delimiters
  # Pattern handles: ", and ", " and ", "; ", ", "
  authors <- author_string %>%
    # Handle "and" as separator
    str_replace_all(",?\\s+and\\s+", "|||") %>%
    # Handle semicolons
    str_replace_all(";\\s*", "|||") %>%
    # Split
    str_split("\\|\\|\\|") %>%
    unlist() %>%
    str_trim()

  # Remove empty entries
  authors <- authors[nchar(authors) > 0]

  return(authors)
}


# ------------------------------------------------------------------------------
# TITLE PARSING
# ------------------------------------------------------------------------------

#' Extract title from NBER citation
#'
#' Titles are typically in quotes after the year
#'
#' @param citation Full citation string
#' @return Title string (without quotes)
extract_nber_title <- function(citation) {

  if (is.na(citation) || nchar(citation) == 0) {
    return(NA_character_)
  }

  # Pattern 1: Title in double quotes - "Title"
  title_match <- str_match(citation, '"([^"]+)"')
  if (!is.na(title_match[1, 2])) {
    return(trimws(title_match[1, 2]))
  }

  # Pattern 2: Title in curly quotes - "Title"
  title_match <- str_match(citation, '[\u201C\u201D]([^\u201C\u201D]+)[\u201C\u201D]')
  if (!is.na(title_match[1, 2])) {
    return(trimws(title_match[1, 2]))
  }

  # Pattern 3: After year colon - (YYYY): Title,
  title_match <- str_match(citation, "\\(\\d{4}[a-z]?\\):\\s*(.+?),\\s*[A-Z]")
  if (!is.na(title_match[1, 2])) {
    return(trimws(title_match[1, 2]))
  }

  # Pattern 4: After year period - (YYYY). Title.
  title_match <- str_match(citation, "\\(\\d{4}[a-z]?\\)\\.\\s*(.+?)\\.\\s*[A-Z]")
  if (!is.na(title_match[1, 2])) {
    return(trimws(title_match[1, 2]))
  }

  return(NA_character_)
}


# ------------------------------------------------------------------------------
# PUBLICATION PARSING
# ------------------------------------------------------------------------------

#' Extract publication/journal name from NBER citation
#'
#' @param citation Full citation string
#' @return Publication name
extract_nber_publication <- function(citation) {

  if (is.na(citation) || nchar(citation) == 0) {
    return(NA_character_)
  }

  # After title (in quotes), publication name comes after comma
  # Pattern: "Title," Publication, Volume, Pages.

  # Try to extract after closing quote and comma
  pub_match <- str_match(citation, '",?\\s*([^,\\d]+)')
  if (!is.na(pub_match[1, 2])) {
    pub <- trimws(pub_match[1, 2])
    # Clean up trailing punctuation
    pub <- str_remove(pub, "[.,;:]+$")
    if (nchar(pub) > 3) {
      return(pub)
    }
  }

  # Try curly quotes
  pub_match <- str_match(citation, '[\u201C\u201D],?\\s*([^,\\d]+)')
  if (!is.na(pub_match[1, 2])) {
    pub <- trimws(pub_match[1, 2])
    pub <- str_remove(pub, "[.,;:]+$")
    if (nchar(pub) > 3) {
      return(pub)
    }
  }

  return(NA_character_)
}


#' Extract volume number from citation
#'
#' @param citation Full citation string
#' @return Volume as character (may include issue)
extract_nber_volume <- function(citation) {

  if (is.na(citation) || nchar(citation) == 0) {
    return(NA_character_)
  }

  # Common patterns:
  # - ", 34(2)," (volume with issue)
  # - ", 34, " (volume only)
  # - ", Vol. 34," or ", Volume 34,"

  # Pattern 1: Volume with issue in parens
  vol_match <- str_match(citation, ",\\s*(\\d+)\\s*\\([^)]+\\)\\s*,")
  if (!is.na(vol_match[1, 2])) {
    return(vol_match[1, 2])
  }

  # Pattern 2: Explicit Vol./Volume
  vol_match <- str_match(citation, "(?i)vol(?:ume)?\\.?\\s*(\\d+)")
  if (!is.na(vol_match[1, 2])) {
    return(vol_match[1, 2])
  }

  # Pattern 3: Number between commas after publication
  # Need to be careful not to pick up year or pages
  vol_match <- str_match(citation, '",\\s*[^,]+,\\s*(\\d{1,3})\\s*,')
  if (!is.na(vol_match[1, 2])) {
    return(vol_match[1, 2])
  }

  return(NA_character_)
}


#' Extract page range from citation
#'
#' @param citation Full citation string
#' @return Page range as character
extract_nber_pages <- function(citation) {

  if (is.na(citation) || nchar(citation) == 0) {
    return(NA_character_)
  }

  # Common patterns:
  # - "87-108" or "87–108" (en-dash)
  # - "pp. 87-108"
  # - ", 87-108."

  # Pattern 1: Page range with dash
  pages_match <- str_extract(citation, "\\d+[\\-\u2013]\\d+")
  if (!is.na(pages_match)) {
    # Normalize dash
    return(str_replace(pages_match, "\u2013", "-"))
  }

  # Pattern 2: pp. prefix
  pages_match <- str_match(citation, "(?i)pp?\\.?\\s*(\\d+[\\-\u2013]?\\d*)")
  if (!is.na(pages_match[1, 2])) {
    return(str_replace(pages_match[1, 2], "\u2013", "-"))
  }

  return(NA_character_)
}


# ------------------------------------------------------------------------------
# YEAR PARSING
# ------------------------------------------------------------------------------

#' Extract year from NBER citation
#'
#' @param citation Full citation string
#' @return Integer year
extract_nber_year <- function(citation) {

  if (is.na(citation) || nchar(citation) == 0) {
    return(NA_integer_)
  }

  # Pattern 1: Year in parentheses (most common)
  year_match <- str_match(citation, "\\((\\d{4})[a-z]?\\)")
  if (!is.na(year_match[1, 2])) {
    year <- as.integer(year_match[1, 2])
    if (year >= 1800 && year <= as.integer(format(Sys.Date(), "%Y")) + 1) {
      return(year)
    }
  }

  # Pattern 2: Year without parentheses (e.g., "Author 2020: Title")
  year_match <- str_extract(citation, "\\b(19|20)\\d{2}\\b")
  if (!is.na(year_match)) {
    year <- as.integer(year_match)
    if (year >= 1800 && year <= as.integer(format(Sys.Date(), "%Y")) + 1) {
      return(year)
    }
  }

  return(NA_integer_)
}


# ------------------------------------------------------------------------------
# CITATION TYPE CLASSIFICATION
# ------------------------------------------------------------------------------

#' Classify NBER citation type
#'
#' @param citation Full citation string
#' @param publication Extracted publication name (optional)
#' @return Character: classification type
classify_nber_citation <- function(citation, publication = NA) {

  if (is.na(citation) || nchar(citation) == 0) {
    return("unknown")
  }

  citation_lower <- str_to_lower(citation)

  # Check for working paper indicators
  if (str_detect(citation_lower, "working paper|nber|ssrn|cepr|iza|ecb")) {
    return("working_paper")
  }

  # Check for book indicators
  if (str_detect(citation_lower, "\\bpress\\b|\\bpublisher|university press|cambridge|oxford|princeton|mit press")) {
    return("book")
  }

  # Check for book chapter indicators
  if (str_detect(citation_lower, "\\bin:\\s|\\beds?\\.\\s|\\bchapter\\b|handbook of")) {
    return("book_chapter")
  }

  # Check for dissertation
  if (str_detect(citation_lower, "dissertation|thesis|ph\\.?d\\.?")) {
    return("dissertation")
  }

  # Check for report/policy paper
  if (str_detect(citation_lower, "report|policy|imf|world bank|oecd|federal reserve")) {
    return("report")
  }

  # Check for journal indicators
  if (str_detect(citation_lower, "journal|review|quarterly|econometrica|american economic")) {
    return("journal_article")
  }

  # If publication looks like a journal (has "of" or ends in common suffixes)
  if (!is.na(publication)) {
    pub_lower <- str_to_lower(publication)
    if (str_detect(pub_lower, "journal of|review of|quarterly|econometrica")) {
      return("journal_article")
    }
  }

  return("other")
}


# ------------------------------------------------------------------------------
# MAIN PARSING FUNCTION
# ------------------------------------------------------------------------------

#' Parse a single NBER bibliography-style citation
#'
#' @param citation Full citation string
#' @return Named list with parsed fields
#'
#' @examples
#' cit <- 'Barro, R. and J. Furman (2018): "Macro Effects," BPEA, 49, 257-345.'
#' parse_nber_citation(cit)
parse_nber_citation <- function(citation) {

  # Default result
  default_result <- list(
    authors = list(NA_character_),
    authors_string = NA_character_,
    year = NA_integer_,
    title = NA_character_,
    publication = NA_character_,
    volume = NA_character_,
    pages = NA_character_,
    citation_type = "unknown",
    parse_status = "failed"
  )

  if (is.na(citation) || nchar(trimws(citation)) == 0) {
    return(default_result)
  }

  citation <- trimws(citation)

  # Extract components
  year <- extract_nber_year(citation)
  title <- extract_nber_title(citation)
  publication <- extract_nber_publication(citation)
  volume <- extract_nber_volume(citation)
  pages <- extract_nber_pages(citation)

  # Extract authors (text before year)
  author_string <- NA_character_
  if (!is.na(year)) {
    # Get text before year (including the opening paren)
    author_match <- str_match(citation, paste0("^(.+?)\\s*\\(", year))
    if (!is.na(author_match[1, 2])) {
      author_string <- trimws(author_match[1, 2])
    }
  } else {
    # Fallback: text before first quote or digit
    author_match <- str_match(citation, '^([^"\\d]+)')
    if (!is.na(author_match[1, 2])) {
      author_string <- trimws(author_match[1, 2])
    }
  }

  authors <- parse_nber_authors(author_string)
  citation_type <- classify_nber_citation(citation, publication)

  # Determine parse status
  parse_status <- "failed"
  if (!is.na(year) && !is.na(title)) {
    if (!is.na(publication) && length(authors) > 0 && !is.na(authors[1])) {
      parse_status <- "complete"
    } else {
      parse_status <- "partial"
    }
  } else if (!is.na(year) || !is.na(title)) {
    parse_status <- "partial"
  }

  return(list(
    authors = list(authors),
    authors_string = author_string,
    year = year,
    title = title,
    publication = publication,
    volume = volume,
    pages = pages,
    citation_type = citation_type,
    parse_status = parse_status
  ))
}


#' Parse multiple NBER citations
#'
#' @param citations Character vector of citations
#' @param paper_id Source paper ID
#' @param progress Show progress bar?
#' @return Tibble with parsed citations
parse_nber_citations <- function(citations, paper_id = "unknown", progress = TRUE) {

  if (length(citations) == 0) {
    return(create_nber_schema())
  }

  message("Parsing ", length(citations), " citations from ", paper_id, "...")

  # Parse each citation
  if (progress && length(citations) > 10) {
    parsed <- map(citations, parse_nber_citation, .progress = TRUE)
  } else {
    parsed <- map(citations, parse_nber_citation)
  }

  # Build tibble
  result <- tibble(
    paper_id = paper_id,
    citation_num = seq_along(citations),
    raw_citation = citations
  ) %>%
    bind_cols(
      parsed %>%
        map(function(p) {
          tibble(
            authors = p$authors,
            authors_string = p$authors_string,
            year = p$year,
            title = p$title,
            publication = p$publication,
            volume = p$volume,
            pages = p$pages,
            citation_type = p$citation_type,
            parse_status = p$parse_status
          )
        }) %>%
        bind_rows()
    )

  # Report results
  status_counts <- table(result$parse_status)
  message("Parsing results:")
  for (status in names(status_counts)) {
    message(sprintf("  %s: %d", status, status_counts[status]))
  }

  return(result)
}


# ------------------------------------------------------------------------------
# BIBLIOGRAPHY SECTION EXTRACTION
# ------------------------------------------------------------------------------

#' Detect and extract bibliography section from paper text
#'
#' @param text Full paper text
#' @return Character string of bibliography section
extract_bibliography <- function(text) {

  if (is.na(text) || nchar(text) < 100) {
    return(NA_character_)
  }

  # Common section headers
  patterns <- c(
    "(?i)\\n\\s*references\\s*\\n",
    "(?i)\\n\\s*bibliography\\s*\\n",
    "(?i)\\n\\s*works cited\\s*\\n",
    "(?i)\\n\\s*literature cited\\s*\\n"
  )

  bib_start <- NA_integer_

  for (pattern in patterns) {
    match <- str_locate(text, pattern)
    if (!is.na(match[1, "end"])) {
      bib_start <- match[1, "end"]
      break
    }
  }

  if (is.na(bib_start)) {
    return(NA_character_)
  }

  # Extract from start to end (or to appendix/tables)
  bib_text <- str_sub(text, bib_start)

  # Detect end of bibliography
  end_patterns <- c(
    "(?i)\\n\\s*appendix",
    "(?i)\\n\\s*tables?\\s*\\n",
    "(?i)\\n\\s*figures?\\s*\\n",
    "(?i)\\n\\s*supplementary"
  )

  for (pattern in end_patterns) {
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
#' @param bib_text Bibliography section text
#' @return Character vector of individual citations
split_bibliography <- function(bib_text) {

  if (is.na(bib_text) || nchar(bib_text) < 20) {
    return(character())
  }

  # Strategy 1: Split on newline + Author pattern
  # Pattern: newline followed by name and year in parentheses
  citations <- bib_text %>%
    str_replace_all("\\r\\n", "\n") %>%
    str_split("\\n(?=[A-Z][a-z]+,?\\s+[A-Z]\\.?[^\\n]*\\(\\d{4})") %>%
    unlist() %>%
    str_trim() %>%
    .[nchar(.) > 20]

  # If too few results, try double newline split
  if (length(citations) < 3) {
    citations <- bib_text %>%
      str_split("\\n{2,}") %>%
      unlist() %>%
      str_trim() %>%
      .[nchar(.) > 20]
  }

  # Clean up each citation
  citations <- citations %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()

  return(citations)
}


#' Extract and parse all citations from an NBER paper
#'
#' @param paper_id NBER paper ID
#' @param text Full paper text
#' @return Tibble with parsed citations
extract_and_parse_paper <- function(paper_id, text) {

  # Extract bibliography section
  bib_text <- extract_bibliography(text)

  if (is.na(bib_text)) {
    warning("Could not find bibliography in ", paper_id)
    return(create_nber_schema() %>% mutate(paper_id = paper_id))
  }

  # Split into individual citations
  citations <- split_bibliography(bib_text)

  if (length(citations) == 0) {
    warning("No citations extracted from ", paper_id)
    return(create_nber_schema() %>% mutate(paper_id = paper_id))
  }

  # Parse citations
  parse_nber_citations(citations, paper_id, progress = FALSE)
}


# ------------------------------------------------------------------------------
# DIAGNOSTICS
# ------------------------------------------------------------------------------

#' Generate NBER parsing diagnostics
#'
#' @param nber_data Parsed NBER citations tibble
#' @return List with diagnostic information
nber_parsing_diagnostics <- function(nber_data) {

  report <- list(
    total_citations = nrow(nber_data),
    total_papers = n_distinct(nber_data$paper_id),
    citations_per_paper = nrow(nber_data) / max(1, n_distinct(nber_data$paper_id)),

    parse_status = as.list(table(nber_data$parse_status)),

    type_distribution = as.list(table(nber_data$citation_type)),

    field_completeness = list(
      authors_string = mean(!is.na(nber_data$authors_string)),
      year = mean(!is.na(nber_data$year)),
      title = mean(!is.na(nber_data$title)),
      publication = mean(!is.na(nber_data$publication)),
      volume = mean(!is.na(nber_data$volume)),
      pages = mean(!is.na(nber_data$pages))
    ),

    year_range = list(
      min = min(nber_data$year, na.rm = TRUE),
      max = max(nber_data$year, na.rm = TRUE),
      median = median(nber_data$year, na.rm = TRUE)
    )
  )

  return(report)
}


#' Print NBER parsing diagnostics
#'
#' @param nber_data Parsed NBER citations tibble
print_nber_diagnostics <- function(nber_data) {

  report <- nber_parsing_diagnostics(nber_data)

  message("\n", strrep("=", 60))
  message("NBER Citation Parsing Diagnostics")
  message(strrep("=", 60))

  message("\nTotal citations: ", report$total_citations)
  message("Papers processed: ", report$total_papers)
  message("Avg citations/paper: ", round(report$citations_per_paper, 1))

  message("\nParsing status:")
  for (status in names(report$parse_status)) {
    pct <- 100 * report$parse_status[[status]] / report$total_citations
    message(sprintf("  %-12s: %6d (%5.1f%%)", status, report$parse_status[[status]], pct))
  }

  message("\nCitation types:")
  for (type in names(report$type_distribution)) {
    pct <- 100 * report$type_distribution[[type]] / report$total_citations
    message(sprintf("  %-15s: %6d (%5.1f%%)", type, report$type_distribution[[type]], pct))
  }

  message("\nField completeness:")
  for (field in names(report$field_completeness)) {
    pct <- 100 * report$field_completeness[[field]]
    message(sprintf("  %-15s: %5.1f%%", field, pct))
  }

  message("\nYear range: ", report$year_range$min, " - ", report$year_range$max,
          " (median: ", report$year_range$median, ")")

  invisible(report)
}


# ------------------------------------------------------------------------------
# SAMPLE DATA AND TESTING
# ------------------------------------------------------------------------------

#' Get sample NBER citations for testing
#'
#' @return Character vector of sample citations
get_nber_samples <- function() {
  c(
    'Auerbach, A. J. and L. H. Summers (1979): "The Investment Tax Credit: An Evaluation," National Bureau of Economic Research Working Paper Series.',
    'Barro, R. and J. Furman (2018): "Macroeconomic Effects of the 2017 Tax Reform," Brookings Papers on Economic Activity, 49, 257-345.',
    'Feldstein, M. (1995): "The Effect of Marginal Tax Rates on Taxable Income: A Panel Study of the 1986 Tax Reform Act," Journal of Political Economy, 103, 551-572.',
    'Piketty, T., E. Saez, and S. Stantcheva (2014): "Optimal Taxation of Top Labor Incomes: A Tale of Three Elasticities," American Economic Journal: Economic Policy, 6, 230-271.',
    'Chetty, R. (2009): "Is the Taxable Income Elasticity Sufficient to Calculate Deadweight Loss? The Implications of Evasion and Avoidance," American Economic Journal: Economic Policy, 1, 31-52.'
  )
}


#' Test NBER parser with sample data
#'
#' @return Tibble with parsed sample data
test_nber_parser <- function() {

  samples <- get_nber_samples()

  message("Testing NBER parser with ", length(samples), " samples...\n")

  # Parse samples
  results <- parse_nber_citations(samples, "test_paper", progress = FALSE)

  # Show results
  message("\nParsed results:")
  print(results %>% select(citation_num, authors_string, year, title, citation_type, parse_status))

  return(results)
}


# ------------------------------------------------------------------------------
# MAIN EXECUTION (if run as script)
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  message("\n", strrep("=", 60))
  message("NBER Citation Parser Module")
  message(strrep("=", 60), "\n")

  message("Available functions:")
  message("  - parse_nber_citation()      Parse single citation")
  message("  - parse_nber_citations()     Parse multiple citations")
  message("  - extract_bibliography()     Extract bibliography from text")
  message("  - split_bibliography()       Split into individual citations")
  message("  - extract_and_parse_paper()  Full pipeline for one paper")
  message("  - print_nber_diagnostics()   Show parsing statistics")

  message("\nRunning parser test...")
  test_results <- test_nber_parser()
}
