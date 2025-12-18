# ==============================================================================
# WOS PARSER
# Parse Web of Science reference data with structured field extraction
# ==============================================================================

# Load configuration and data models
if (!exists("config")) source("src/00_config.R")
source("src/data_models.R")

# ------------------------------------------------------------------------------
# CORE PARSING FUNCTIONS
# ------------------------------------------------------------------------------

#' Parse a single WOS reference string into structured fields
#'
#' WOS references follow the pattern:
#'   "Author Year Page Citations Title Journal"
#'
#' Example:
#'   "Diepenmaat-Wolters, MGE 1997 147 55 High-performance anion-exchange..."
#'
#' @param reference Character string containing WOS reference
#' @return Named list with parsed fields
#'
#' @examples
#' ref <- "Diepenmaat-Wolters, MGE 1997 147 55 High-performance... JOURNAL NAME"
#' parse_wos_reference(ref)
parse_wos_reference <- function(reference) {

  # Default result for failed parsing
  default_result <- list(
    author = NA_character_,
    year = NA_integer_,
    page = NA_integer_,
    citations = NA_integer_,
    title = NA_character_,
    journal = NA_character_,
    parse_status = "failed"
  )

  # Handle NULL, NA, or empty input
  if (is.null(reference) || is.na(reference) || nchar(trimws(reference)) == 0) {
    return(default_result)
  }

  reference <- trimws(reference)

  # Pattern explanation:
  # ^(.+?)\s+          - Author: greedy minimal match for author name(s)
  # (\d{4})\s+         - Year: exactly 4 digits
  # (\d+)\s+           - Page: one or more digits
  # (\d+)\s+           - Citations: one or more digits
  # (.+?)\s+           - Title: greedy minimal match
  # ([A-Z][A-Z\s&\-:]+)$ - Journal: uppercase words at end

  # Try primary pattern with all fields
  pattern <- paste0(
    "^(.+?)\\s+",            # Author
    "(\\d{4})\\s+",          # Year
    "(\\d+)\\s+",            # Page
    "(\\d+)\\s+",            # Citations
    "(.+?)\\s+",             # Title
    "([A-Z][A-Z0-9\\s&\\-:,]+)$"  # Journal (uppercase at end)
  )

  match <- str_match(reference, pattern)

  if (!is.na(match[1, 1])) {
    return(list(
      author = trimws(match[1, 2]),
      year = as.integer(match[1, 3]),
      page = as.integer(match[1, 4]),
      citations = as.integer(match[1, 5]),
      title = trimws(match[1, 6]),
      journal = trimws(match[1, 7]),
      parse_status = "complete"
    ))
  }

  # Fallback: Try simpler pattern without page/citations
  pattern_simple <- paste0(
    "^(.+?)\\s+",            # Author
    "(\\d{4})\\s+",          # Year
    "(.+?)\\s+",             # Title
    "([A-Z][A-Z0-9\\s&\\-:,]+)$"  # Journal
  )

  match <- str_match(reference, pattern_simple)

  if (!is.na(match[1, 1])) {
    return(list(
      author = trimws(match[1, 2]),
      year = as.integer(match[1, 3]),
      page = NA_integer_,
      citations = NA_integer_,
      title = trimws(match[1, 4]),
      journal = trimws(match[1, 5]),
      parse_status = "partial"
    ))
  }

  # Second fallback: Extract what we can
  result <- default_result

  # Try to extract year
  year_match <- str_extract(reference, "\\b(19|20)\\d{2}\\b")
  if (!is.na(year_match)) {
    result$year <- as.integer(year_match)
  }

  # Try to extract author (text before year)
  if (!is.na(result$year)) {
    author_match <- str_extract(reference, paste0("^.+?(?=\\s+", result$year, ")"))
    if (!is.na(author_match)) {
      result$author <- trimws(author_match)
    }
  }

  # Try to extract journal (uppercase text at end)
  journal_match <- str_extract(reference, "[A-Z][A-Z0-9\\s&\\-:,]{10,}$")
  if (!is.na(journal_match)) {
    result$journal <- trimws(journal_match)
  }

  # Title is whatever remains
  if (!is.na(result$author) && !is.na(result$year) && !is.na(result$journal)) {
    # Remove known parts to get title
    remaining <- reference
    remaining <- str_remove(remaining, paste0("^", fixed(result$author)))
    remaining <- str_remove(remaining, paste0("\\s*", result$year, "\\s*"))
    remaining <- str_remove(remaining, paste0(fixed(result$journal), "$"))
    remaining <- str_remove_all(remaining, "^\\s*\\d+\\s+\\d+\\s*")  # Remove page/citations
    result$title <- trimws(remaining)
    result$parse_status <- "partial"
  }

  return(result)
}


#' Parse WOS tab-separated file line
#'
#' Each line contains: WOS_ID<TAB>Reference_String
#'
#' @param line Single line from WOS file
#' @return Named list with wos_id and parsed reference fields
parse_wos_line <- function(line) {

  # Split on tab
  parts <- str_split(line, "\t", n = 2)[[1]]

  if (length(parts) < 2) {
    return(list(
      wos_id = NA_character_,
      author = NA_character_,
      year = NA_integer_,
      page = NA_integer_,
      citations = NA_integer_,
      title = NA_character_,
      journal = NA_character_,
      reference = line,
      parse_status = "failed"
    ))
  }

  wos_id <- trimws(parts[1])
  reference <- trimws(parts[2])

  # Parse the reference string
  parsed <- parse_wos_reference(reference)

  # Combine with ID
  c(
    list(wos_id = wos_id, reference = reference),
    parsed
  )
}


# ------------------------------------------------------------------------------
# BATCH PROCESSING
# ------------------------------------------------------------------------------

#' Load and parse WOS tab-separated file
#'
#' @param file_path Path to WOS data file
#' @param has_header Does file have header row? Default FALSE
#' @param progress Show progress? Default TRUE
#' @return Tibble with parsed WOS records
#'
#' @examples
#' wos_data <- load_wos_tsv("data/raw/wos/wos_export.txt")
load_wos_tsv <- function(file_path, has_header = TRUE, progress = TRUE) {

  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  message("Loading WOS data from: ", basename(file_path))

  # Read all lines
  lines <- read_lines(file_path)

  if (length(lines) == 0) {
    warning("Empty file: ", file_path)
    return(create_wos_schema())
  }

  # Skip header if present
  if (has_header) {
    header <- lines[1]
    lines <- lines[-1]
    message("  Header: ", substr(header, 1, 50), "...")
  }

  message("  Processing ", length(lines), " records...")

  # Parse each line
  if (progress && length(lines) > 100) {
    results <- map(lines, parse_wos_line, .progress = TRUE)
  } else {
    results <- map(lines, parse_wos_line)
  }

  # Convert to tibble
  wos_data <- results %>%
    map(as_tibble_row) %>%
    bind_rows()

  # Ensure correct types
  wos_data <- wos_data %>%
    mutate(
      year = as.integer(year),
      page = as.integer(page),
      citations = as.integer(citations)
    )

  # Report parsing results
  status_counts <- table(wos_data$parse_status)
  message("\nParsing results:")
  for (status in names(status_counts)) {
    message(sprintf("  %s: %d (%.1f%%)",
                    status,
                    status_counts[status],
                    100 * status_counts[status] / nrow(wos_data)))
  }

  return(wos_data)
}


#' Load and parse WOS CSV file (alternative format)
#'
#' For CSV files with ID and reference columns
#'
#' @param file_path Path to WOS CSV file
#' @param id_col Name of ID column
#' @param ref_col Name of reference column
#' @return Tibble with parsed WOS records
load_wos_csv <- function(file_path,
                         id_col = "ID",
                         ref_col = "reference") {

  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  message("Loading WOS CSV from: ", basename(file_path))

  # Read CSV
  raw_data <- read_csv(file_path, show_col_types = FALSE)

  # Validate columns
  if (!id_col %in% names(raw_data)) {
    warning("ID column '", id_col, "' not found. Using row numbers.")
    raw_data[[id_col]] <- paste0("ROW:", seq_len(nrow(raw_data)))
  }

  if (!ref_col %in% names(raw_data)) {
    stop("Reference column '", ref_col, "' not found. Available: ",
         paste(names(raw_data), collapse = ", "))
  }

  message("  Processing ", nrow(raw_data), " records...")

  # Parse each reference
  parsed <- map(raw_data[[ref_col]], parse_wos_reference, .progress = TRUE)

  # Combine with IDs
  wos_data <- tibble(
    wos_id = raw_data[[id_col]],
    reference = raw_data[[ref_col]]
  ) %>%
    bind_cols(bind_rows(parsed))

  # Report results
  status_counts <- table(wos_data$parse_status)
  message("\nParsing results:")
  for (status in names(status_counts)) {
    message(sprintf("  %s: %d (%.1f%%)",
                    status,
                    status_counts[status],
                    100 * status_counts[status] / nrow(wos_data)))
  }

  return(wos_data)
}


#' Load all WOS files from directory
#'
#' Supports both .txt (TSV) and .csv formats
#'
#' @param path Directory containing WOS files
#' @param pattern File pattern to match
#' @return Combined tibble with all parsed WOS records
load_wos_directory <- function(path = config$paths$wos_raw,
                               pattern = "\\.(txt|csv)$") {

  if (!dir.exists(path)) {
    stop("Directory not found: ", path)
  }

  files <- list.files(path, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    warning("No WOS files found in: ", path)
    return(create_wos_schema())
  }

  message("Found ", length(files), " WOS file(s)")

  # Process each file
  all_data <- map(files, function(f) {
    message("\n", strrep("-", 40))

    if (str_detect(f, "\\.csv$")) {
      load_wos_csv(f)
    } else {
      load_wos_tsv(f)
    }
  }) %>%
    bind_rows()

  message("\n", strrep("=", 40))
  message("Total WOS records loaded: ", nrow(all_data))

  return(all_data)
}


# ------------------------------------------------------------------------------
# VALIDATION AND DIAGNOSTICS
# ------------------------------------------------------------------------------

#' Check WOS reference for common issues
#'
#' @param reference Reference string
#' @return Character vector of detected issues
diagnose_wos_reference <- function(reference) {

  issues <- character()

  if (is.na(reference) || nchar(reference) == 0) {
    return("Empty reference")
  }

  # Check for missing year
  if (!str_detect(reference, "\\b(19|20)\\d{2}\\b")) {
    issues <- c(issues, "No valid year found")
  }

  # Check for uppercase journal at end
  if (!str_detect(reference, "[A-Z]{3,}[A-Z\\s]+$")) {
    issues <- c(issues, "No uppercase journal name at end")
  }

  # Check for numeric fields (page, citations)
  numbers <- str_extract_all(reference, "\\b\\d+\\b")[[1]]
  if (length(numbers) < 3) {
    issues <- c(issues, "Missing numeric fields (page/citations)")
  }

  # Check reference length
  if (nchar(reference) < 50) {
    issues <- c(issues, "Reference unusually short")
  }

  if (length(issues) == 0) {
    return("No issues detected")
  }

  return(issues)
}


#' Generate parsing diagnostics report
#'
#' @param wos_data Parsed WOS tibble
#' @return List with diagnostic information
wos_parsing_diagnostics <- function(wos_data) {

  report <- list(
    total_records = nrow(wos_data),

    parse_status = as.list(table(wos_data$parse_status)),

    field_completeness = list(
      wos_id = mean(!is.na(wos_data$wos_id)),
      author = mean(!is.na(wos_data$author)),
      year = mean(!is.na(wos_data$year)),
      page = mean(!is.na(wos_data$page)),
      citations = mean(!is.na(wos_data$citations)),
      title = mean(!is.na(wos_data$title)),
      journal = mean(!is.na(wos_data$journal))
    ),

    year_range = list(
      min = min(wos_data$year, na.rm = TRUE),
      max = max(wos_data$year, na.rm = TRUE),
      median = median(wos_data$year, na.rm = TRUE)
    ),

    title_lengths = list(
      min = min(nchar(wos_data$title), na.rm = TRUE),
      max = max(nchar(wos_data$title), na.rm = TRUE),
      median = median(nchar(wos_data$title), na.rm = TRUE)
    )
  )

  return(report)
}


#' Print WOS parsing diagnostics
#'
#' @param wos_data Parsed WOS tibble
print_wos_diagnostics <- function(wos_data) {

  report <- wos_parsing_diagnostics(wos_data)

  message("\n", strrep("=", 60))
  message("WOS Parsing Diagnostics")
  message(strrep("=", 60))

  message("\nTotal records: ", report$total_records)

  message("\nParsing status:")
  for (status in names(report$parse_status)) {
    pct <- 100 * report$parse_status[[status]] / report$total_records
    message(sprintf("  %-12s: %6d (%5.1f%%)", status, report$parse_status[[status]], pct))
  }

  message("\nField completeness:")
  for (field in names(report$field_completeness)) {
    pct <- 100 * report$field_completeness[[field]]
    message(sprintf("  %-12s: %5.1f%%", field, pct))
  }

  message("\nYear range: ", report$year_range$min, " - ", report$year_range$max,
          " (median: ", report$year_range$median, ")")

  message("\nTitle length: ", report$title_lengths$min, " - ", report$title_lengths$max,
          " chars (median: ", report$title_lengths$median, ")")

  invisible(report)
}


# ------------------------------------------------------------------------------
# SAMPLE DATA AND TESTING
# ------------------------------------------------------------------------------

#' Get sample WOS data for testing
#'
#' @return Character vector of sample WOS lines
get_wos_samples <- function() {
  c(
    "WOS:000060312800003\tDiepenmaat-Wolters, MGE 1997 147 55 High-performance anion-exchange chromatography method for analysis of propylene glycol alginate in beer JOURNAL OF THE AMERICAN SOCIETY OF BREWING CHEMISTS",
    "WOS:000077689500013\tKlessmann, V 2000 217 4 Practical theology as theory of praxis in church and society INTERNATIONAL JOURNAL OF PRACTICAL THEOLOGY",
    "WOS:000087052800001\tDalgard, O 2002 25 68 Community health profiles INTERNATIONAL JOURNAL OF EPIDEMIOLOGY",
    "WOS:000070939000013\tLercher, P 1997 2502 10 Birth weight, education, environment, and lung function at school age: a community study in an alpine area EUROPEAN RESPIRATORY JOURNAL",
    "WOS:000077316700006\tFarrow, CE 1998 1 105 PDFfit, a program for full profile structural refinement of the atomic pair distribution function JOURNAL OF PHYSICS-CONDENSED MATTER",
    "WOS:000079215100007\tBartos, L 1999 1 25 Social position and response to novel objects and open spaces in red deer hinds BEHAVIOUR"
  )
}


#' Test WOS parser with sample data
#'
#' @return Tibble with parsed sample data
test_wos_parser <- function() {

  samples <- get_wos_samples()

  message("Testing WOS parser with ", length(samples), " samples...")

  # Parse samples
  results <- map(samples, parse_wos_line) %>%
    map(as_tibble_row) %>%
    bind_rows()

  # Show results
  message("\nParsed results:")
  print(results %>% select(wos_id, author, year, page, citations, parse_status))

  return(results)
}


# ------------------------------------------------------------------------------
# MAIN EXECUTION (if run as script)
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  message("\n", strrep("=", 60))
  message("WOS Parser Module")
  message(strrep("=", 60), "\n")

  message("Available functions:")
  message("  - parse_wos_reference()   Parse single reference string")
  message("  - parse_wos_line()        Parse TSV line with ID")
  message("  - load_wos_tsv()          Load tab-separated WOS file")
  message("  - load_wos_csv()          Load CSV format WOS file")
  message("  - load_wos_directory()    Load all WOS files from directory")
  message("  - print_wos_diagnostics() Show parsing statistics")

  message("\nRunning parser test...")
  test_results <- test_wos_parser()
}
