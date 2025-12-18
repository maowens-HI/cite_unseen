# ==============================================================================
# WOS PARSER
# Parse Web of Science reference data
# ==============================================================================
#
# WOS files are tab-separated with format:
#   WOS_ID <TAB> Author Year Page Citations Title JOURNAL NAME
#
# Example:
#   WOS:000060312800003	Diepenmaat-Wolters 1997 147 55 High-performance... JOURNAL OF BREWING
#
# ==============================================================================

if (!exists("config")) source("src/00_config.R")
source("src/data_models.R")

# ------------------------------------------------------------------------------
# MAIN PARSING FUNCTION
# ------------------------------------------------------------------------------

#' Parse a single WOS reference string
#'
#' @param reference The reference string (everything after the tab)
#' @return Named list with: author, year, title, journal
parse_wos_reference <- function(reference) {

  # Return NAs if empty
  if (is.na(reference) || nchar(trimws(reference)) == 0) {
    return(list(author = NA, year = NA, title = NA, journal = NA))
  }

  reference <- trimws(reference)

  # Try the full pattern: Author Year Page Citations Title JOURNAL
  # Journal names are typically ALL CAPS at the end
  pattern <- "^(.+?)\\s+(\\d{4})\\s+\\d+\\s+\\d+\\s+(.+?)\\s+([A-Z][A-Z0-9\\s&\\-:,]+)$"
  match <- str_match(reference, pattern)

  if (!is.na(match[1, 1])) {
    return(list(
      author  = trimws(match[1, 2]),
      year    = as.integer(match[1, 3]),
      title   = trimws(match[1, 4]),
      journal = trimws(match[1, 5])
    ))
  }

  # Fallback: just extract what we can
  year <- as.integer(str_extract(reference, "\\b(19|20)\\d{2}\\b"))
  journal <- str_extract(reference, "[A-Z][A-Z0-9\\s&\\-:,]{10,}$")

  # Author is text before year
  author <- NA
  if (!is.na(year)) {
    author_match <- str_extract(reference, paste0("^.+?(?=\\s+", year, ")"))
    if (!is.na(author_match)) author <- trimws(author_match)
  }

  return(list(
    author  = author,
    year    = year,
    title   = NA,  # Can't reliably extract without full pattern
    journal = if (!is.na(journal)) trimws(journal) else NA
  ))
}

# ------------------------------------------------------------------------------
# FILE LOADING
# ------------------------------------------------------------------------------

#' Load WOS data from a tab-separated file
#'
#' @param file_path Path to the WOS file
#' @return Tibble with parsed WOS records
load_wos_file <- function(file_path) {

  if (!file.exists(file_path)) stop("File not found: ", file_path)

  message("Loading WOS data from: ", basename(file_path))
  lines <- read_lines(file_path)

  # Skip header if present (first line usually has column names)
  if (length(lines) > 0 && str_detect(lines[1], "^ID\\t|^wos_id\\t|^WOS")) {
    lines <- lines[-1]
  }

  message("  Parsing ", length(lines), " records...")

  # Parse each line
  results <- map(lines, function(line) {
    parts <- str_split(line, "\t", n = 2)[[1]]
    if (length(parts) < 2) {
      return(tibble(wos_id = NA, author = NA, year = NA, title = NA, journal = NA, reference = line))
    }

    parsed <- parse_wos_reference(parts[2])
    tibble(
      wos_id    = trimws(parts[1]),
      author    = parsed$author,
      year      = parsed$year,
      title     = parsed$title,
      journal   = parsed$journal,
      reference = trimws(parts[2])
    )
  })

  wos_data <- bind_rows(results)

  # Report how it went
  n_complete <- sum(!is.na(wos_data$year) & !is.na(wos_data$author))
  message(sprintf("  Parsed: %d complete, %d partial", n_complete, nrow(wos_data) - n_complete))

  return(wos_data)
}

#' Load all WOS files from a directory
#'
#' @param path Directory containing WOS files
#' @return Combined tibble
load_wos_directory <- function(path = config$paths$wos_raw) {

  if (!dir.exists(path)) stop("Directory not found: ", path)

  files <- list.files(path, pattern = "\\.(txt|csv)$", full.names = TRUE)

  if (length(files) == 0) {
    warning("No WOS files found in: ", path)
    return(create_wos_tibble())
  }

  message("Found ", length(files), " WOS file(s)")

  all_data <- map(files, load_wos_file) %>% bind_rows()
  message("Total WOS records: ", nrow(all_data))

  return(all_data)
}

# ------------------------------------------------------------------------------
# TEST WITH SAMPLE DATA
# ------------------------------------------------------------------------------

test_wos_parser <- function() {
  samples <- c(
    "WOS:000060312800003\tDiepenmaat-Wolters, MGE 1997 147 55 High-performance chromatography JOURNAL OF BREWING",
    "WOS:000077689500013\tKlessmann, V 2000 217 4 Practical theology INTERNATIONAL JOURNAL OF THEOLOGY"
  )

  message("Testing WOS parser...")
  for (s in samples) {
    parts <- str_split(s, "\t")[[1]]
    result <- parse_wos_reference(parts[2])
    message("  ", parts[1], " -> year=", result$year, ", author=", result$author)
  }
}

if (sys.nframe() == 0) test_wos_parser()
