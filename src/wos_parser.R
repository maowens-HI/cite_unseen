# ==============================================================================
# WOS PARSER
# Parse Web of Science reference data
# ==============================================================================
#
# WOS files are tab-separated:
#   WOS_ID <TAB> Author Year Page Citations Title JOURNAL NAME
#
# Example:
#   WOS:000060312800003	Smith, A 1997 147 55 Some title NATURE
#
# ==============================================================================

if (!exists("config")) source("src/00_config.R")
source("src/data_models.R")

# ==============================================================================
# PARSE A SINGLE REFERENCE
# ==============================================================================

#' Parse a WOS reference string
#'
#' @param reference The reference text (after the tab)
#' @return List with: author, year, journal
parse_wos_reference <- function(reference) {
  if (is.na(reference) || nchar(trimws(reference)) == 0) {
    return(list(author = NA, year = NA, journal = NA))
  }

  reference <- trimws(reference)

  # Extract year (4 digits starting with 19 or 20)
  year <- as.integer(str_extract(reference, "\\b(19|20)\\d{2}\\b"))

  # Extract author (text before the year)
  author <- NA
  if (!is.na(year)) {
    author_match <- str_extract(reference, paste0("^.+?(?=\\s+", year, ")"))
    if (!is.na(author_match)) author <- trimws(author_match)
  }

  # Extract journal (ALL CAPS at the end, at least 10 chars)
  journal <- str_extract(reference, "[A-Z][A-Z0-9\\s&\\-:,]{10,}$")
  if (!is.na(journal)) journal <- trimws(journal)

  list(author = author, year = year, journal = journal)
}

# ==============================================================================
# FILE LOADING
# ==============================================================================

#' Load a WOS file
#'
#' @param file_path Path to the file
#' @return Tibble with parsed records
load_wos_file <- function(file_path) {
  if (!file.exists(file_path)) stop("File not found: ", file_path)

  message("Loading: ", basename(file_path))
  lines <- read_lines(file_path)

  # Skip header if present
  if (length(lines) > 0 && str_detect(lines[1], "^ID\\t|^wos_id\\t|^WOS")) {
    lines <- lines[-1]
  }

  message("  Parsing ", length(lines), " records...")

  results <- map(lines, function(line) {
    parts <- str_split(line, "\t", n = 2)[[1]]
    if (length(parts) < 2) {
      return(tibble(wos_id = NA, author = NA, year = NA,
                    journal = NA, reference = line))
    }

    parsed <- parse_wos_reference(parts[2])
    tibble(
      wos_id    = trimws(parts[1]),
      author    = parsed$author,
      year      = parsed$year,
      journal   = parsed$journal,
      reference = trimws(parts[2])
    )
  })

  bind_rows(results)
}

#' Load all WOS files from a directory
#'
#' @param path Directory path
#' @return Combined tibble
load_wos_directory <- function(path = config$paths$wos_raw) {
  if (!dir.exists(path)) stop("Directory not found: ", path)

  files <- list.files(path, pattern = "\\.(txt|csv)$", full.names = TRUE)

  if (length(files) == 0) {
    warning("No WOS files in: ", path)
    return(create_wos_tibble())
  }

  message("Found ", length(files), " WOS file(s)")
  all_data <- map(files, load_wos_file) %>% bind_rows()
  message("Total: ", nrow(all_data), " records")

  all_data
}

# ==============================================================================
# TEST
# ==============================================================================

if (sys.nframe() == 0) {
  message("Testing WOS parser...")

  samples <- c(
    "Diepenmaat-Wolters, MGE 1997 147 55 Some title JOURNAL OF BREWING",
    "Klessmann, V 2000 217 4 Practical theology INTERNATIONAL JOURNAL"
  )

  for (s in samples) {
    result <- parse_wos_reference(s)
    message("  ", result$author, " (", result$year, ") -> ", result$journal)
  }
}
