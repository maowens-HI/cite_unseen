# ==============================================================================
# DATA INGESTION
# Load and validate raw data from WOS and NBER sources
# ==============================================================================

# Load configuration
if (!exists("config")) source("src/00_config.R")

# ------------------------------------------------------------------------------
# WOS DATA LOADING
# ------------------------------------------------------------------------------

#' Load all WOS CSV files from the raw data directory
#'
#' @param path Directory containing WOS CSV files
#' @param pattern File pattern to match (default: "*.csv")
#' @return Tibble with combined WOS data
#'
#' @examples
#' wos_data <- load_wos_corpus("data/raw/wos")
load_wos_corpus <- function(path = config$paths$wos_raw,
                            pattern = "\\.csv$") {

  # Validate path exists
  if (!dir.exists(path)) {
    stop("WOS directory does not exist: ", path)
  }

  # Find all matching files
  files <- list.files(path, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    warning("No CSV files found in: ", path)
    return(tibble())
  }

  message("Loading ", length(files), " WOS file(s)...")

  # Load and combine all files
  wos_data <- files %>%
    map(function(f) {
      message("  - ", basename(f))
      read_csv(f, show_col_types = FALSE)
    }) %>%
    bind_rows()

  message("Loaded ", nrow(wos_data), " WOS records")

  # Basic validation
  if (!"reference" %in% names(wos_data)) {
    warning("Expected 'reference' column not found. Available columns: ",
            paste(names(wos_data), collapse = ", "))
  }

  return(wos_data)
}

#' Load a single WOS CSV file
#'
#' @param file_path Path to WOS CSV file
#' @return Tibble with WOS data
load_wos_file <- function(file_path) {

  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }

  wos_data <- read_csv(file_path, show_col_types = FALSE)

  message("Loaded ", nrow(wos_data), " records from ", basename(file_path))

  return(wos_data)
}

# ------------------------------------------------------------------------------
# NBER DATA LOADING
# ------------------------------------------------------------------------------

#' Load NBER paper text files
#'
#' @param path Directory containing NBER text files
#' @param pattern File pattern to match (default: "w*.txt")
#' @return Tibble with paper_id and text content
#'
#' @examples
#' nber_data <- load_nber_papers("data/raw/nber")
load_nber_papers <- function(path = config$paths$nber_raw,
                             pattern = "^w\\d+\\.txt$") {

  # Validate path exists
  if (!dir.exists(path)) {
    stop("NBER directory does not exist: ", path)
  }

  # Find all matching files
  files <- list.files(path, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    warning("No NBER text files found in: ", path)
    return(tibble(paper_id = character(), text = character()))
  }

  message("Loading ", length(files), " NBER paper(s)...")

  # Load all papers
  nber_data <- tibble(
    file_path = files,
    paper_id = str_extract(basename(files), "w\\d+"),
    text = map_chr(files, read_file_safe)
  )

  # Remove papers that failed to load
  failed <- sum(is.na(nber_data$text))
  if (failed > 0) {
    warning(failed, " papers failed to load")
    nber_data <- filter(nber_data, !is.na(text))
  }

  message("Successfully loaded ", nrow(nber_data), " papers")

  return(select(nber_data, paper_id, text))
}

#' Safely read a text file
#'
#' @param path File path
#' @return File contents or NA if read fails
read_file_safe <- function(path) {
  tryCatch(
    read_file(path),
    error = function(e) {
      warning("Failed to read: ", path, " - ", e$message)
      NA_character_
    }
  )
}

#' Load a single NBER paper
#'
#' @param paper_id NBER paper ID (e.g., "w31280")
#' @param path Directory containing NBER files
#' @return Character string with paper text
load_nber_paper <- function(paper_id, path = config$paths$nber_raw) {

  # Try .txt first, then .pdf
  txt_path <- file.path(path, paste0(paper_id, ".txt"))
  pdf_path <- file.path(path, paste0(paper_id, ".pdf"))

  if (file.exists(txt_path)) {
    return(read_file(txt_path))
  } else if (file.exists(pdf_path)) {
    # Requires pdftools package
    if (!requireNamespace("pdftools", quietly = TRUE)) {
      stop("pdftools package required for PDF reading")
    }
    return(paste(pdftools::pdf_text(pdf_path), collapse = "\n"))
  } else {
    stop("Paper not found: ", paper_id)
  }
}

# ------------------------------------------------------------------------------
# DATA VALIDATION
# ------------------------------------------------------------------------------

#' Validate WOS data structure
#'
#' @param data WOS tibble
#' @return TRUE if valid, stops with error if not
validate_wos_data <- function(data) {

  # Check for required columns
  required_cols <- c("reference")
  missing <- setdiff(required_cols, names(data))

  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  # Check for empty data
  if (nrow(data) == 0) {
    stop("WOS data is empty")
  }

  # Check for NA references
  na_count <- sum(is.na(data$reference))
  if (na_count > 0) {
    warning(na_count, " NA values in reference column (",
            round(100 * na_count / nrow(data), 1), "%)")
  }

  # Report statistics
  message("WOS data validation passed:")
  message("  - ", nrow(data), " total records")
  message("  - ", sum(!is.na(data$reference)), " valid references")

  invisible(TRUE)
}

#' Validate NBER data structure
#'
#' @param data NBER tibble
#' @return TRUE if valid, stops with error if not
validate_nber_data <- function(data) {

  # Check for required columns
  required_cols <- c("paper_id", "text")
  missing <- setdiff(required_cols, names(data))

  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  # Check for empty data
  if (nrow(data) == 0) {
    stop("NBER data is empty")
  }

  # Check for empty texts
  empty_count <- sum(nchar(data$text) < 100)
  if (empty_count > 0) {
    warning(empty_count, " papers have very short text (< 100 chars)")
  }

  # Report statistics
  message("NBER data validation passed:")
  message("  - ", nrow(data), " total papers")
  message("  - Median text length: ", median(nchar(data$text)), " characters")

  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# MAIN EXECUTION (if run as script)
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  message("\n", strrep("=", 60))
  message("Running Data Ingestion")
  message(strrep("=", 60), "\n")

  # Load WOS data
  wos_data <- load_wos_corpus()
  if (nrow(wos_data) > 0) validate_wos_data(wos_data)

  # Load NBER data
  nber_data <- load_nber_papers()
  if (nrow(nber_data) > 0) validate_nber_data(nber_data)

  message("\nData ingestion complete.")
}
