# ==============================================================================
# DATA INGESTION
# Load and validate raw data from WOS and NBER sources
# ==============================================================================

# Load configuration and specialized parsers
if (!exists("config")) source("src/00_config.R")
source("src/data_models.R")
source("src/wos_parser.R")
source("src/nber_parser.R")

# ------------------------------------------------------------------------------
# WOS DATA LOADING
# ------------------------------------------------------------------------------
# WOS data format: Tab-separated files with ID and reference columns
# Reference format: "Author Year Page Citations Title Journal"
#
# Example line:
#   WOS:000060312800003	Diepenmaat-Wolters, MGE 1997 147 55 High-performance... JOURNAL NAME
# ------------------------------------------------------------------------------

#' Load all WOS files from the raw data directory with parsing
#'
#' Supports both TSV (.txt) and CSV (.csv) formats.
#' Uses structured parsing to extract author, year, page, citations,
#' title, and journal from reference strings.
#'
#' @param path Directory containing WOS files
#' @param pattern File pattern to match (default: txt and csv files)
#' @param parse_references Parse reference strings into fields? Default TRUE
#' @return Tibble with parsed WOS data
#'
#' @examples
#' wos_data <- load_wos_corpus("data/raw/wos")
load_wos_corpus <- function(path = config$paths$wos_raw,
                            pattern = "\\.(txt|csv)$",
                            parse_references = TRUE) {

  # Validate path exists
  if (!dir.exists(path)) {
    stop("WOS directory does not exist: ", path)
  }

  # Find all matching files
  files <- list.files(path, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    warning("No WOS files found in: ", path)
    return(create_wos_schema())
  }

  message("Loading ", length(files), " WOS file(s) with structured parsing...")

  # Use the new parser module
  wos_data <- load_wos_directory(path, pattern)

  message("Loaded ", nrow(wos_data), " WOS records")

  # Show parsing diagnostics
  if (nrow(wos_data) > 0) {
    print_wos_diagnostics(wos_data)
  }

  return(wos_data)
}

#' Load a single WOS file with parsing
#'
#' @param file_path Path to WOS file (TSV or CSV)
#' @return Tibble with parsed WOS data
load_wos_file <- function(file_path) {

  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }

  # Detect format and use appropriate loader
  if (str_detect(file_path, "\\.csv$")) {
    wos_data <- load_wos_csv(file_path)
  } else {
    wos_data <- load_wos_tsv(file_path)
  }

  message("Loaded ", nrow(wos_data), " records from ", basename(file_path))

  return(wos_data)
}

#' Load WOS data without parsing (raw format)
#'
#' For cases where structured parsing is not needed.
#'
#' @param path Directory containing WOS files
#' @return Tibble with raw reference strings
load_wos_raw <- function(path = config$paths$wos_raw) {

  files <- list.files(path, pattern = "\\.(txt|csv)$", full.names = TRUE)

  if (length(files) == 0) {
    warning("No WOS files found in: ", path)
    return(tibble(wos_id = character(), reference = character()))
  }

  all_data <- map(files, function(f) {
    lines <- read_lines(f)
    if (length(lines) > 0 && str_detect(lines[1], "^(ID|WOS)")) {
      lines <- lines[-1]  # Skip header
    }

    tibble(raw = lines) %>%
      separate(raw, into = c("wos_id", "reference"), sep = "\t", extra = "merge", fill = "right")
  }) %>%
    bind_rows()

  return(all_data)
}

# ------------------------------------------------------------------------------
# NBER DATA LOADING
# ------------------------------------------------------------------------------
# NBER papers contain bibliography sections with formatted citations.
# Citation format: Authors (Year): "Title," Publication, Volume, Pages.
#
# Example citation:
#   Barro, R. and J. Furman (2018): "Macroeconomic Effects," BPEA, 49, 257-345.
# ------------------------------------------------------------------------------

#' Load NBER paper text files
#'
#' @param path Directory containing NBER text files
#' @param pattern File pattern to match (default: "w*.txt" or "w*.pdf")
#' @return Tibble with paper_id and text content
#'
#' @examples
#' nber_data <- load_nber_papers("data/raw/nber")
load_nber_papers <- function(path = config$paths$nber_raw,
                             pattern = "^w\\d+\\.(txt|pdf)$") {

  # Validate path exists
  if (!dir.exists(path)) {
    stop("NBER directory does not exist: ", path)
  }

  # Find all matching files
  files <- list.files(path, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    warning("No NBER paper files found in: ", path)
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

#' Safely read a text or PDF file
#'
#' @param path File path
#' @return File contents or NA if read fails
read_file_safe <- function(path) {
  tryCatch({
    if (str_detect(path, "\\.pdf$")) {
      # Read PDF
      if (!requireNamespace("pdftools", quietly = TRUE)) {
        warning("pdftools package required for PDF reading: ", path)
        return(NA_character_)
      }
      paste(pdftools::pdf_text(path), collapse = "\n")
    } else {
      # Read text file
      read_file(path)
    }
  }, error = function(e) {
    warning("Failed to read: ", path, " - ", e$message)
    NA_character_
  })
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

#' Load and parse NBER paper citations
#'
#' Combines paper loading with bibliography extraction and parsing.
#'
#' @param path Directory containing NBER files
#' @param pattern File pattern to match
#' @return Tibble with parsed citations from all papers
#'
#' @examples
#' nber_citations <- load_nber_citations("data/raw/nber")
load_nber_citations <- function(path = config$paths$nber_raw,
                                pattern = "^w\\d+\\.(txt|pdf)$") {

  # First load the papers
  papers <- load_nber_papers(path, pattern)

  if (nrow(papers) == 0) {
    warning("No papers loaded")
    return(create_nber_schema())
  }

  message("\nExtracting and parsing citations...")

  # Extract and parse citations from each paper
  all_citations <- map2(
    papers$paper_id,
    papers$text,
    function(pid, txt) {
      tryCatch(
        extract_and_parse_paper(pid, txt),
        error = function(e) {
          warning("Failed to process ", pid, ": ", e$message)
          create_nber_schema() %>% mutate(paper_id = pid)
        }
      )
    },
    .progress = TRUE
  ) %>%
    bind_rows()

  # Show diagnostics
  if (nrow(all_citations) > 0) {
    print_nber_diagnostics(all_citations)
  }

  return(all_citations)
}

# ------------------------------------------------------------------------------
# DATA VALIDATION
# ------------------------------------------------------------------------------

#' Validate WOS data structure
#'
#' Works with both raw (reference only) and parsed (all fields) formats.
#'
#' @param data WOS tibble
#' @return TRUE if valid, stops with error if not
validate_wos_data <- function(data) {

  # Check for required columns (at minimum need wos_id and reference)
  required_cols <- c("wos_id", "reference")
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

  # If parsed, show parsing stats
  if ("parse_status" %in% names(data)) {
    status_tbl <- table(data$parse_status)
    message("  - Parsing status:")
    for (s in names(status_tbl)) {
      message("      ", s, ": ", status_tbl[s])
    }
  }

  invisible(TRUE)
}

#' Validate NBER paper data structure
#'
#' @param data NBER papers tibble
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

#' Validate NBER parsed citations
#'
#' @param data NBER citations tibble (parsed format)
#' @return TRUE if valid, stops with error if not
validate_nber_citations <- function(data) {

  # Check for required columns from parsed schema
  required_cols <- c("paper_id", "citation_num", "year", "raw_citation")
  missing <- setdiff(required_cols, names(data))

  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  if (nrow(data) == 0) {
    stop("NBER citations data is empty")
  }

  # Validate with schema validator
  validation <- validate_nber_schema(data)

  message("NBER citations validation passed:")
  message("  - ", validation$n_records, " citations from ", validation$n_papers, " papers")

  if (length(validation$warnings) > 0) {
    for (w in validation$warnings) {
      warning(w)
    }
  }

  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# MAIN EXECUTION (if run as script)
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  message("\n", strrep("=", 60))
  message("Running Data Ingestion")
  message(strrep("=", 60), "\n")

  # Load and parse WOS data
  message("\n--- WOS Data ---")
  wos_data <- load_wos_corpus()
  if (nrow(wos_data) > 0) validate_wos_data(wos_data)

  # Load NBER papers
  message("\n--- NBER Papers ---")
  nber_papers <- load_nber_papers()
  if (nrow(nber_papers) > 0) validate_nber_data(nber_papers)

  # Optionally parse NBER citations
  # nber_citations <- load_nber_citations()
  # if (nrow(nber_citations) > 0) validate_nber_citations(nber_citations)

  message("\nData ingestion complete.")
  message("Available functions:")
  message("  - load_wos_corpus()       Load and parse WOS files")
  message("  - load_wos_file()         Load single WOS file")
  message("  - load_nber_papers()      Load NBER paper texts")
  message("  - load_nber_citations()   Load and parse NBER citations")
}
