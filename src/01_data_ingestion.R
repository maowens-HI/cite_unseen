# ==============================================================================
# DATA INGESTION
# Load raw data from WOS and NBER sources
# ==============================================================================
#
# This script loads:
#   1. WOS (Web of Science) reference data - the "ground truth" of real papers
#   2. NBER paper texts - from which we extract bibliography citations
#
# ==============================================================================

if (!exists("config")) source("src/00_config.R")
source("src/data_models.R")
source("src/wos_parser.R")
source("src/nber_parser.R")
source("src/utils.R")

# ------------------------------------------------------------------------------
# WOS DATA LOADING
# ------------------------------------------------------------------------------

#' Load all WOS files from a directory
#'
#' @param path Directory containing WOS files (default: from config)
#' @return Tibble with parsed WOS records
load_wos_corpus <- function(path = config$paths$wos_raw) {
  load_wos_directory(path)
}

# ------------------------------------------------------------------------------
# NBER DATA LOADING
# ------------------------------------------------------------------------------

#' Load NBER paper text files
#'
#' @param path Directory containing NBER text files
#' @return Tibble with paper_id and text content
load_nber_papers <- function(path = config$paths$nber_raw) {

  if (!dir.exists(path)) {
    stop("NBER directory does not exist: ", path)
  }

  # Find text files (pattern: w12345.txt)
  files <- list.files(path, pattern = "^w\\d+\\.txt$", full.names = TRUE)

  if (length(files) == 0) {
    warning("No NBER paper files found in: ", path)
    return(tibble(paper_id = character(), text = character()))
  }

  message("Loading ", length(files), " NBER paper(s)...")

  # Load each file
  nber_data <- tibble(
    file_path = files,
    paper_id = str_extract(basename(files), "w\\d+"),
    text = map_chr(files, function(f) {
      tryCatch(read_file_safe(f), error = function(e) NA_character_)
    })
  )

  # Report failures
  failed <- sum(is.na(nber_data$text))
  if (failed > 0) warning(failed, " papers failed to load")

  nber_data <- filter(nber_data, !is.na(text))
  message("Successfully loaded ", nrow(nber_data), " papers")

  select(nber_data, paper_id, text)
}

#' Load and parse all NBER citations
#'
#' @param path Directory containing NBER files
#' @return Tibble with parsed citations from all papers
load_nber_citations <- function(path = config$paths$nber_raw) {

  papers <- load_nber_papers(path)

  if (nrow(papers) == 0) {
    return(create_nber_tibble())
  }

  message("\nExtracting citations from papers...")

  # Parse each paper
  all_citations <- map2(
    papers$paper_id,
    papers$text,
    function(pid, txt) {
      tryCatch(
        parse_nber_paper(pid, txt),
        error = function(e) {
          warning("Failed to process ", pid)
          create_nber_tibble()
        }
      )
    }
  ) %>% bind_rows()

  message("Total: ", nrow(all_citations), " citations from ", n_distinct(all_citations$paper_id), " papers")

  return(all_citations)
}

# ------------------------------------------------------------------------------
# RUN AS SCRIPT
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  message("\n=== Data Ingestion ===\n")

  # Load WOS
  message("--- WOS Data ---")
  wos_data <- load_wos_corpus()
  check_data(wos_data, "WOS")

  # Load NBER
  message("\n--- NBER Papers ---")
  nber_data <- load_nber_papers()
  check_data(nber_data, "NBER papers")

  message("\nDone.")
}
