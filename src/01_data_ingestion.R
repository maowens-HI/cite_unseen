# ==============================================================================
# DATA INGESTION
# Load raw data from WOS and NBER sources
# ==============================================================================
#
# This script loads:
#   1. WOS (Web of Science) - the "ground truth" of real papers
#   2. NBER paper texts - from which we extract citations
#
# ==============================================================================

if (!exists("config")) source("src/00_config.R")
source("src/data_models.R")
source("src/wos_parser.R")
source("src/nber_parser.R")
source("src/utils.R")

# ==============================================================================
# WOS DATA
# ==============================================================================

#' Load WOS corpus
load_wos_corpus <- function(path = config$paths$wos_raw) {
  load_wos_directory(path)
}

# ==============================================================================
# NBER DATA
# ==============================================================================

#' Load NBER paper text files
load_nber_papers <- function(path = config$paths$nber_raw) {
  if (!dir.exists(path)) stop("Directory not found: ", path)

  files <- list.files(path, pattern = "^w\\d+\\.txt$", full.names = TRUE)

  if (length(files) == 0) {
    warning("No NBER files in: ", path)
    return(tibble(paper_id = character(), text = character()))
  }

  message("Loading ", length(files), " NBER paper(s)...")

  nber_data <- tibble(
    paper_id = str_extract(basename(files), "w\\d+"),
    text = map_chr(files, function(f) {
      tryCatch(read_file_safe(f), error = function(e) NA_character_)
    })
  ) %>% filter(!is.na(text))

  message("Loaded ", nrow(nber_data), " papers")
  nber_data
}

#' Load and parse all NBER citations
load_nber_citations <- function(path = config$paths$nber_raw) {
  papers <- load_nber_papers(path)
  if (nrow(papers) == 0) return(create_nber_tibble())

  message("Extracting citations...")

  map2(papers$paper_id, papers$text, function(pid, txt) {
    tryCatch(parse_nber_paper(pid, txt), error = function(e) create_nber_tibble())
  }) %>% bind_rows()
}

# ==============================================================================
# TEST
# ==============================================================================

if (sys.nframe() == 0) {
  message("\n=== Data Ingestion ===\n")

  wos <- load_wos_corpus()
  check_data(wos, "WOS")

  nber <- load_nber_papers()
  check_data(nber, "NBER")
}
