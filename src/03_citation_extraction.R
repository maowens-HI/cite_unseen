# ==============================================================================
# CITATION EXTRACTION
# Extract citations from NBER papers
# ==============================================================================
#
# This is a thin wrapper around the nber_parser functions.
# The main work is done in nber_parser.R
#
# ==============================================================================

if (!exists("config")) source("src/00_config.R")
source("src/nber_parser.R")
source("src/01_data_ingestion.R")

# ------------------------------------------------------------------------------
# MAIN FUNCTION
# ------------------------------------------------------------------------------

#' Extract all citations from NBER papers
#'
#' @param nber_data Tibble with paper_id and text columns
#' @return Tibble with extracted citations
extract_citations <- function(nber_data) {

  if (!all(c("paper_id", "text") %in% names(nber_data))) {
    stop("Data must have 'paper_id' and 'text' columns")
  }

  message("Extracting citations from ", nrow(nber_data), " papers...")

  # Use the parser from nber_parser.R
  all_citations <- map2(
    nber_data$paper_id,
    nber_data$text,
    function(pid, txt) {
      tryCatch(
        parse_nber_paper(pid, txt),
        error = function(e) {
          warning("Failed: ", pid)
          create_nber_tibble()
        }
      )
    }
  ) %>% bind_rows()

  message("Extracted ", nrow(all_citations), " citations")

  return(all_citations)
}

# ------------------------------------------------------------------------------
# RUN AS SCRIPT
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  message("\n=== Citation Extraction ===\n")

  # Demo
  sample_text <- "
  Introduction here.

  References

  Smith, J. (2020): \"Machine Learning,\" Journal of Economics, 34, 87-108.

  Johnson, A. (2019): \"Deep Learning,\" Nature, 1, 15-25.
  "

  result <- parse_nber_paper("w99999", sample_text)
  print(result)
}
