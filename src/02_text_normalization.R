# ==============================================================================
# TEXT NORMALIZATION
# Functions for standardizing citation text for comparison
# ==============================================================================

# Load configuration
if (!exists("config")) source("src/00_config.R")

# ------------------------------------------------------------------------------
# CORE NORMALIZATION FUNCTIONS
# ------------------------------------------------------------------------------

#' Normalize a single citation string
#'
#' Converts citation to lowercase and removes all non-alphabetic characters.
#' This creates a standardized form for fuzzy matching.
#'
#' @param citation Character string of citation text
#' @param remove_numbers Logical; remove numeric characters? (default: FALSE)
#' @return Normalized character string (lowercase letters only)
#'
#' @examples
#' normalize_citation("Smith, J. (2020). Machine Learning. Journal, 34(2).")
#' # Returns: "smithjmachinelearningjournal"
#'
#' normalize_citation("Smith, J. (2020). Title.", remove_numbers = FALSE)
#' # Returns: "smithj2020title"
normalize_citation <- function(citation,
                               remove_numbers = config$normalization$remove_numbers) {


  # Handle NULL, NA, or non-character input
  if (is.null(citation) || is.na(citation)) {
    return(NA_character_)
  }

  if (!is.character(citation)) {
    citation <- as.character(citation)
  }

  # Handle empty strings
  if (nchar(citation) == 0) {
    return("")
  }

  # Step 1: Convert to lowercase
  normalized <- str_to_lower(citation)

  # Step 2: Remove numbers if requested
  if (remove_numbers) {
    normalized <- str_remove_all(normalized, "[0-9]")
  }

  # Step 3: Remove all non-alphanumeric characters
  # Keep only lowercase letters (and numbers if not removed)
  if (remove_numbers) {
    normalized <- str_remove_all(normalized, "[^a-z]")
  } else {
    normalized <- str_remove_all(normalized, "[^a-z0-9]")
  }

  return(normalized)
}

#' Normalize multiple citations (vectorized)
#'
#' @param citations Character vector of citations
#' @param remove_numbers Logical; remove numeric characters?
#' @param progress Logical; show progress bar?
#' @return Character vector of normalized citations
#'
#' @examples
#' citations <- c("Author A. (2020). Title.", "Author B. (2021). Title.")
#' normalize_citations(citations)
normalize_citations <- function(citations,
                                remove_numbers = config$normalization$remove_numbers,
                                progress = TRUE) {

  if (length(citations) == 0) {
    return(character())
  }

  # Use vectorized operations for speed
  message("Normalizing ", length(citations), " citations...")

  normalized <- citations %>%
    str_to_lower() %>%
    {
      if (remove_numbers) {
        str_remove_all(., "[0-9]")
      } else {
        .
      }
    } %>%
    {
      if (remove_numbers) {
        str_remove_all(., "[^a-z]")
      } else {
        str_remove_all(., "[^a-z0-9]")
      }
    }

  # Preserve NA values
  normalized[is.na(citations)] <- NA_character_

  message("  Done. ", sum(!is.na(normalized)), " valid normalized citations.")

  return(normalized)
}

# ------------------------------------------------------------------------------
# VALIDATION FUNCTIONS
# ------------------------------------------------------------------------------

#' Check if normalized citation meets minimum requirements
#'
#' @param normalized_citation Normalized citation string
#' @param min_length Minimum acceptable length
#' @return Logical; TRUE if valid
is_valid_normalized <- function(normalized_citation,
                                min_length = config$normalization$min_length) {

  if (is.na(normalized_citation)) return(FALSE)
  if (nchar(normalized_citation) < min_length) return(FALSE)

  return(TRUE)
}

#' Filter out invalid normalized citations
#'
#' @param data Tibble with normalized_citation column
#' @param min_length Minimum acceptable length
#' @return Filtered tibble
filter_valid_citations <- function(data,
                                   min_length = config$normalization$min_length) {

  if (!"normalized_citation" %in% names(data)) {
    stop("Data must have 'normalized_citation' column")
  }

  n_before <- nrow(data)

  data_filtered <- data %>%
    filter(
      !is.na(normalized_citation),
      nchar(normalized_citation) >= min_length
    )

  n_after <- nrow(data_filtered)
  n_removed <- n_before - n_after

  if (n_removed > 0) {
    message("Filtered out ", n_removed, " invalid citations (",
            round(100 * n_removed / n_before, 1), "%)")
  }

  return(data_filtered)
}

# ------------------------------------------------------------------------------
# BATCH PROCESSING
# ------------------------------------------------------------------------------

#' Process WOS data: add normalized citations
#'
#' @param wos_data Tibble with 'reference' column
#' @return Tibble with added 'normalized_reference' column
normalize_wos_corpus <- function(wos_data) {

  if (!"reference" %in% names(wos_data)) {
    stop("WOS data must have 'reference' column")
  }

  message("Processing WOS corpus...")

  wos_data %>%
    mutate(
      normalized_reference = normalize_citations(reference, progress = FALSE)
    )
}

#' Process NBER citations: add normalized versions
#'
#' @param nber_citations Tibble with 'citation' column
#' @return Tibble with added 'normalized_citation' column
normalize_nber_citations <- function(nber_citations) {

  if (!"citation" %in% names(nber_citations)) {
    stop("NBER data must have 'citation' column")
  }

  message("Processing NBER citations...")

  nber_citations %>%
    mutate(
      normalized_citation = normalize_citations(citation, progress = FALSE)
    )
}

# ------------------------------------------------------------------------------
# EXAMPLES AND TESTING
# ------------------------------------------------------------------------------

#' Run normalization examples for verification
#'
#' @return Tibble with example inputs and outputs
run_normalization_examples <- function() {

  examples <- tibble(
    input = c(
      "Smith, J. (2020). Machine Learning in Economics. Journal of Economic Perspectives, 34(2), 87-108.",
      "O'Brien, C. & Mueller-Langer, F. (2019). AI & Law. Tech. Rev., 15, 1-20.",
      "Johnson Jr., A.B. (2021). \"Deep Learning\" Applications. Nature: Science, 100.",
      "",
      NA_character_,
      "123 456 789"
    ),
    expected_with_numbers = c(
      "smithj2020machinelearningineconomicsjournalofeconomicperspectives34287108",
      "obriencmuellerlanger2019ailawtech rev15120",
      "johnsonjrab2021deeplearningapplicationsnaturescience100",
      "",
      NA_character_,
      "123456789"
    ),
    expected_no_numbers = c
    (
      "smithjmachinelearningineconomicsjournalofeconomicperspectives",
      "obriencmuellerlanger ai lawtechrev",
      "johnsonjrabdeeplearningapplicationsnaturescience",
      "",
      NA_character_,
      ""
    )
  )

  # Test with numbers
  examples$actual_with_numbers <- normalize_citations(
    examples$input,
    remove_numbers = FALSE,
    progress = FALSE
  )

  # Test without numbers
  examples$actual_no_numbers <- normalize_citations(
    examples$input,
    remove_numbers = TRUE,
    progress = FALSE
  )

  return(examples)
}

# ------------------------------------------------------------------------------
# MAIN EXECUTION (if run as script)
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  message("\n", strrep("=", 60))
  message("Text Normalization Module")
  message(strrep("=", 60), "\n")

  # Run examples
  message("Running normalization examples...\n")
  examples <- run_normalization_examples()

  print(examples %>% select(input, actual_with_numbers))

  message("\nNormalization module loaded successfully.")
  message("Key functions: normalize_citation(), normalize_citations()")
}
