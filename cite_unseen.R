# ==============================================================================
# CITE UNSEEN
# Detecting LLM-Hallucinated References in NBER Papers
# ==============================================================================
#
# This script detects whether LLMs have increased unverifiable (hallucinated)
# references in academic papers using the ChatGPT release (Nov 30, 2022) as
# a natural experiment.
#
# USAGE:
#   1. Place WOS data in data/wos/ (tab-separated: WOS_ID <tab> reference)
#   2. Place NBER papers in data/nber/ (text files named w#####.txt)
#   3. Run: Rscript cite_unseen.R
#
# ==============================================================================

# --- PACKAGES -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(stringdist)
})

# --- CONFIGURATION ------------------------------------------------------------
CONFIG <- list(
  treatment_date = as.Date("2022-11-30"),  # ChatGPT release
  hallucination_threshold = 0.30,           # Distance > this = hallucination
  seed = 42,
  paths = list(
    wos  = "data/wos",
    nber = "data/nber",
    out  = "output"
  )
)

set.seed(CONFIG$seed)
for (p in CONFIG$paths) if (!dir.exists(p)) dir.create(p, recursive = TRUE)

# ==============================================================================
# PARSING FUNCTIONS
# ==============================================================================

#' Parse WOS reference string
parse_wos_reference <- function(reference) {
  if (is.na(reference) || nchar(trimws(reference)) == 0) {
    return(list(author = NA, year = NA, title = NA, journal = NA))
  }

  reference <- trimws(reference)
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

  # Fallback
  list(
    author  = str_extract(reference, "^.+?(?=\\s+\\d{4})") %>% trimws(),
    year    = as.integer(str_extract(reference, "\\b(19|20)\\d{2}\\b")),
    title   = NA,
    journal = str_extract(reference, "[A-Z][A-Z0-9\\s&\\-:,]{10,}$") %>% trimws()
  )
}

#' Parse NBER citation
parse_nber_citation <- function(citation) {
  result <- list(authors = NA, year = NA, title = NA, publication = NA)
  if (is.na(citation) || nchar(trimws(citation)) == 0) return(result)

  citation <- trimws(citation)

  # Year in parentheses
  year_match <- str_match(citation, "\\((\\d{4})[a-z]?\\)")
  if (!is.na(year_match[1, 2])) {
    result$year <- as.integer(year_match[1, 2])
  } else {
    result$year <- as.integer(str_extract(citation, "\\b(19|20)\\d{2}\\b"))
  }

  # Authors before year
  if (!is.na(result$year)) {
    author_match <- str_match(citation, paste0("^(.+?)\\s*\\(", result$year))
    if (!is.na(author_match[1, 2])) result$authors <- trimws(author_match[1, 2])
  }

  # Title in quotes
  title_match <- str_match(citation, '"([^"]+)"')
  if (!is.na(title_match[1, 2])) result$title <- trimws(title_match[1, 2])

  # Publication after title
  pub_match <- str_match(citation, '",?\\s*([^,\\d]+)')
  if (!is.na(pub_match[1, 2])) {
    pub <- str_remove(trimws(pub_match[1, 2]), "[.,;:]+$")
    if (nchar(pub) > 3) result$publication <- pub
  }

  result
}

#' Extract bibliography section from paper text
extract_bibliography <- function(text) {
  if (is.na(text) || nchar(text) < 100) return(NA)

  patterns <- c("(?i)\\n\\s*references\\s*\\n", "(?i)\\n\\s*bibliography\\s*\\n")
  bib_start <- NA

  for (pattern in patterns) {
    match <- str_locate(text, pattern)
    if (!is.na(match[1, "end"])) { bib_start <- match[1, "end"]; break }
  }

  if (is.na(bib_start)) return(NA)

  bib_text <- str_sub(text, bib_start)

  # Cut at appendix
  for (pattern in c("(?i)\\n\\s*appendix", "(?i)\\n\\s*tables?\\s*\\n")) {
    match <- str_locate(bib_text, pattern)
    if (!is.na(match[1, "start"]) && match[1, "start"] > 100) {
      bib_text <- str_sub(bib_text, 1, match[1, "start"] - 1)
      break
    }
  }

  trimws(bib_text)
}

#' Split bibliography into individual citations
split_bibliography <- function(bib_text) {
  if (is.na(bib_text) || nchar(bib_text) < 20) return(character())

  citations <- bib_text %>%
    str_replace_all("\\r\\n", "\n") %>%
    str_split("\\n(?=[A-Z][a-z]+,?\\s+[A-Z]\\.?[^\\n]*\\(\\d{4})") %>%
    unlist() %>% str_trim()

  citations <- citations[nchar(citations) > 20]

  if (length(citations) < 3) {
    citations <- str_split(bib_text, "\\n{2,}")[[1]] %>% str_trim()
    citations <- citations[nchar(citations) > 20]
  }

  str_replace_all(citations, "\\s+", " ")
}

# ==============================================================================
# DATA LOADING
# ==============================================================================

#' Load WOS corpus
load_wos <- function(path = CONFIG$paths$wos) {
  files <- list.files(path, pattern = "\\.(txt|csv)$", full.names = TRUE)
  if (length(files) == 0) { warning("No WOS files found"); return(tibble()) }

  message("Loading ", length(files), " WOS file(s)...")

  map_dfr(files, function(f) {
    lines <- read_lines(f)
    if (length(lines) > 0 && str_detect(lines[1], "^ID\\t|^wos_id\\t")) lines <- lines[-1]

    map_dfr(lines, function(line) {
      parts <- str_split(line, "\t", n = 2)[[1]]
      if (length(parts) < 2) return(NULL)

      parsed <- parse_wos_reference(parts[2])
      tibble(
        wos_id = trimws(parts[1]), author = parsed$author, year = parsed$year,
        title = parsed$title, journal = parsed$journal, reference = trimws(parts[2])
      )
    })
  })
}

#' Load and parse NBER papers
load_nber <- function(path = CONFIG$paths$nber) {
  files <- list.files(path, pattern = "^w\\d+\\.txt$", full.names = TRUE)
  if (length(files) == 0) { warning("No NBER files found"); return(tibble()) }

  message("Loading ", length(files), " NBER paper(s)...")

  map_dfr(files, function(f) {
    paper_id <- str_extract(basename(f), "w\\d+")
    text <- tryCatch(paste(readLines(f, warn = FALSE), collapse = "\n"), error = function(e) NA)
    if (is.na(text)) return(NULL)

    bib_text <- extract_bibliography(text)
    if (is.na(bib_text)) return(NULL)

    citations <- split_bibliography(bib_text)
    if (length(citations) == 0) return(NULL)

    parsed <- map(citations, parse_nber_citation)

    tibble(
      paper_id = paper_id,
      citation_num = seq_along(citations),
      authors = map_chr(parsed, ~ .x$authors %||% NA_character_),
      year = map_int(parsed, ~ .x$year %||% NA_integer_),
      title = map_chr(parsed, ~ .x$title %||% NA_character_),
      raw_citation = citations
    )
  })
}

# ==============================================================================
# FUZZY MATCHING
# ==============================================================================

#' Normalize citation for matching (lowercase, alphanumeric only)
normalize <- function(x) {
  if (is.na(x)) return(NA_character_)
  str_remove_all(str_to_lower(x), "[^a-z0-9]")
}

#' Match NBER citations against WOS corpus
match_citations <- function(nber, wos, threshold = CONFIG$hallucination_threshold) {
  message("Matching ", nrow(nber), " citations against ", nrow(wos), " WOS references...")

  nber_norm <- map_chr(nber$raw_citation, normalize)
  wos_norm <- map_chr(wos$reference, normalize)

  results <- map_dfr(seq_len(nrow(nber)), function(i) {
    if (i %% 100 == 0) message("  ", i, "/", nrow(nber))

    query <- nber_norm[i]
    if (is.na(query) || query == "") {
      return(tibble(best_wos_id = NA, distance = NA, is_hallucination = NA))
    }

    raw_dists <- stringdist(query, wos_norm, method = "lv")
    max_lens <- pmax(nchar(query), nchar(wos_norm))
    distances <- raw_dists / max_lens

    best_idx <- which.min(distances)

    tibble(
      best_wos_id = wos$wos_id[best_idx],
      distance = distances[best_idx],
      is_hallucination = distances[best_idx] > threshold
    )
  })

  bind_cols(nber, results)
}

# ==============================================================================
# ANALYSIS
# ==============================================================================

#' Summary statistics
summarize_results <- function(results) {
  cat("\n=== RESULTS SUMMARY ===\n")
  cat("Total citations:", nrow(results), "\n")
  cat("Mean distance:", round(mean(results$distance, na.rm = TRUE), 3), "\n")
  cat("Median distance:", round(median(results$distance, na.rm = TRUE), 3), "\n")
  cat("Exact matches:", sum(results$distance == 0, na.rm = TRUE), "\n")
  cat("Potential hallucinations (d >", CONFIG$hallucination_threshold, "):",
      sum(results$is_hallucination, na.rm = TRUE),
      "(", round(100 * mean(results$is_hallucination, na.rm = TRUE), 1), "%)\n")
}

#' Difference-in-Differences regression
run_did <- function(results, paper_dates) {
  data <- results %>%
    left_join(paper_dates, by = "paper_id") %>%
    filter(!is.na(pub_date)) %>%
    mutate(post = as.integer(pub_date >= CONFIG$treatment_date))

  if (nrow(data) == 0) { message("No data with dates"); return(NULL) }

  model <- lm(distance ~ post, data = data)

  cat("\n=== DiD REGRESSION ===\n")
  cat("Model: distance ~ post_treatment\n\n")
  print(summary(model)$coefficients)

  model
}

#' Plot score distribution
plot_distribution <- function(results, save_path = NULL) {
  p <- ggplot(results, aes(x = distance)) +
    geom_histogram(bins = 50, fill = "steelblue", color = "white", alpha = 0.8) +
    geom_vline(xintercept = CONFIG$hallucination_threshold, color = "red", linetype = "dashed") +
    labs(title = "Citation Match Score Distribution",
         subtitle = paste("Red line = hallucination threshold (", CONFIG$hallucination_threshold, ")"),
         x = "Normalized Levenshtein Distance", y = "Count") +
    theme_minimal()

  if (!is.null(save_path)) ggsave(save_path, p, width = 8, height = 5)
  p
}

# ==============================================================================
# MAIN PIPELINE
# ==============================================================================

run_analysis <- function() {
  message("\n========================================")
  message("CITE UNSEEN - Citation Hallucination Detection")
  message("========================================\n")

  # Load data
  wos <- load_wos()
  if (nrow(wos) == 0) stop("No WOS data found. Add files to data/wos/")
  message("Loaded ", nrow(wos), " WOS references\n")

  nber <- load_nber()
  if (nrow(nber) == 0) stop("No NBER data found. Add files to data/nber/")
  message("Loaded ", nrow(nber), " NBER citations\n")

  # Match citations
  results <- match_citations(nber, wos)

  # Summarize
  summarize_results(results)

  # Save results
  write_csv(results, file.path(CONFIG$paths$out, "match_results.csv"))
  message("\nResults saved to output/match_results.csv")

  # Plot
  plot_distribution(results, file.path(CONFIG$paths$out, "score_distribution.png"))
  message("Plot saved to output/score_distribution.png")

  invisible(results)
}

# ==============================================================================
# RUN
# ==============================================================================

if (sys.nframe() == 0) {
  run_analysis()
}
