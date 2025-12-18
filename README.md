# Cite Unseen: Detecting LLM-Hallucinated References in Academic Papers

A causal identification study examining whether Large Language Models (LLMs) have increased unverifiable references in NBER working papers following the public release of ChatGPT 3.0.

## Research Question

**Do LLMs leave detectable artifacts in academic writing through hallucinated bibliographic references?**

This project exploits the exogenous timing of ChatGPT 3.0's public release (November 30, 2022) to causally identify changes in citation verifiability patterns in NBER working papers. The hypothesis is that LLM-assisted writing produces bibliographic "hallucinations"—plausible-sounding but non-existent references—that can be detected through fuzzy matching against verified academic databases.

## Methodology Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                          CITE UNSEEN PIPELINE                               │
└─────────────────────────────────────────────────────────────────────────────┘

  DATA SOURCES                    PROCESSING                      ANALYSIS
  ────────────                    ──────────                      ────────

  ┌─────────────┐              ┌──────────────────┐
  │ WOS Corpus  │──────────────│ Text             │
  │ (CSV files) │              │ Normalization    │
  └─────────────┘              │                  │
                               │ • lowercase      │
  ┌─────────────┐              │ • remove spaces  │         ┌─────────────────┐
  │ NBER Papers │──────────────│ • strip symbols  │────────▶│ Fuzzy Matching  │
  │ (text/PDF)  │              │ • standardize    │         │ (Levenshtein)   │
  └─────────────┘              └──────────────────┘         └────────┬────────┘
                                                                     │
                                                                     ▼
  ┌─────────────┐              ┌──────────────────┐         ┌─────────────────┐
  │ Treatment   │              │ Match Scores     │         │ Causal          │
  │ Date:       │──────────────│ Per Citation     │────────▶│ Identification  │
  │ 2022-11-30  │              │ (0 = exact match)│         │ (DiD/RDD)       │
  └─────────────┘              └──────────────────┘         └─────────────────┘
                                        │
                                        ▼
                               ┌──────────────────┐
                               │ Hallucination    │
                               │ Classification   │
                               │ (threshold-based)│
                               └──────────────────┘
```

### Key Steps

1. **Data Collection**: Gather Web of Science (WOS) citation corpus and NBER working paper texts
2. **Text Normalization**: Standardize all citations (lowercase, remove spaces/special characters)
3. **Ground Truth Establishment**: Calculate normalized Levenshtein distances within WOS corpus
4. **Fuzzy Matching**: Compare NBER citations against WOS corpus
5. **Score Generation**: Compute match quality for each NBER citation
6. **Causal Analysis**: Exploit ChatGPT 3.0 release timing for identification

## Project Structure

```
cite_unseen/
├── README.md                    # This file
├── .gitignore                   # Git ignore rules
├── renv.lock                    # R package versions (reproducibility)
│
├── data/
│   ├── raw/
│   │   ├── wos/                 # Web of Science CSV exports
│   │   │   └── README.txt
│   │   └── nber/                # NBER working paper files
│   │       └── README.txt
│   ├── processed/               # Cleaned, normalized data
│   │   └── README.txt
│   └── intermediate/            # Intermediate processing files
│       └── README.txt
│
├── src/                         # R source code
│   ├── 00_config.R              # Project configuration & constants
│   ├── 01_data_ingestion.R      # Load raw data files
│   ├── 02_text_normalization.R  # Citation normalization functions
│   ├── 03_citation_extraction.R # Extract citations from NBER papers
│   ├── 04_fuzzy_matching.R      # Levenshtein distance calculations
│   ├── 05_score_analysis.R      # Statistical analysis of match scores
│   ├── 06_causal_estimation.R   # DiD/RDD estimation
│   └── utils/                   # Helper functions
│       ├── text_utils.R         # Text processing utilities
│       └── io_utils.R           # File I/O utilities
│
├── code/                        # Legacy/exploratory scripts
│   ├── bk.R                     # BK distance exploration
│   ├── nber_scraper.R           # NBER PDF scraper (sample)
│   └── nber_full_scraper.R      # NBER PDF scraper (full)
│
├── tests/                       # Unit tests
│   ├── test_normalization.R
│   └── test_fuzzy_matching.R
│
├── output/
│   ├── figures/                 # Generated plots
│   ├── tables/                  # Result tables
│   └── reports/                 # Analysis reports
│
└── docs/                        # Additional documentation
    └── methodology.md
```

## Installation & Setup

### Prerequisites

- R >= 4.2.0
- RStudio (recommended) or command-line R

### Required R Packages

```r
# Core tidyverse
install.packages("tidyverse")

# Text processing
install.packages("stringdist")  # Levenshtein distance
install.packages("stringi")     # String manipulation
install.packages("tokenizers")  # Text tokenization

# Data handling
install.packages("data.table")  # Large file processing
install.packages("arrow")       # Parquet support for large datasets

# PDF processing (for NBER papers)
install.packages("pdftools")

# Web scraping
install.packages("httr")
install.packages("rvest")

# Parallel processing
install.packages("furrr")
install.packages("future")

# Reproducibility
install.packages("renv")
```

### Quick Start

```r
# Clone the repository
# git clone <repository-url>
# cd cite_unseen

# Initialize renv (for reproducibility)
renv::restore()

# Load project configuration
source("src/00_config.R")

# Run the pipeline
source("src/01_data_ingestion.R")
source("src/02_text_normalization.R")
# ... continue through pipeline
```

## Usage Examples

### Example 1: Text Normalization

```r
source("src/02_text_normalization.R")

# Single citation
citation <- "Smith, J. (2020). Machine Learning in Economics.
             Journal of Economic Perspectives, 34(2), 87-108."
normalize_citation(citation)
# Returns: "smithjmachinelearningineconomicsjournalofeconomicperspectives"

# Batch processing
citations <- c(
  "Author, A. (2019). Title One. Journal One.",
  "Author, B. (2020). Title Two. Journal Two."
)
normalize_citations(citations)
```

### Example 2: Fuzzy Matching

```r
source("src/04_fuzzy_matching.R")

# Compare single citation against WOS corpus
result <- find_best_match(
  query = "smithjmachinelearningineconomics",
  corpus = wos_normalized,
  method = "lv"  # Levenshtein
)
# Returns: list(best_match, distance, normalized_distance)

# Batch matching with progress
results <- batch_fuzzy_match(
  queries = nber_citations_normalized,
  corpus = wos_normalized,
  n_cores = 4
)
```

### Example 3: Complete Pipeline

```r
# Full analysis pipeline
source("src/00_config.R")

# 1. Load data
wos_data <- load_wos_corpus(config$paths$wos_raw)
nber_data <- load_nber_papers(config$paths$nber_raw)

# 2. Extract and normalize citations
nber_citations <- extract_citations(nber_data)
wos_normalized <- normalize_citations(wos_data$reference)
nber_normalized <- normalize_citations(nber_citations$citation)

# 3. Fuzzy matching
match_results <- batch_fuzzy_match(nber_normalized, wos_normalized)

# 4. Analyze results
analysis <- analyze_match_scores(
  match_results,
  treatment_date = as.Date("2022-11-30")
)

# 5. Export results
export_results(analysis, config$paths$output)
```

## Data Sources

### Web of Science (WOS)

- **Format**: CSV exports from Web of Science database
- **Fields**: Reference string, DOI, publication year, journal, authors
- **Purpose**: Ground truth corpus of verified academic citations
- **Location**: `data/raw/wos/`

### NBER Working Papers

- **Format**: Text files extracted from PDFs, or PDF files directly
- **Source**: https://www.nber.org/papers
- **Fields**: Paper ID, publication date, full text (including bibliography)
- **Location**: `data/raw/nber/`

### Treatment Date

- **Event**: ChatGPT 3.0 public release
- **Date**: November 30, 2022
- **Identification Strategy**: Difference-in-differences or regression discontinuity

## Methodology Details

### Text Normalization Specification

```
Input:  "Smith, J. (2020). Machine Learning in Economics.
         Journal of Economic Perspectives, 34(2), 87-108."

Step 1: lowercase
        "smith, j. (2020). machine learning in economics.
         journal of economic perspectives, 34(2), 87-108."

Step 2: Remove numbers (optional, configurable)
        "smith, j. (). machine learning in economics.
         journal of economic perspectives, (), -."

Step 3: Remove all non-alphabetic characters
        "smithjmachinelearningineconomicsjournalofeconomicperspectives"

Output: "smithjmachinelearningineconomicsjournalofeconomicperspectives"
```

### Normalized Levenshtein Distance

```
Definition: normalized_distance = raw_distance / max(len(string_a), len(string_b))

Example:
  String A: "smithjmachinelearningineconomics" (32 chars)
  String B: "smithjmachinelearningineconomcis" (32 chars)  # typo: "cis" vs "ics"

  Raw Levenshtein distance: 2 (two character transpositions)
  Normalized distance: 2 / 32 = 0.0625

Interpretation:
  0.00       = Exact match
  0.00-0.10  = Very high similarity (likely same reference)
  0.10-0.30  = Moderate similarity (possible match, needs review)
  0.30+      = Low similarity (likely different references)
```

### Expected Output Format

| NBER_paper_id | citation_num | original_citation | normalized | best_wos_match | match_score | potential_hallucination |
|---------------|--------------|-------------------|------------|----------------|-------------|------------------------|
| w31280 | 1 | "Smith, J..." | "smithj..." | "smithjmachine..." | 0.0625 | FALSE |
| w31280 | 2 | "Unknown, X..." | "unknownx..." | "closest_match..." | 0.8500 | TRUE |

## Performance Considerations

### Memory Management

- Process NBER files in batches (configurable batch size)
- Use `data.table` for large WOS corpus operations
- Implement lazy loading for intermediate results
- Consider Arrow/Parquet for disk-based processing

### Parallel Processing

```r
# Enable parallel processing
library(furrr)
plan(multisession, workers = parallel::detectCores() - 1)

# Parallel fuzzy matching
results <- future_map(
  nber_citations,
  ~find_best_match(.x, wos_corpus),
  .progress = TRUE
)
```

### Benchmarks (Expected)

| Dataset Size | Processing Time | Memory Usage |
|--------------|-----------------|--------------|
| 1,000 NBER papers | ~10 min | ~2 GB |
| 10,000 NBER papers | ~2 hours | ~8 GB |
| Full corpus | ~24 hours | ~32 GB |

## Testing & Validation

### Unit Tests

```bash
# Run all tests
Rscript -e "testthat::test_dir('tests/')"

# Run specific test file
Rscript -e "testthat::test_file('tests/test_normalization.R')"
```

### Validation Checklist

- [ ] Normalization produces consistent results
- [ ] Levenshtein distances match known examples
- [ ] Known citations have distance ≈ 0
- [ ] Random strings have high distances
- [ ] Processing handles edge cases (empty strings, special characters)
- [ ] Results are reproducible across runs

## Collaboration Guidelines

### For AI-Assisted Development

This project is structured for effective collaboration with AI coding assistants:

1. **Modular Functions**: Each function does one thing well
2. **Clear Naming**: Function and variable names are self-documenting
3. **Typed Inputs/Outputs**: Comments specify expected data types
4. **README in Every Folder**: Context is always available
5. **Example Data**: Sample inputs/outputs for testing

### Code Style

- Follow tidyverse style guide
- Use `snake_case` for variables and functions
- Prefix internal/helper functions with `.`
- Document all exported functions with roxygen2-style comments

### Contribution Workflow

1. Create feature branch from `main`
2. Implement changes with tests
3. Update relevant README files
4. Submit pull request with description

## Citation

If you use this code or methodology, please cite:

```bibtex
@misc{cite_unseen_2024,
  author = {Owens, Myles and [Coauthor Name]},
  title = {Cite Unseen: Detecting LLM-Hallucinated References in Academic Papers},
  year = {2024},
  publisher = {GitHub},
  url = {https://github.com/[repository]}
}
```

## License

[TBD - Recommend MIT or Apache 2.0 for academic research]

## Acknowledgments

- NBER for providing access to working paper archives
- Web of Science for citation data
- [Coauthor acknowledgments]

---

**Contact**: Myles Owens (myles.owens@stanford.edu)

**Last Updated**: December 2024
