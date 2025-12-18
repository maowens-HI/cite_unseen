================================================================================
SOURCE CODE
================================================================================

PURPOSE
-------
R source files implementing the analysis pipeline.

FILE STRUCTURE
--------------
Scripts are numbered for sequential execution:

00_config.R
  - Project configuration and constants
  - File paths, thresholds, parameters
  - Load this first in every session

01_data_ingestion.R
  - Load WOS CSV files
  - Load NBER paper files
  - Basic data validation

02_text_normalization.R
  - Citation text normalization functions
  - normalize_citation(): single string
  - normalize_citations(): vectorized

03_citation_extraction.R
  - Extract citations from NBER papers
  - PDF text extraction
  - Bibliography section detection

04_fuzzy_matching.R
  - Levenshtein distance calculations
  - find_best_match(): single query
  - batch_fuzzy_match(): parallel processing

05_score_analysis.R
  - Descriptive statistics
  - Visualization generation
  - Threshold sensitivity analysis

06_causal_estimation.R
  - DiD/RDD estimation
  - Regression tables
  - Robustness checks

/utils/
  text_utils.R  - Text processing helpers
  io_utils.R    - File I/O helpers

USAGE
-----
# Option 1: Run full pipeline
source("src/00_config.R")
source("src/01_data_ingestion.R")
# ... continue through 06

# Option 2: Load functions only
source("src/00_config.R")
source("src/02_text_normalization.R")
# Use normalize_citation() as needed

# Option 3: Run from command line
Rscript src/01_data_ingestion.R
Rscript src/02_text_normalization.R

DEPENDENCIES
------------
See 00_config.R for full package list. Key packages:
  - tidyverse (core data manipulation)
  - stringdist (Levenshtein distance)
  - furrr (parallel processing)
  - arrow (Parquet I/O)

CODING STANDARDS
----------------
1. Follow tidyverse style guide
2. Use snake_case for names
3. Document all functions with comments
4. Include input/output type annotations
5. Write defensive code with input validation

TESTING
-------
Unit tests in /tests/ folder:
  Rscript -e "testthat::test_dir('tests/')"

CONTACT
-------
For code questions, contact: myles.owens@stanford.edu
================================================================================
