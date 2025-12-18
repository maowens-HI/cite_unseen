================================================================================
TESTS
================================================================================

PURPOSE
-------
Unit tests for validating core functions in the analysis pipeline.

TEST FILES
----------
test_normalization.R
  - Test normalize_citation() function
  - Edge cases: empty strings, special characters, Unicode
  - Consistency checks

test_fuzzy_matching.R
  - Test Levenshtein distance calculations
  - Verify normalized distance formula
  - Test find_best_match() function

test_citation_extraction.R
  - Test bibliography section detection
  - Test citation parsing
  - Test PDF extraction (if applicable)

RUNNING TESTS
-------------
# Run all tests
Rscript -e "testthat::test_dir('tests/')"

# Run specific test file
Rscript -e "testthat::test_file('tests/test_normalization.R')"

# Run with verbose output
Rscript -e "testthat::test_dir('tests/', reporter = 'summary')"

WRITING TESTS
-------------
Use testthat framework:

  library(testthat)
  source("src/02_text_normalization.R")

  test_that("normalization removes spaces", {
    result <- normalize_citation("hello world")
    expect_equal(result, "helloworld")
  })

  test_that("normalization handles empty strings", {
    result <- normalize_citation("")
    expect_equal(result, "")
  })

TEST DATA
---------
Store test fixtures in tests/fixtures/:
  - sample_citations.txt
  - expected_normalized.txt
  - sample_wos_corpus.csv

COVERAGE
--------
Aim for high coverage of:
  - Normal cases
  - Edge cases (empty, NULL, NA)
  - Error conditions
  - Boundary conditions

CONTACT
-------
For testing questions, contact: myles.owens@stanford.edu
================================================================================
