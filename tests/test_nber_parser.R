# ==============================================================================
# TESTS: NBER Citation Parser
# ==============================================================================

library(testthat)

source("src/00_config.R")
source("src/data_models.R")
source("src/nber_parser.R")

# ------------------------------------------------------------------------------
# Test: parse_citation()
# ------------------------------------------------------------------------------

test_that("parse_citation extracts year in parentheses", {
  result <- parse_citation('Smith, A. (2020): "Title," Journal.')
  expect_equal(result$year, 2020L)
})

test_that("parse_citation extracts authors before year", {
  result <- parse_citation('Barro, R. (2018): "Title," Journal.')
  expect_equal(result$authors, "Barro, R.")
})

test_that("parse_citation extracts quoted title", {
  result <- parse_citation('Smith (2020): "Machine Learning," Journal.')
  expect_equal(result$title, "Machine Learning")
})

test_that("parse_citation handles NA input", {
  result <- parse_citation(NA)
  expect_true(is.na(result$year))
})

test_that("parse_citation handles empty string", {
  result <- parse_citation("")
  expect_true(is.na(result$year))
})

# ------------------------------------------------------------------------------
# Test: find_bibliography()
# ------------------------------------------------------------------------------

test_that("find_bibliography finds References section", {
  paper_text <- "
  Introduction here.

  References

  Smith, A. (2020): \"Title,\" Journal.
  "
  result <- find_bibliography(paper_text)
  expect_true(!is.na(result))
  expect_true(grepl("Smith", result))
})

test_that("find_bibliography returns NA when no references", {
  result <- find_bibliography("No references section here.")
  expect_true(is.na(result))
})

# ------------------------------------------------------------------------------
# Test: split_citations()
# ------------------------------------------------------------------------------

test_that("split_citations separates citations", {
  bib_text <- "
  Smith, A. (2020): \"First,\" Journal A.

  Jones, B. (2019): \"Second,\" Journal B.
  "
  result <- split_citations(bib_text)
  expect_true(length(result) >= 2)
})

test_that("split_citations handles empty input", {
  expect_length(split_citations(""), 0)
})

# ------------------------------------------------------------------------------
# Run tests
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  test_file("tests/test_nber_parser.R")
}
