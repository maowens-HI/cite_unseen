# ==============================================================================
# TESTS: WOS Parser
# ==============================================================================

library(testthat)

source("src/00_config.R")
source("src/data_models.R")
source("src/wos_parser.R")

# ------------------------------------------------------------------------------
# Test: parse_wos_reference()
# ------------------------------------------------------------------------------

test_that("parse_wos_reference extracts year", {
  ref <- "Smith, J 2005 100 25 Some title NATURE"
  result <- parse_wos_reference(ref)
  expect_equal(result$year, 2005L)
})

test_that("parse_wos_reference extracts author", {
  ref <- "Diepenmaat-Wolters, MGE 1997 147 55 Title JOURNAL NAME"
  result <- parse_wos_reference(ref)
  expect_equal(result$author, "Diepenmaat-Wolters, MGE")
})

test_that("parse_wos_reference extracts journal", {
  ref <- "Smith, A 2000 1 1 Title NATURE MEDICINE"
  result <- parse_wos_reference(ref)
  expect_equal(result$journal, "NATURE MEDICINE")
})

test_that("parse_wos_reference handles NA input", {
  result <- parse_wos_reference(NA)
  expect_true(is.na(result$year))
})

test_that("parse_wos_reference handles empty string", {
  result <- parse_wos_reference("")
  expect_true(is.na(result$year))
})

# ------------------------------------------------------------------------------
# Run tests
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  test_file("tests/test_wos_parser.R")
}
