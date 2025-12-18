# ==============================================================================
# TESTS: Fuzzy Matching
# ==============================================================================

library(testthat)

source("src/00_config.R")
source("src/04_fuzzy_matching.R")

# ------------------------------------------------------------------------------
# Test: find_best_match()
# ------------------------------------------------------------------------------

test_that("find_best_match finds exact match", {
  corpus <- c("apple", "banana", "cherry")
  result <- find_best_match("banana", corpus)

  expect_equal(result$best_match, "banana")
  expect_equal(result$distance, 0)
})

test_that("find_best_match finds closest match", {
  corpus <- c("machinelearning", "deeplearning")
  result <- find_best_match("machinelearnin", corpus)  # missing 'g'

  expect_equal(result$best_match, "machinelearning")
  expect_true(result$distance < 0.1)
})

test_that("find_best_match handles NA query", {
  corpus <- c("one", "two", "three")
  result <- find_best_match(NA, corpus)

  expect_true(is.na(result$best_match))
  expect_true(is.na(result$distance))
})

test_that("find_best_match handles empty corpus", {
  result <- find_best_match("test", character())

  expect_true(is.na(result$best_match))
})

test_that("find_best_match returns corpus IDs when provided", {
  corpus <- c("apple", "banana", "cherry")
  ids <- c("id1", "id2", "id3")
  result <- find_best_match("banana", corpus, corpus_ids = ids)

  expect_equal(result$match_id, "id2")
})

# ------------------------------------------------------------------------------
# Run tests
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  test_file("tests/test_fuzzy_matching.R")
}
