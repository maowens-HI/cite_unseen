# ==============================================================================
# TESTS: Fuzzy Matching
# Unit tests for Levenshtein distance and matching functions
# ==============================================================================

library(testthat)

# Source the functions to test
source("src/00_config.R")
source("src/04_fuzzy_matching.R")

# ------------------------------------------------------------------------------
# Test: normalized_distance()
# ------------------------------------------------------------------------------

test_that("normalized_distance returns 0 for identical strings", {
  expect_equal(normalized_distance("hello", "hello"), 0)
  expect_equal(normalized_distance("machinelearning", "machinelearning"), 0)
})

test_that("normalized_distance returns 1 for completely different strings", {
  result <- normalized_distance("abc", "xyz")
  expect_true(result > 0.9)
})

test_that("normalized_distance returns value between 0 and 1", {
  result <- normalized_distance("hello", "hallo")
  expect_true(result >= 0)
  expect_true(result <= 1)
})

test_that("normalized_distance handles empty strings", {
  expect_equal(normalized_distance("", ""), 0)
  expect_equal(normalized_distance("hello", ""), 1)
  expect_equal(normalized_distance("", "hello"), 1)
})

test_that("normalized_distance handles NA values", {
  expect_true(is.na(normalized_distance(NA, "hello")))
  expect_true(is.na(normalized_distance("hello", NA)))
  expect_true(is.na(normalized_distance(NA, NA)))
})

test_that("normalized_distance calculation is correct", {
  # "machinelearning" vs "machinelearninng" (one character insertion)
  a <- "machinelearning"
  b <- "machinelearninng"
  result <- normalized_distance(a, b)
  expected <- 1 / max(nchar(a), nchar(b))  # distance 1, max length 16
  expect_equal(result, expected, tolerance = 0.001)
})

test_that("normalized_distance is symmetric", {
  dist_ab <- normalized_distance("hello", "hallo")
  dist_ba <- normalized_distance("hallo", "hello")
  expect_equal(dist_ab, dist_ba)
})

# ------------------------------------------------------------------------------
# Test: calculate_distances()
# ------------------------------------------------------------------------------

test_that("calculate_distances returns correct length", {
  corpus <- c("one", "two", "three", "four")
  result <- calculate_distances("test", corpus)
  expect_length(result, 4)
})

test_that("calculate_distances handles NA query", {
  corpus <- c("one", "two", "three")
  result <- calculate_distances(NA, corpus)
  expect_true(all(is.na(result)))
})

test_that("calculate_distances handles empty corpus", {
  result <- calculate_distances("test", character())
  expect_length(result, 0)
})

# ------------------------------------------------------------------------------
# Test: find_best_match()
# ------------------------------------------------------------------------------

test_that("find_best_match finds exact match", {
  corpus <- c("apple", "banana", "cherry", "date")
  result <- find_best_match("banana", corpus)

  expect_equal(result$best_match, "banana")
  expect_equal(result$distance, 0)
})

test_that("find_best_match finds closest match", {
  corpus <- c("machinelearning", "deeplearning", "reinforcementlearning")
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

test_that("find_best_match handles empty query", {
  corpus <- c("one", "two", "three")
  result <- find_best_match("", corpus)

  expect_true(is.na(result$best_match))
})

test_that("find_best_match handles empty corpus", {
  result <- find_best_match("test", character())

  expect_true(is.na(result$best_match))
  expect_true(is.na(result$distance))
})

test_that("find_best_match returns corpus IDs when provided", {
  corpus <- c("apple", "banana", "cherry")
  ids <- c("id1", "id2", "id3")
  result <- find_best_match("banana", corpus, corpus_ids = ids)

  expect_equal(result$match_id, "id2")
})

# ------------------------------------------------------------------------------
# Test: batch_fuzzy_match()
# ------------------------------------------------------------------------------

test_that("batch_fuzzy_match returns tibble with correct columns", {
  queries <- c("apple", "banan")
  corpus <- c("apple", "banana", "cherry")

  result <- batch_fuzzy_match(queries, corpus, parallel = FALSE, progress = FALSE)

  expect_s3_class(result, "tbl_df")
  expect_true("query" %in% names(result))
  expect_true("best_match" %in% names(result))
  expect_true("match_distance" %in% names(result))
  expect_true("potential_hallucination" %in% names(result))
})

test_that("batch_fuzzy_match handles multiple queries", {
  queries <- c("one", "two", "three")
  corpus <- c("one", "two", "three", "four")

  result <- batch_fuzzy_match(queries, corpus, parallel = FALSE, progress = FALSE)

  expect_equal(nrow(result), 3)
  # All should be exact matches
  expect_true(all(result$match_distance == 0))
})

test_that("batch_fuzzy_match classifies hallucinations", {
  queries <- c("exactmatch", "completelydifferentstring")
  corpus <- c("exactmatch", "anotherthing")

  result <- batch_fuzzy_match(queries, corpus, parallel = FALSE, progress = FALSE)

  expect_false(result$potential_hallucination[1])  # exact match
  # Second query should have high distance
  expect_true(result$match_distance[2] > 0.5)
})

# ------------------------------------------------------------------------------
# Test: classify_match()
# ------------------------------------------------------------------------------

test_that("classify_match categorizes correctly", {
  expect_equal(as.character(classify_match(0)), "exact")
  expect_equal(as.character(classify_match(0.03)), "high_confidence")
  expect_equal(as.character(classify_match(0.10)), "medium_confidence")
  expect_equal(as.character(classify_match(0.25)), "low_confidence")
  expect_equal(as.character(classify_match(0.60)), "no_match")
  expect_equal(as.character(classify_match(NA)), "unknown")
})

# ------------------------------------------------------------------------------
# Test: Known examples
# ------------------------------------------------------------------------------

test_that("example from documentation works correctly", {
  # From the task description example
  a <- "smithjmachinelearningineconomics"
  b <- "smithjmachinelearningineconomcis"  # typo: cis vs ics

  distance <- normalized_distance(a, b)

  # Should be approximately 2/32 = 0.0625
  expect_true(distance > 0.05)
  expect_true(distance < 0.10)
})

# ------------------------------------------------------------------------------
# Run tests
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  test_results <- test_dir("tests/", reporter = "summary")
}
