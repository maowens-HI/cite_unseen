# ==============================================================================
# TESTS: Text Normalization
# Unit tests for citation normalization functions
# ==============================================================================

library(testthat)

# Source the functions to test
source("src/00_config.R")
source("src/02_text_normalization.R")

# ------------------------------------------------------------------------------
# Test: normalize_citation()
# ------------------------------------------------------------------------------

test_that("normalize_citation converts to lowercase", {
  result <- normalize_citation("HELLO WORLD")
  expect_true(result == tolower(result))
})

test_that("normalize_citation removes spaces", {
  result <- normalize_citation("hello world")
  expect_false(grepl(" ", result))
  expect_equal(result, "helloworld")
})

test_that("normalize_citation removes punctuation", {
  result <- normalize_citation("Smith, J. (2020). Title!")
  expect_false(grepl("[,\\.\\(\\)!]", result))
})

test_that("normalize_citation handles typical citation", {
  input <- "Smith, J. (2020). Machine Learning in Economics. Journal of Economic Perspectives, 34(2), 87-108."
  result <- normalize_citation(input, remove_numbers = TRUE)
  expected <- "smithjmachinelearningineconomicsjournalofeconomicperspectives"
  expect_equal(result, expected)
})

test_that("normalize_citation preserves numbers when requested", {
  input <- "Author (2020). Title. Vol 34."
  result <- normalize_citation(input, remove_numbers = FALSE)
  expect_true(grepl("2020", result))
  expect_true(grepl("34", result))
})

test_that("normalize_citation handles empty string", {
  expect_equal(normalize_citation(""), "")
})

test_that("normalize_citation handles NA", {
  expect_true(is.na(normalize_citation(NA)))
})

test_that("normalize_citation handles NULL", {
  expect_true(is.na(normalize_citation(NULL)))
})

test_that("normalize_citation handles special characters", {
  result <- normalize_citation("O'Brien & Mueller-Langer")
  expect_equal(result, "obrienmullerlanger")
})

test_that("normalize_citation handles unicode characters", {
  result <- normalize_citation("Müller, François, José")
  # Should remove or transliterate unicode
  expect_false(grepl("[^a-z0-9]", result))
})

# ------------------------------------------------------------------------------
# Test: normalize_citations() (vectorized)
# ------------------------------------------------------------------------------

test_that("normalize_citations handles vector input", {
  input <- c("Hello World", "Foo Bar", "Test Case")
  result <- normalize_citations(input, progress = FALSE)
  expect_length(result, 3)
  expect_equal(result, c("helloworld", "foobar", "testcase"))
})

test_that("normalize_citations preserves NA positions", {
  input <- c("Hello", NA, "World")
  result <- normalize_citations(input, progress = FALSE)
  expect_true(is.na(result[2]))
  expect_equal(result[1], "hello")
  expect_equal(result[3], "world")
})

test_that("normalize_citations handles empty vector", {
  result <- normalize_citations(character(), progress = FALSE)
  expect_length(result, 0)
})

# ------------------------------------------------------------------------------
# Test: is_valid_normalized()
# ------------------------------------------------------------------------------

test_that("is_valid_normalized rejects short strings", {
  expect_false(is_valid_normalized("abc", min_length = 10))
  expect_true(is_valid_normalized("abcdefghijklmnop", min_length = 10))
})

test_that("is_valid_normalized rejects NA", {
  expect_false(is_valid_normalized(NA))
})

# ------------------------------------------------------------------------------
# Test: Edge cases
# ------------------------------------------------------------------------------

test_that("normalization is consistent across runs", {
  input <- "Smith, J. (2020). Some Random Title. Journal Name, 10(2), 1-20."
  result1 <- normalize_citation(input)
  result2 <- normalize_citation(input)
  expect_identical(result1, result2)
})

test_that("normalization handles very long citations", {
  long_citation <- paste(rep("Author Name Title Journal", 100), collapse = " ")
  result <- normalize_citation(long_citation)
  expect_true(nchar(result) > 0)
})

test_that("normalization handles citations with only numbers", {
  result <- normalize_citation("12345 67890", remove_numbers = TRUE)
  expect_equal(result, "")

  result2 <- normalize_citation("12345 67890", remove_numbers = FALSE)
  expect_equal(result2, "1234567890")
})

# ------------------------------------------------------------------------------
# Run tests
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  test_results <- test_dir("tests/", reporter = "summary")
}
