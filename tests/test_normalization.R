# ==============================================================================
# TESTS: Text Normalization
# ==============================================================================

library(testthat)

source("src/00_config.R")
source("src/02_text_normalization.R")

# ------------------------------------------------------------------------------
# Test: normalize()
# ------------------------------------------------------------------------------

test_that("normalize converts to lowercase", {
  expect_equal(normalize("HELLO"), "hello")
})

test_that("normalize removes spaces and punctuation", {
  expect_equal(normalize("Hello, World!"), "helloworld")
})

test_that("normalize keeps numbers", {
  expect_equal(normalize("Smith (2020)"), "smith2020")
})

test_that("normalize handles typical citation", {
  input <- "Smith, J. (2020). Machine Learning."
  expect_equal(normalize(input), "smithj2020machinelearning")
})

test_that("normalize handles vectors", {
  input <- c("Hello", "World")
  expect_equal(normalize(input), c("hello", "world"))
})

test_that("normalize handles NA", {
  expect_true(is.na(normalize(NA)))
})

test_that("normalize handles empty string", {
  expect_true(is.na(normalize("")))
})

# ------------------------------------------------------------------------------
# Run tests
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  test_file("tests/test_normalization.R")
}
