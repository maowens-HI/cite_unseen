# ==============================================================================
# TESTS: WOS Parser
# Unit tests for Web of Science reference parsing functions
# ==============================================================================

library(testthat)

# Source the functions to test
source("src/00_config.R")
source("src/data_models.R")
source("src/wos_parser.R")

# ------------------------------------------------------------------------------
# Test Data
# ------------------------------------------------------------------------------

# Sample WOS lines from real data
sample_wos_lines <- c(
  "WOS:000060312800003\tDiepenmaat-Wolters, MGE 1997 147 55 High-performance anion-exchange chromatography method for analysis of propylene glycol alginate in beer JOURNAL OF THE AMERICAN SOCIETY OF BREWING CHEMISTS",
  "WOS:000077689500013\tKlessmann, V 2000 217 4 Practical theology as theory of praxis in church and society INTERNATIONAL JOURNAL OF PRACTICAL THEOLOGY",
  "WOS:000087052800001\tDalgard, O 2002 25 68 Community health profiles INTERNATIONAL JOURNAL OF EPIDEMIOLOGY",
  "WOS:000070939000013\tLercher, P 1997 2502 10 Birth weight, education, environment, and lung function at school age: a community study in an alpine area EUROPEAN RESPIRATORY JOURNAL",
  "WOS:000077316700006\tFarrow, CE 1998 1 105 PDFfit, a program for full profile structural refinement of the atomic pair distribution function JOURNAL OF PHYSICS-CONDENSED MATTER",
  "WOS:000079215100007\tBartos, L 1999 1 25 Social position and response to novel objects and open spaces in red deer hinds BEHAVIOUR"
)

# Reference strings only (without IDs)
sample_references <- c(
  "Diepenmaat-Wolters, MGE 1997 147 55 High-performance anion-exchange chromatography method for analysis of propylene glycol alginate in beer JOURNAL OF THE AMERICAN SOCIETY OF BREWING CHEMISTS",
  "Klessmann, V 2000 217 4 Practical theology as theory of praxis in church and society INTERNATIONAL JOURNAL OF PRACTICAL THEOLOGY"
)

# ------------------------------------------------------------------------------
# Test: parse_wos_reference()
# ------------------------------------------------------------------------------

test_that("parse_wos_reference extracts author correctly", {
  ref <- "Diepenmaat-Wolters, MGE 1997 147 55 High-performance anion-exchange chromatography method for analysis of propylene glycol alginate in beer JOURNAL OF THE AMERICAN SOCIETY OF BREWING CHEMISTS"
  result <- parse_wos_reference(ref)

  expect_equal(result$author, "Diepenmaat-Wolters, MGE")
})

test_that("parse_wos_reference extracts year correctly", {
  ref <- "Smith, J 2005 100 25 Some research title NATURE"
  result <- parse_wos_reference(ref)

  expect_equal(result$year, 2005L)
})

test_that("parse_wos_reference extracts page number correctly", {
  ref <- "Lercher, P 1997 2502 10 Birth weight education EUROPEAN RESPIRATORY JOURNAL"
  result <- parse_wos_reference(ref)

  expect_equal(result$page, 2502L)
})

test_that("parse_wos_reference extracts citation count correctly", {
  ref <- "Farrow, CE 1998 1 105 PDFfit program JOURNAL OF PHYSICS-CONDENSED MATTER"
  result <- parse_wos_reference(ref)

  expect_equal(result$citations, 105L)
})

test_that("parse_wos_reference extracts journal correctly", {
  ref <- "Smith, A 2000 1 1 Some title NATURE MEDICINE"
  result <- parse_wos_reference(ref)

  expect_equal(result$journal, "NATURE MEDICINE")
})

test_that("parse_wos_reference extracts multi-word journal correctly", {
  ref <- "Diepenmaat-Wolters, MGE 1997 147 55 High-performance method JOURNAL OF THE AMERICAN SOCIETY OF BREWING CHEMISTS"
  result <- parse_wos_reference(ref)

  expect_equal(result$journal, "JOURNAL OF THE AMERICAN SOCIETY OF BREWING CHEMISTS")
})

test_that("parse_wos_reference handles hyphenated author names", {
  ref <- "Diepenmaat-Wolters, MGE 1997 147 55 Title JOURNAL NAME"
  result <- parse_wos_reference(ref)

  expect_true(grepl("-", result$author))
})

test_that("parse_wos_reference marks complete parsing", {
  ref <- "Smith, A 2000 100 50 Title JOURNAL NAME"
  result <- parse_wos_reference(ref)

  expect_equal(result$parse_status, "complete")
})

test_that("parse_wos_reference handles NA input", {
  result <- parse_wos_reference(NA)

  expect_equal(result$parse_status, "failed")
  expect_true(is.na(result$author))
  expect_true(is.na(result$year))
})

test_that("parse_wos_reference handles empty string", {
  result <- parse_wos_reference("")

  expect_equal(result$parse_status, "failed")
})

test_that("parse_wos_reference handles NULL input", {
  result <- parse_wos_reference(NULL)

  expect_equal(result$parse_status, "failed")
})

# ------------------------------------------------------------------------------
# Test: parse_wos_line()
# ------------------------------------------------------------------------------

test_that("parse_wos_line extracts WOS ID correctly", {
  line <- "WOS:000060312800003\tDiepenmaat-Wolters, MGE 1997 147 55 Title JOURNAL NAME"
  result <- parse_wos_line(line)

  expect_equal(result$wos_id, "WOS:000060312800003")
})

test_that("parse_wos_line handles all sample lines", {
  for (line in sample_wos_lines) {
    result <- parse_wos_line(line)

    expect_true(grepl("^WOS:", result$wos_id))
    expect_true(!is.na(result$year))
    expect_true(result$year >= 1900 && result$year <= 2100)
  }
})

test_that("parse_wos_line preserves original reference", {
  line <- "WOS:000000001\tSome reference text"
  result <- parse_wos_line(line)

  expect_equal(result$reference, "Some reference text")
})

test_that("parse_wos_line handles missing tab separator", {
  line <- "WOS:000000001 Missing tab separator"
  result <- parse_wos_line(line)

  expect_equal(result$parse_status, "failed")
})

# ------------------------------------------------------------------------------
# Test: Sample data accuracy
# ------------------------------------------------------------------------------

test_that("parse correctly identifies Diepenmaat-Wolters entry", {
  line <- sample_wos_lines[1]
  result <- parse_wos_line(line)

  expect_equal(result$wos_id, "WOS:000060312800003")
  expect_equal(result$author, "Diepenmaat-Wolters, MGE")
  expect_equal(result$year, 1997L)
  expect_equal(result$page, 147L)
  expect_equal(result$citations, 55L)
  expect_true(grepl("BREWING CHEMISTS", result$journal))
})

test_that("parse correctly identifies Lercher entry", {
  line <- sample_wos_lines[4]
  result <- parse_wos_line(line)

  expect_equal(result$wos_id, "WOS:000070939000013")
  expect_equal(result$author, "Lercher, P")
  expect_equal(result$year, 1997L)
  expect_equal(result$page, 2502L)
  expect_equal(result$citations, 10L)
  expect_true(grepl("RESPIRATORY", result$journal))
})

test_that("parse correctly identifies Farrow entry", {
  line <- sample_wos_lines[5]
  result <- parse_wos_line(line)

  expect_equal(result$wos_id, "WOS:000077316700006")
  expect_equal(result$author, "Farrow, CE")
  expect_equal(result$year, 1998L)
  expect_equal(result$page, 1L)
  expect_equal(result$citations, 105L)
  expect_true(grepl("PHYSICS", result$journal))
})

# ------------------------------------------------------------------------------
# Test: Edge cases
# ------------------------------------------------------------------------------

test_that("parse handles references without clear journal end", {
  ref <- "Smith, A 2000 1 1 lowercase title mixed Case"
  result <- parse_wos_reference(ref)

  # Should still attempt to parse with fallback
  expect_true(!is.na(result$year))
  expect_equal(result$year, 2000L)
})

test_that("parse handles very short references", {
  ref <- "A 2000 1 1 T J"
  result <- parse_wos_reference(ref)

  # May fail or partially parse
  expect_true(result$parse_status %in% c("complete", "partial", "failed"))
})

test_that("parse handles very long titles", {
  long_title <- paste(rep("word", 100), collapse = " ")
  ref <- paste("Smith, A 2000 1 1", long_title, "JOURNAL NAME")
  result <- parse_wos_reference(ref)

  expect_true(nchar(result$title) > 0 || result$parse_status == "failed")
})

# ------------------------------------------------------------------------------
# Test: Year validation
# ------------------------------------------------------------------------------

test_that("parse rejects implausible years", {
  ref <- "Smith, A 1500 1 1 Title JOURNAL"
  result <- parse_wos_reference(ref)

  # 1500 is outside typical academic year range
  # Parser should either reject or accept based on fallback behavior
  if (!is.na(result$year)) {
    expect_true(result$year >= 1800)
  }
})

test_that("parse accepts recent years", {
  ref <- "Smith, A 2023 1 1 Title JOURNAL NAME"
  result <- parse_wos_reference(ref)

  expect_equal(result$year, 2023L)
})

# ------------------------------------------------------------------------------
# Test: Batch processing
# ------------------------------------------------------------------------------

test_that("test_wos_parser runs without error", {
  result <- test_wos_parser()

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
  expect_true("wos_id" %in% names(result))
  expect_true("author" %in% names(result))
  expect_true("year" %in% names(result))
})

test_that("all sample data parses with complete status", {
  result <- test_wos_parser()

  # Majority should parse completely
  complete_count <- sum(result$parse_status == "complete")
  expect_true(complete_count / nrow(result) >= 0.8)
})

# ------------------------------------------------------------------------------
# Test: Diagnostics
# ------------------------------------------------------------------------------

test_that("wos_parsing_diagnostics returns expected structure", {
  result <- test_wos_parser()
  diag <- wos_parsing_diagnostics(result)

  expect_true("total_records" %in% names(diag))
  expect_true("parse_status" %in% names(diag))
  expect_true("field_completeness" %in% names(diag))
  expect_true("year_range" %in% names(diag))
})

# ------------------------------------------------------------------------------
# Run tests
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  test_results <- test_file("tests/test_wos_parser.R", reporter = "summary")
}
