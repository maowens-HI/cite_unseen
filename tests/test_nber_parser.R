# ==============================================================================
# TESTS: NBER Citation Parser
# Unit tests for NBER bibliography citation parsing functions
# ==============================================================================

library(testthat)

# Source the functions to test
source("src/00_config.R")
source("src/data_models.R")
source("src/nber_parser.R")

# ------------------------------------------------------------------------------
# Test Data
# ------------------------------------------------------------------------------

# Sample NBER citations from typical formats
sample_nber_citations <- c(
  'Auerbach, A. J. and L. H. Summers (1979): "The Investment Tax Credit: An Evaluation," National Bureau of Economic Research Working Paper Series.',
  'Barro, R. and J. Furman (2018): "Macroeconomic Effects of the 2017 Tax Reform," Brookings Papers on Economic Activity, 49, 257-345.',
  'Feldstein, M. (1995): "The Effect of Marginal Tax Rates on Taxable Income: A Panel Study of the 1986 Tax Reform Act," Journal of Political Economy, 103, 551-572.',
  'Piketty, T., E. Saez, and S. Stantcheva (2014): "Optimal Taxation of Top Labor Incomes: A Tale of Three Elasticities," American Economic Journal: Economic Policy, 6, 230-271.',
  'Chetty, R. (2009): "Is the Taxable Income Elasticity Sufficient to Calculate Deadweight Loss? The Implications of Evasion and Avoidance," American Economic Journal: Economic Policy, 1, 31-52.'
)

# Additional format variations
format_variations <- c(
  'Smith, J. (2020). "Machine Learning Methods." Journal of Economics, 45, 1-25.',  # Period after year
  "Jones, A. (2015): Title Without Quotes, Some Journal, 10, 100-150.",  # No quotes around title
  'Brown, B., C. White, and D. Black (2018): "Joint Work," Review of Economics, 22, 50-75.'  # Three authors
)

# ------------------------------------------------------------------------------
# Test: parse_nber_authors()
# ------------------------------------------------------------------------------

test_that("parse_nber_authors splits two authors with 'and'", {
  result <- parse_nber_authors("Auerbach, A. J. and L. H. Summers")

  expect_length(result, 2)
  expect_equal(result[1], "Auerbach, A. J.")
  expect_equal(result[2], "L. H. Summers")
})

test_that("parse_nber_authors splits three authors with comma and 'and'", {
  result <- parse_nber_authors("Piketty, T., E. Saez, and S. Stantcheva")

  expect_length(result, 3)
  expect_equal(result[1], "Piketty, T.")
})

test_that("parse_nber_authors handles single author", {
  result <- parse_nber_authors("Chetty, R.")

  expect_length(result, 1)
  expect_equal(result[1], "Chetty, R.")
})

test_that("parse_nber_authors handles NA input", {
  result <- parse_nber_authors(NA)

  expect_true(is.na(result))
})

test_that("parse_nber_authors handles empty string", {
  result <- parse_nber_authors("")

  expect_true(is.na(result))
})

# ------------------------------------------------------------------------------
# Test: extract_nber_year()
# ------------------------------------------------------------------------------

test_that("extract_nber_year extracts year in parentheses", {
  result <- extract_nber_year('Smith, A. (2020): "Title"')

  expect_equal(result, 2020L)
})

test_that("extract_nber_year handles year with letter suffix", {
  result <- extract_nber_year('Smith, A. (2020a): "Title"')

  expect_equal(result, 2020L)
})

test_that("extract_nber_year handles various citation years", {
  years <- c(1979, 2018, 1995, 2014, 2009)
  for (i in seq_along(sample_nber_citations)) {
    result <- extract_nber_year(sample_nber_citations[i])
    expect_equal(result, years[i])
  }
})

test_that("extract_nber_year returns NA for missing year", {
  result <- extract_nber_year('Smith, A.: "No Year Here"')

  expect_true(is.na(result))
})

test_that("extract_nber_year rejects implausible years", {
  result <- extract_nber_year('Smith, A. (1500): "Ancient Text"')

  expect_true(is.na(result))
})

# ------------------------------------------------------------------------------
# Test: extract_nber_title()
# ------------------------------------------------------------------------------

test_that("extract_nber_title extracts quoted title", {
  result <- extract_nber_title('Smith (2020): "The Great Title," Journal')

  expect_equal(result, "The Great Title")
})

test_that("extract_nber_title extracts from sample citations", {
  expected_titles <- c(
    "The Investment Tax Credit: An Evaluation",
    "Macroeconomic Effects of the 2017 Tax Reform",
    "The Effect of Marginal Tax Rates on Taxable Income: A Panel Study of the 1986 Tax Reform Act",
    "Optimal Taxation of Top Labor Incomes: A Tale of Three Elasticities",
    "Is the Taxable Income Elasticity Sufficient to Calculate Deadweight Loss? The Implications of Evasion and Avoidance"
  )

  for (i in seq_along(sample_nber_citations)) {
    result <- extract_nber_title(sample_nber_citations[i])
    expect_equal(result, expected_titles[i])
  }
})

test_that("extract_nber_title handles curly quotes", {
  result <- extract_nber_title('Smith (2020): \u201CTitle Here\u201D, Journal')

  expect_equal(result, "Title Here")
})

test_that("extract_nber_title returns NA for missing title", {
  result <- extract_nber_title("Smith (2020), Journal Name")

  expect_true(is.na(result))
})

# ------------------------------------------------------------------------------
# Test: extract_nber_publication()
# ------------------------------------------------------------------------------

test_that("extract_nber_publication extracts journal after quoted title", {
  result <- extract_nber_publication('(2020): "Title," Journal of Economics, 45, 1-25.')

  expect_true(grepl("Journal", result))
})

test_that("extract_nber_publication identifies working paper series", {
  result <- extract_nber_publication(sample_nber_citations[1])

  expect_true(grepl("National Bureau|NBER|Working Paper", result, ignore.case = TRUE))
})

test_that("extract_nber_publication identifies Brookings Papers", {
  result <- extract_nber_publication(sample_nber_citations[2])

  expect_true(grepl("Brookings", result))
})

# ------------------------------------------------------------------------------
# Test: extract_nber_volume()
# ------------------------------------------------------------------------------

test_that("extract_nber_volume extracts volume number", {
  cit <- 'Barro, R. (2018): "Title," Brookings Papers, 49, 257-345.'
  result <- extract_nber_volume(cit)

  expect_equal(result, "49")
})

test_that("extract_nber_volume returns NA for no volume", {
  cit <- 'Smith (2020): "Title," Working Paper Series.'
  result <- extract_nber_volume(cit)

  expect_true(is.na(result))
})

# ------------------------------------------------------------------------------
# Test: extract_nber_pages()
# ------------------------------------------------------------------------------

test_that("extract_nber_pages extracts page range with hyphen", {
  cit <- 'Author (2020): "Title," Journal, 50, 100-150.'
  result <- extract_nber_pages(cit)

  expect_equal(result, "100-150")
})

test_that("extract_nber_pages handles en-dash", {
  cit <- 'Author (2020): "Title," Journal, 50, 100\u2013150.'
  result <- extract_nber_pages(cit)

  expect_equal(result, "100-150")  # Normalized to hyphen
})

test_that("extract_nber_pages returns NA for no pages", {
  cit <- 'Author (2020): "Title," Working Paper.'
  result <- extract_nber_pages(cit)

  expect_true(is.na(result))
})

# ------------------------------------------------------------------------------
# Test: classify_nber_citation()
# ------------------------------------------------------------------------------

test_that("classify_nber_citation identifies working papers", {
  result <- classify_nber_citation(sample_nber_citations[1])

  expect_equal(result, "working_paper")
})

test_that("classify_nber_citation identifies journal articles", {
  result <- classify_nber_citation(sample_nber_citations[3])

  expect_equal(result, "journal_article")
})

test_that("classify_nber_citation identifies books", {
  book_cit <- 'Author (2020): "Title," Cambridge University Press.'
  result <- classify_nber_citation(book_cit)

  expect_equal(result, "book")
})

test_that("classify_nber_citation identifies book chapters", {
  chapter_cit <- 'Author (2020): "Chapter Title," in: Handbook of Economics, eds. Smith.'
  result <- classify_nber_citation(chapter_cit)

  expect_equal(result, "book_chapter")
})

# ------------------------------------------------------------------------------
# Test: parse_nber_citation() - Full parsing
# ------------------------------------------------------------------------------

test_that("parse_nber_citation returns all expected fields", {
  result <- parse_nber_citation(sample_nber_citations[1])

  expect_true("authors" %in% names(result))
  expect_true("authors_string" %in% names(result))
  expect_true("year" %in% names(result))
  expect_true("title" %in% names(result))
  expect_true("publication" %in% names(result))
  expect_true("citation_type" %in% names(result))
  expect_true("parse_status" %in% names(result))
})

test_that("parse_nber_citation correctly parses Auerbach & Summers", {
  result <- parse_nber_citation(sample_nber_citations[1])

  expect_equal(result$year, 1979L)
  expect_equal(result$title, "The Investment Tax Credit: An Evaluation")
  expect_equal(result$citation_type, "working_paper")
  expect_equal(result$parse_status, "complete")
})

test_that("parse_nber_citation correctly parses Barro & Furman", {
  result <- parse_nber_citation(sample_nber_citations[2])

  expect_equal(result$year, 2018L)
  expect_equal(result$title, "Macroeconomic Effects of the 2017 Tax Reform")
  expect_true(grepl("Brookings", result$publication))
  expect_equal(result$parse_status, "complete")
})

test_that("parse_nber_citation correctly parses multi-author citation", {
  result <- parse_nber_citation(sample_nber_citations[4])

  expect_equal(result$year, 2014L)
  expect_true(grepl("Three Elasticities", result$title))
  expect_equal(length(result$authors[[1]]), 3)
})

test_that("parse_nber_citation handles NA input", {
  result <- parse_nber_citation(NA)

  expect_equal(result$parse_status, "failed")
  expect_true(is.na(result$year))
})

test_that("parse_nber_citation handles empty string", {
  result <- parse_nber_citation("")

  expect_equal(result$parse_status, "failed")
})

# ------------------------------------------------------------------------------
# Test: parse_nber_citations() - Batch processing
# ------------------------------------------------------------------------------

test_that("parse_nber_citations handles multiple citations", {
  result <- parse_nber_citations(sample_nber_citations, "test_paper", progress = FALSE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), length(sample_nber_citations))
  expect_true("paper_id" %in% names(result))
  expect_true("citation_num" %in% names(result))
})

test_that("parse_nber_citations assigns sequential citation numbers", {
  result <- parse_nber_citations(sample_nber_citations, "test_paper", progress = FALSE)

  expect_equal(result$citation_num, 1:length(sample_nber_citations))
})

test_that("parse_nber_citations preserves paper_id", {
  result <- parse_nber_citations(sample_nber_citations, "w12345", progress = FALSE)

  expect_true(all(result$paper_id == "w12345"))
})

test_that("parse_nber_citations handles empty input", {
  result <- parse_nber_citations(character(), "empty_paper", progress = FALSE)

  expect_equal(nrow(result), 0)
})

# ------------------------------------------------------------------------------
# Test: Bibliography extraction
# ------------------------------------------------------------------------------

test_that("extract_bibliography finds References section", {
  paper_text <- "
  Introduction text here.

  Methodology section.

  References

  Smith, A. (2020): \"Title,\" Journal.
  Jones, B. (2019): \"Another,\" Review.

  Appendix

  Extra material.
  "

  result <- extract_bibliography(paper_text)

  expect_true(!is.na(result))
  expect_true(grepl("Smith", result))
  expect_true(grepl("Jones", result))
  expect_false(grepl("Appendix", result))
})

test_that("extract_bibliography handles different header formats", {
  paper_text1 <- "\nReferences\n\nCitations here"
  paper_text2 <- "\nBibliography\n\nCitations here"
  paper_text3 <- "\nWorks Cited\n\nCitations here"

  expect_true(!is.na(extract_bibliography(paper_text1)))
  expect_true(!is.na(extract_bibliography(paper_text2)))
  expect_true(!is.na(extract_bibliography(paper_text3)))
})

test_that("extract_bibliography returns NA for no references section", {
  paper_text <- "No references section in this paper."

  result <- extract_bibliography(paper_text)

  expect_true(is.na(result))
})

# ------------------------------------------------------------------------------
# Test: split_bibliography()
# ------------------------------------------------------------------------------

test_that("split_bibliography separates citations", {
  bib_text <- "
  Smith, A. (2020): \"First Title,\" Journal A.

  Jones, B. (2019): \"Second Title,\" Journal B.

  Brown, C. (2018): \"Third Title,\" Journal C.
  "

  result <- split_bibliography(bib_text)

  expect_true(length(result) >= 2)
})

test_that("split_bibliography handles empty input", {
  result <- split_bibliography("")

  expect_length(result, 0)
})

test_that("split_bibliography handles NA input", {
  result <- split_bibliography(NA)

  expect_length(result, 0)
})

# ------------------------------------------------------------------------------
# Test: test_nber_parser()
# ------------------------------------------------------------------------------

test_that("test_nber_parser runs without error", {
  result <- test_nber_parser()

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
})

test_that("test_nber_parser parses most citations completely", {
  result <- test_nber_parser()

  complete_rate <- mean(result$parse_status == "complete")
  expect_true(complete_rate >= 0.8)
})

# ------------------------------------------------------------------------------
# Test: Diagnostics
# ------------------------------------------------------------------------------

test_that("nber_parsing_diagnostics returns expected structure", {
  result <- parse_nber_citations(sample_nber_citations, "test", progress = FALSE)
  diag <- nber_parsing_diagnostics(result)

  expect_true("total_citations" %in% names(diag))
  expect_true("total_papers" %in% names(diag))
  expect_true("parse_status" %in% names(diag))
  expect_true("type_distribution" %in% names(diag))
  expect_true("field_completeness" %in% names(diag))
})

# ------------------------------------------------------------------------------
# Test: Edge cases
# ------------------------------------------------------------------------------

test_that("parse handles citations with special characters", {
  cit <- 'O\'Brien, J. and M\u00fcller, K. (2020): "Special \u2014 Title," Journal.'
  result <- parse_nber_citation(cit)

  expect_true(!is.na(result$year))
})

test_that("parse handles very long citations", {
  long_title <- paste(rep("word", 50), collapse = " ")
  cit <- sprintf('Author (2020): "%s," Journal.', long_title)
  result <- parse_nber_citation(cit)

  expect_equal(result$year, 2020L)
})

test_that("parse handles citations with numbers in titles", {
  cit <- 'Smith (2020): "The 2008 Financial Crisis: A 10-Year Review," Journal.'
  result <- parse_nber_citation(cit)

  expect_true(grepl("2008", result$title))
  expect_equal(result$year, 2020L)  # Should not confuse 2008 with year
})

# ------------------------------------------------------------------------------
# Run tests
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  test_results <- test_file("tests/test_nber_parser.R", reporter = "summary")
}
