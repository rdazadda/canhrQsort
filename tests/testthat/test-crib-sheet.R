# Tests for crib sheet generation

test_that("generate_crib_sheet produces valid output", {
  skip_on_cran()

  # Create test data
  set.seed(42)
  sorts <- matrix(sample(-4:4, 270, replace = TRUE), nrow = 10, ncol = 27)
  rownames(sorts) <- paste0("P", 1:10)
  colnames(sorts) <- paste0("S", 1:27)

  qdata <- qsort_data(sorts, statements = paste("Statement", 1:27), validate = FALSE)
  results <- qsort_analyze(qdata, nfactors = 2)

  # Generate crib sheet for factor 1
  crib <- generate_crib_sheet(results, factor = 1)

  expect_true(is.list(crib))
  expect_equal(crib$factor, "F1")
  expect_true("most_agree" %in% names(crib))
  expect_true("most_disagree" %in% names(crib))
  expect_true("higher_than_all" %in% names(crib))
  expect_true("lower_than_all" %in% names(crib))
  expect_true("narrative_template" %in% names(crib))
})

test_that("generate_crib_sheet handles all factors", {
  skip_on_cran()

  set.seed(123)
  sorts <- matrix(sample(-3:3, 210, replace = TRUE), nrow = 10, ncol = 21)
  rownames(sorts) <- paste0("P", 1:10)
  colnames(sorts) <- paste0("S", 1:21)

  qdata <- qsort_data(sorts, statements = paste("Statement", 1:21), validate = FALSE)
  results <- qsort_analyze(qdata, nfactors = 3)

  # Generate crib sheets for all factors
  crib_all <- generate_crib_sheet(results, factor = NULL)

  expect_true(inherits(crib_all, "list"))
  expect_equal(length(crib_all), 3)  # 3 factors
  expect_true("F1" %in% names(crib_all))
  expect_true("F2" %in% names(crib_all))
  expect_true("F3" %in% names(crib_all))
})

test_that("crib sheet most_agree has correct structure", {
  skip_on_cran()

  set.seed(456)
  sorts <- matrix(sample(-2:2, 50, replace = TRUE), nrow = 5, ncol = 10)
  rownames(sorts) <- paste0("P", 1:5)
  colnames(sorts) <- paste0("S", 1:10)

  qdata <- qsort_data(sorts, statements = paste("Statement", 1:10), validate = FALSE)
  results <- qsort_analyze(qdata, nfactors = 2)

  crib <- generate_crib_sheet(results, factor = 1, n_extreme = 3)

  # Check most_agree structure
  expect_true(is.data.frame(crib$most_agree))
  expect_true("statement_num" %in% names(crib$most_agree))
  expect_true("zscore" %in% names(crib$most_agree))
  expect_lte(nrow(crib$most_agree), 3)  # n_extreme = 3
})

test_that("crib sheet handles edge cases", {
  skip_on_cran()

  # Very small dataset
  set.seed(789)
  sorts <- matrix(sample(-1:1, 15, replace = TRUE), nrow = 3, ncol = 5)
  rownames(sorts) <- paste0("P", 1:3)
  colnames(sorts) <- paste0("S", 1:5)

  qdata <- qsort_data(sorts, statements = paste("Statement", 1:5), validate = FALSE)
  results <- qsort_analyze(qdata, nfactors = 2)

  # Should not error with small n_extreme larger than available statements
  crib <- generate_crib_sheet(results, factor = 1, n_extreme = 10)

  expect_true(is.list(crib))
  expect_lte(nrow(crib$most_agree), 5)  # Can't exceed 5 statements
})
