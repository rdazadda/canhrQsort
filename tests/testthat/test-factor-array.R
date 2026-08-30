# Tests for factor array generation

test_that("generate_factor_array produces valid output", {
  skip_on_cran()

  set.seed(42)
  sorts <- matrix(sample(-4:4, 270, replace = TRUE), nrow = 10, ncol = 27)
  rownames(sorts) <- paste0("P", 1:10)
  colnames(sorts) <- paste0("S", 1:27)

  qdata <- qsort_data(sorts, statements = paste("Statement", 1:27), validate = FALSE)
  results <- qsort_analyze(qdata, nfactors = 2)

  # Generate factor array
  array_result <- generate_factor_array(results, factor_num = 1)

  expect_true(inherits(array_result, "QsortFactorArray"))
  expect_true("array" %in% names(array_result))
  expect_true("distribution" %in% names(array_result))
  expect_true("factor" %in% names(array_result))
  expect_equal(array_result$factor, 1)
})

test_that("factor array contains required columns", {
  skip_on_cran()

  set.seed(123)
  sorts <- matrix(sample(-3:3, 140, replace = TRUE), nrow = 10, ncol = 14)
  rownames(sorts) <- paste0("P", 1:10)
  colnames(sorts) <- paste0("S", 1:14)

  qdata <- qsort_data(sorts, validate = FALSE)
  results <- qsort_analyze(qdata, nfactors = 2)

  array_result <- generate_factor_array(results, factor_num = 1)

  # Check array data frame structure
  expect_true(is.data.frame(array_result$array))
  expect_true("statement_num" %in% names(array_result$array))
  expect_true("statement" %in% names(array_result$array))
  expect_true("zscore" %in% names(array_result$array))
  expect_true("score" %in% names(array_result$array))
})

test_that("factor array scores match distribution", {
  skip_on_cran()

  set.seed(456)
  # Create a forced distribution with known shape
  distribution <- c(1, 2, 3, 4, 3, 2, 1)  # 16 statements, -3 to +3

  sorts <- matrix(sample(-3:3, 160, replace = TRUE), nrow = 10, ncol = 16)
  rownames(sorts) <- paste0("P", 1:10)
  colnames(sorts) <- paste0("S", 1:16)

  qdata <- qsort_data(sorts, distribution = distribution, validate = FALSE)
  results <- qsort_analyze(qdata, nfactors = 2)

  array_result <- generate_factor_array(results, factor_num = 1)

  # Check that all statements are included
  expect_equal(nrow(array_result$array), 16)

  # Check that scores fall within expected range
  score_range <- range(array_result$array$score)
  expect_gte(score_range[1], -3)
  expect_lte(score_range[2], 3)
})

test_that("factor array handles all factors", {
  skip_on_cran()

  set.seed(789)
  sorts <- matrix(sample(-2:2, 100, replace = TRUE), nrow = 10, ncol = 10)
  rownames(sorts) <- paste0("P", 1:10)
  colnames(sorts) <- paste0("S", 1:10)

  qdata <- qsort_data(sorts, validate = FALSE)
  results <- qsort_analyze(qdata, nfactors = 3)

  # Generate arrays for each factor
  for (f in 1:3) {
    array_result <- generate_factor_array(results, factor_num = f)
    expect_equal(array_result$factor, f)
    expect_equal(nrow(array_result$array), 10)
  }
})

test_that("factor array rejects invalid factor numbers", {
  skip_on_cran()

  set.seed(101)
  sorts <- matrix(sample(-2:2, 50, replace = TRUE), nrow = 5, ncol = 10)
  rownames(sorts) <- paste0("P", 1:5)
  colnames(sorts) <- paste0("S", 1:10)

  qdata <- qsort_data(sorts, validate = FALSE)
  results <- qsort_analyze(qdata, nfactors = 2)

  # Should error when factor_num > n_factors
  expect_error(generate_factor_array(results, factor_num = 5))
})

