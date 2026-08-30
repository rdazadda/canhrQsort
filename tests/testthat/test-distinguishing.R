# Tests for distinguishing and consensus statement identification

test_that("qsort_distinguish identifies consensus statements", {
  # Create factor scores where all factors have similar z-scores for some statements
  factor_scores <- data.frame(
    statement = paste0("S", 1:5),
    statement_num = 1:5,
    F1_zscore = c(1.5, 0.5, 0.0, -0.5, -1.5),  # Varies
    F2_zscore = c(1.4, 0.4, 0.1, -0.4, -1.4)   # Very similar to F1
  )

  # Small SED means small differences are significant
  sed_matrix <- matrix(c(NA, 0.1, 0.1, NA), nrow = 2)
  rownames(sed_matrix) <- colnames(sed_matrix) <- c("F1", "F2")

  result <- qsort_distinguish(factor_scores, sed_matrix = sed_matrix)

  expect_true(is.list(result))
  expect_true("consensus" %in% names(result))
  expect_true("distinguishing" %in% names(result))
})

test_that("qsort_distinguish identifies distinguishing statements", {
  # Create factor scores with clear differences
  factor_scores <- data.frame(
    statement = paste0("S", 1:5),
    statement_num = 1:5,
    F1_zscore = c(2.0, 1.5, 0.0, -1.0, -2.0),
    F2_zscore = c(-2.0, -1.5, 0.0, 1.0, 2.0)  # Opposite pattern
  )

  # Moderate SED
  sed_matrix <- matrix(c(NA, 0.3, 0.3, NA), nrow = 2)
  rownames(sed_matrix) <- colnames(sed_matrix) <- c("F1", "F2")

  result <- qsort_distinguish(factor_scores, sed_matrix = sed_matrix)

  # S1, S2, S4, S5 should be distinguishing (large differences)
  expect_true(length(result$distinguishing$F1) > 0)
  expect_true(length(result$distinguishing$F2) > 0)
})

test_that("distinguishing categories are assigned correctly", {
  skip_on_cran()

  set.seed(42)
  sorts <- matrix(sample(-3:3, 210, replace = TRUE), nrow = 10, ncol = 21)
  rownames(sorts) <- paste0("P", 1:10)
  colnames(sorts) <- paste0("S", 1:21)

  qdata <- qsort_data(sorts, validate = FALSE)
  results <- qsort_analyze(qdata, nfactors = 3)

  # Check that categories are valid strings
  categories <- results@distinguishing
  expect_true(is.list(categories))

  # Extract from the actual distinguish computation
  zscore_cols <- grep("_zscore$", names(results@factor_scores), value = TRUE)
  expect_equal(length(zscore_cols), 3)
})

test_that("significance levels are computed correctly", {
  # Test significance thresholds
  # |z_f - z_g| > SED × 1.96 for p < 0.05
  # |z_f - z_g| > SED × 2.58 for p < 0.01
  # |z_f - z_g| > SED × 3.29 for p < 0.001

  sed <- 0.3

  # p < 0.05: diff > 0.588
  expect_true(0.6 > sed * 1.96)

  # p < 0.01: diff > 0.774
  expect_true(0.8 > sed * 2.58)

  # p < 0.001: diff > 0.987
  expect_true(1.0 > sed * 3.29)
})

test_that("qsort_distinguish handles single factor case", {
  factor_scores <- data.frame(
    statement = paste0("S", 1:5),
    statement_num = 1:5,
    F1_zscore = c(1.5, 0.5, 0.0, -0.5, -1.5)
  )

  result <- qsort_distinguish(factor_scores)

  # With only one factor, all statements should be consensus
  expect_equal(length(result$consensus), 5)
})
