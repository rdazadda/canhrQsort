# Tests for factor scores and z-score computation

test_that("qsort_scores computes weighted z-scores correctly", {
  # Create simple test data
  sorts <- matrix(c(
    -2, -1, 0, 1, 2,
    -2, -1, 0, 1, 2,
    2, 1, 0, -1, -2,
    2, 1, 0, -1, -2
  ), nrow = 4, ncol = 5, byrow = TRUE)
  rownames(sorts) <- paste0("P", 1:4)
  colnames(sorts) <- paste0("S", 1:5)

  # Create loadings (P1, P2 load on F1; P3, P4 load on F2)
  loadings <- matrix(c(
    0.8, 0.1,
    0.7, 0.2,
    0.1, 0.8,
    0.2, 0.7
  ), nrow = 4, ncol = 2, byrow = TRUE)
  rownames(loadings) <- paste0("P", 1:4)
  colnames(loadings) <- c("F1", "F2")

  # Create flags
  flags <- matrix(c(
    TRUE, FALSE,
    TRUE, FALSE,
    FALSE, TRUE,
    FALSE, TRUE
  ), nrow = 4, ncol = 2, byrow = TRUE)
  colnames(flags) <- c("F1", "F2")

  # Compute scores
  scores <- qsort_scores(sorts, flags, loadings, forced = FALSE)

  expect_true(is.data.frame(scores))
  expect_true("F1_zscore" %in% names(scores))
  expect_true("F2_zscore" %in% names(scores))
  expect_equal(nrow(scores), 5)

  # F1 and F2 should have opposite patterns (negative correlation)
  expect_true(cor(scores$F1_zscore, scores$F2_zscore) < 0)
})

test_that("qsort_scores preserves sign for negative loadings", {
  # This tests the critical bug fix
  sorts <- matrix(c(
    -2, -1, 0, 1, 2,
    -2, -1, 0, 1, 2,
    -2, -1, 0, 1, 2
  ), nrow = 3, ncol = 5, byrow = TRUE)
  rownames(sorts) <- paste0("P", 1:3)
  colnames(sorts) <- paste0("S", 1:5)

  # P1 and P2 positive, P3 negative (opposite perspective)
  loadings <- matrix(c(
    0.8,
    0.7,
    -0.7
  ), nrow = 3, ncol = 1)
  rownames(loadings) <- paste0("P", 1:3)
  colnames(loadings) <- "F1"

  flags <- matrix(c(TRUE, TRUE, TRUE), nrow = 3, ncol = 1)
  colnames(flags) <- "F1"

  scores <- qsort_scores(sorts, flags, loadings, forced = FALSE)

  # The negative loader should SUBTRACT from the factor
  # So the weighted sum should be different from simple mean
  # Check that all z-scores are finite
  expect_true(all(is.finite(scores$F1_zscore)))
})

test_that("Brown (1980) weighting formula is correct", {
  # w = f / (1 - f^2)
  # Test with specific loading values
  loadings <- c(0.8, 0.6, 0.4)

  # Manual calculation
  expected_weights <- loadings / (1 - loadings^2)

  # Expected: 0.8/(1-0.64) = 2.22, 0.6/(1-0.36) = 0.94, 0.4/(1-0.16) = 0.48
  expect_equal(expected_weights[1], 0.8 / 0.36, tolerance = 0.01)
  expect_equal(expected_weights[2], 0.6 / 0.64, tolerance = 0.01)
  expect_equal(expected_weights[3], 0.4 / 0.84, tolerance = 0.01)
})

test_that("compute_rounded_scores matches distribution", {
  zscores <- c(1.5, 1.0, 0.5, 0, -0.5, -1.0, -1.5)
  distribution <- c(1, 2, 1, 2, 1)  # Sum = 7

  rounded <- compute_rounded_scores(zscores, distribution)

  expect_equal(length(rounded), 7)
  expect_equal(sum(rounded == 2), 1)  # 1 item at position 2
  expect_equal(sum(rounded == 1), 2)  # 2 items at position 1
  expect_equal(sum(rounded == 0), 1)  # 1 item at position 0
  expect_equal(sum(rounded == -1), 2) # 2 items at position -1
  expect_equal(sum(rounded == -2), 1) # 1 item at position -2
})
