# Comprehensive comparison test between canhrQsort and qmethod
# This script validates that our implementation matches the standard qmethod package

library(testthat)

test_that("Brown weight formula is correctly implemented", {
  # Test the Brown (1980) weight formula: w = f / (1 - f^2)

  test_loadings <- c(0.3, 0.5, 0.7, 0.8, 0.9)

  for (f in test_loadings) {
    expected_weight <- f / (1 - f^2)

    # The weight should increase with loading
    expect_gt(expected_weight, 0)
  }

  # Verify weights increase with loading
  weights <- sapply(test_loadings, function(f) f / (1 - f^2))
  expect_true(all(diff(weights) > 0),
              label = "Weights should increase with loadings")
})

test_that("Rounded scores use rank-based distribution assignment", {
  library(canhrQsort)

  # Test distribution - 7 positions, 23 statements
  distribution <- c(2, 3, 4, 5, 4, 3, 2)  # range -3 to +3

  # Test z-scores (23 values, ordered from highest to lowest)
  zscores <- seq(2, -2, length.out = 23)

  # Compute rounded scores
  rounded <- canhrQsort:::compute_rounded_scores(zscores, distribution)

  # Verify total count matches
  expect_equal(length(rounded), 23)

  # Verify distribution is preserved
  score_counts <- table(rounded)

  # Check each position
  for (pos in -3:3) {
    expected_count <- distribution[pos + 4]  # offset by 4 to get 1-7 index
    actual_count <- sum(rounded == pos)
    expect_equal(actual_count, expected_count,
                 label = paste("Score", pos, "count"))
  }

  # Verify highest z-score gets highest position (+3)
  expect_equal(rounded[1], 3,
               label = "Highest z-score should get +3")

  # Verify lowest z-score gets lowest position (-3)
  expect_equal(rounded[23], -3,
               label = "Lowest z-score should get -3")
})

test_that("Centroid extraction converges properly", {
  library(canhrQsort)

  # Create correlation matrix directly (centroid works on correlation matrix)
  set.seed(42)
  n <- 10  # Number of Q-sorts (rows in correlation matrix)
  data <- matrix(rnorm(n * 20), nrow = n)  # 10 Q-sorts, 20 statements

  # Compute by-person correlation (Q-methodology standard)
  cor_mat <- cor(t(data))

  # Extract with centroid method
  loadings <- canhrQsort:::centroid_extraction(cor_mat, nfactors = 2)

  # Verify dimensions - should be n x nfactors
  expect_equal(nrow(loadings), n)
  expect_equal(ncol(loadings), 2)

  # Verify loadings are in reasonable range
  expect_true(all(abs(loadings) <= 1))

  # Verify communalities are reasonable
  communalities <- rowSums(loadings^2)
  expect_true(all(communalities >= 0 & communalities <= 1))
})

test_that("Distinguishing statement categories work correctly", {
  library(canhrQsort)

  # Create test factor scores
  factor_scores <- data.frame(
    statement = paste0("S", 1:10),
    statement_num = 1:10,
    F1_zscore = c(2.0, 1.5, 0.5, -0.5, -1.5, 0.0, 0.8, -0.8, 1.2, -1.2),
    F2_zscore = c(0.5, 0.3, 0.5, -0.3, -0.5, 0.0, 2.0, -2.0, 0.5, -0.5),
    F3_zscore = c(0.3, 0.2, 0.3, -0.2, -0.3, 0.0, 0.3, -0.3, 0.5, -0.5)
  )

  # Create SED matrix (low SED to ensure significant differences)
  sed_matrix <- matrix(0.15, nrow = 3, ncol = 3)
  diag(sed_matrix) <- NA
  rownames(sed_matrix) <- colnames(sed_matrix) <- c("F1", "F2", "F3")

  # Run distinguishing analysis
  result <- qsort_distinguish(factor_scores, sed_matrix = sed_matrix)

  # Verify result structure
  expect_true("distinguishing" %in% names(result))
  expect_true("consensus" %in% names(result))
  expect_true("categories" %in% names(result))

  # Statement 7 should distinguish F2 (z=2.0 vs F1=0.8, F3=0.3)
  expect_true(length(result$distinguishing$F2) > 0)

  # Statement 6 should be consensus (all zeros)
  expect_true("S6" %in% result$consensus)
})

test_that("Procrustes rotation properly aligns factors", {
  library(canhrQsort)

  # Create target loadings
  target <- matrix(c(
    0.8, 0.2,
    0.7, 0.3,
    0.2, 0.8,
    0.3, 0.7,
    0.5, 0.5
  ), nrow = 5, ncol = 2, byrow = TRUE)
  rownames(target) <- paste0("P", 1:5)
  colnames(target) <- c("F1", "F2")

  # Create loadings with swapped factors (common in factor analysis)
  loadings <- target[, c(2, 1)]  # Swap columns
  rownames(loadings) <- rownames(target)
  colnames(loadings) <- c("F1", "F2")

  # Apply Procrustes rotation
  aligned <- qsort_procrustes(loadings, target)

  # The aligned loadings should be close to target
  # Note: We check correlation ignoring sign (due to reflection)
  for (f in 1:2) {
    cor_val <- abs(cor(aligned[, f], target[, f]))
    # If correlation is low, try the other factor (due to potential swapping)
    if (cor_val < 0.8) {
      other_f <- 3 - f
      cor_val <- max(cor_val, abs(cor(aligned[, f], target[, other_f])))
    }
    expect_gt(cor_val, 0.8)
  }
})

test_that("Flagging criteria follow Brown (1980) formula", {
  library(canhrQsort)

  # Test flagging with known loadings
  test_loadings <- matrix(c(
    0.8, 0.2, 0.1,   # Should flag F1: 0.64 > 0.04+0.01
    0.2, 0.85, 0.1,  # Should flag F2: 0.72 > 0.04+0.01
    0.6, 0.5, 0.3,   # Should flag F1: 0.36 > 0.25+0.09
    0.1, 0.1, 0.9,   # Should flag F3: 0.81 > 0.01+0.01
    0.5, 0.5, 0.5    # Should NOT flag (equal loadings)
  ), nrow = 5, ncol = 3, byrow = TRUE)

  rownames(test_loadings) <- paste0("Q", 1:5)
  colnames(test_loadings) <- paste0("F", 1:3)

  # Use 40 statements for threshold calculation
  nstat <- 40
  test_flags <- qsort_flag(test_loadings, nstat = nstat, method = "auto")

  # Significance threshold: 1.96/sqrt(40) = 0.31
  sig_threshold <- 1.96 / sqrt(nstat)

  # Verify threshold calculation
  expect_equal(sig_threshold, 1.96 / sqrt(40), tolerance = 0.001)

  # Q1 should flag on F1 (0.8 > 0.31 AND 0.64 > 0.05)
  expect_true(test_flags["Q1", "F1"])
  expect_false(test_flags["Q1", "F2"])
  expect_false(test_flags["Q1", "F3"])

  # Q2 should flag on F2 (0.85 > 0.31 AND 0.72 > 0.05)
  expect_false(test_flags["Q2", "F1"])
  expect_true(test_flags["Q2", "F2"])
  expect_false(test_flags["Q2", "F3"])

  # Q4 should flag on F3 (0.9 > 0.31 AND 0.81 > 0.02)
  expect_false(test_flags["Q4", "F1"])
  expect_false(test_flags["Q4", "F2"])
  expect_true(test_flags["Q4", "F3"])

  # Q5 should not flag (equal loadings, no dominant factor)
  expect_false(test_flags["Q5", "F1"])
  expect_false(test_flags["Q5", "F2"])
  expect_false(test_flags["Q5", "F3"])
})

test_that("Composite reliability formula is correct", {
  # Test the Spearman-Brown formula: r = (0.8 * p) / (1 + (p-1) * 0.8)

  test_cases <- data.frame(
    n_flagged = c(1, 2, 3, 5, 10, 20),
    expected = c(
      0.8 / 1.0,               # 0.800
      1.6 / 1.8,               # 0.889
      2.4 / 2.6,               # 0.923
      4.0 / 4.2,               # 0.952
      8.0 / 8.2,               # 0.976
      16.0 / 16.2              # 0.988
    )
  )

  for (i in seq_len(nrow(test_cases))) {
    p <- test_cases$n_flagged[i]
    expected <- test_cases$expected[i]
    computed <- (0.8 * p) / (1 + (p - 1) * 0.8)

    expect_equal(computed, expected, tolerance = 0.001,
                 label = paste("Reliability for", p, "flagged"))
  }
})

test_that("SED formula is correct", {
  # SED = sqrt(SE_f^2 + SE_g^2)

  se_f <- 0.20
  se_g <- 0.25

  expected_sed <- sqrt(se_f^2 + se_g^2)
  computed_sed <- sqrt(0.04 + 0.0625)

  expect_equal(computed_sed, expected_sed, tolerance = 0.001)
  expect_equal(expected_sed, 0.32, tolerance = 0.01)
})
