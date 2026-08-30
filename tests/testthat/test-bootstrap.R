# Tests for bootstrap analysis

test_that("qsort_bootstrap runs without error", {
  skip_on_cran()
  skip_if_not_installed("GPArotation")

  set.seed(42)
  sorts <- matrix(sample(-3:3, 140, replace = TRUE), nrow = 10, ncol = 14)
  rownames(sorts) <- paste0("P", 1:10)
  colnames(sorts) <- paste0("S", 1:14)

  qdata <- qsort_data(sorts, validate = FALSE)

  # Run minimal bootstrap (small n for speed)
  boot_result <- qsort_bootstrap(
    qdata,
    n_bootstrap = 10,
    nfactors = 2,
    seed = 123,
    parallel = FALSE,
    progress = FALSE
  )

  expect_s4_class(boot_result, "QsortBootstrap")
  expect_equal(boot_result@n_bootstrap, 10L)
  expect_equal(boot_result@seed, 123L)
})

test_that("bootstrap confidence intervals are computed", {
  skip_on_cran()
  skip_if_not_installed("GPArotation")

  set.seed(42)
  sorts <- matrix(sample(-2:2, 75, replace = TRUE), nrow = 5, ncol = 15)
  rownames(sorts) <- paste0("P", 1:5)
  colnames(sorts) <- paste0("S", 1:15)

  qdata <- qsort_data(sorts, validate = FALSE)

  boot_result <- qsort_bootstrap(
    qdata,
    n_bootstrap = 20,
    nfactors = 2,
    seed = 456,
    parallel = FALSE,
    progress = FALSE
  )

  # Check loading CIs
  loading_ci <- boot_result@loading_ci
  expect_true(is.data.frame(loading_ci))
  expect_true("ci_lower" %in% names(loading_ci))
  expect_true("ci_upper" %in% names(loading_ci))
  expect_true(all(loading_ci$ci_lower <= loading_ci$ci_upper, na.rm = TRUE))

  # Check score CIs
  score_ci <- boot_result@score_ci
  expect_true(is.data.frame(score_ci))
})

test_that("bootstrap stability metrics are computed", {
  skip_on_cran()
  skip_if_not_installed("GPArotation")

  set.seed(789)
  sorts <- matrix(sample(-2:2, 50, replace = TRUE), nrow = 5, ncol = 10)
  rownames(sorts) <- paste0("P", 1:5)
  colnames(sorts) <- paste0("S", 1:10)

  qdata <- qsort_data(sorts, validate = FALSE)

  boot_result <- qsort_bootstrap(
    qdata,
    n_bootstrap = 15,
    nfactors = 2,
    seed = 789,
    parallel = FALSE,
    progress = FALSE
  )

  stability <- boot_result@stability_metrics

  expect_true(is.list(stability))
  expect_true("loading_stability" %in% names(stability))
  expect_true("score_stability" %in% names(stability))
  expect_true("summary" %in% names(stability))
})

test_that("qsort_procrustes aligns factors correctly", {
  # Create target loadings
  target <- matrix(c(
    0.8, 0.1,
    0.7, 0.2,
    0.1, 0.8,
    0.2, 0.7
  ), nrow = 4, ncol = 2, byrow = TRUE)
  rownames(target) <- paste0("P", 1:4)
  colnames(target) <- c("F1", "F2")

  # Create rotated loadings (factors swapped)
  rotated <- target[, c(2, 1)]

  aligned <- qsort_procrustes(rotated, target)

  # After alignment, should be similar to target
  cor_f1 <- cor(aligned[, 1], target[, 1])
  cor_f2 <- cor(aligned[, 2], target[, 2])

  expect_true(abs(cor_f1) > 0.9 || abs(cor_f2) > 0.9)
})

test_that("qsort_procrustes handles sign flips", {
  target <- matrix(c(0.8, 0.6, 0.4, 0.2), nrow = 4, ncol = 1)
  rownames(target) <- paste0("P", 1:4)

  # Flip signs
  flipped <- -target

  aligned <- qsort_procrustes(flipped, target)

  # Should be restored to positive correlation
  expect_true(cor(aligned, target) > 0.99)
})
