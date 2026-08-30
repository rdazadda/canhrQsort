# Tests for core analysis functions

test_that("qsort_correlation computes by-person correlation", {
  sorts <- matrix(sample(-4:4, 100, replace = TRUE), nrow = 10, ncol = 10)
  qdata <- qsort_data(sorts, validate = FALSE)

  cor_mat <- qsort_correlation(qdata)

  expect_equal(dim(cor_mat), c(10, 10))
  expect_true(all(diag(cor_mat) == 1))
  expect_true(all(cor_mat >= -1 & cor_mat <= 1))
})

test_that("qsort_extract extracts correct number of factors", {
  set.seed(42)
  sorts <- matrix(rnorm(200), nrow = 20, ncol = 10)
  cor_mat <- cor(t(sorts))

  result <- qsort_extract(cor_mat, nfactors = 3, method = "pca")

  expect_equal(ncol(result$loadings), 3)
  expect_equal(nrow(result$loadings), 20)
  expect_equal(length(result$eigenvalues), 3)
})

test_that("qsort_rotate applies varimax rotation", {
  set.seed(42)
  sorts <- matrix(rnorm(200), nrow = 20, ncol = 10)
  cor_mat <- cor(t(sorts))

  extraction <- qsort_extract(cor_mat, nfactors = 3, method = "pca")
  rotation <- qsort_rotate(extraction, method = "varimax")

  expect_equal(dim(rotation$loadings), dim(extraction$loadings))
  expect_equal(rotation$method, "varimax")
})

test_that("qsort_flag identifies significant loadings", {
  loadings <- matrix(c(
    0.8, 0.1,
    0.7, 0.2,
    0.1, 0.9,
    0.2, 0.8,
    0.4, 0.4
  ), nrow = 5, ncol = 2, byrow = TRUE)
  rownames(loadings) <- paste0("P", 1:5)
  colnames(loadings) <- c("F1", "F2")

  # nstat is required for auto flagging (number of statements)
  flags <- qsort_flag(loadings, nstat = 30, method = "auto")

  expect_true(is.matrix(flags))
  expect_equal(dim(flags), dim(loadings))
  expect_true(is.logical(flags[1, 1]))
})

test_that("qsort_analyze completes full pipeline", {
  set.seed(123)
  sorts <- matrix(sample(-4:4, 150, replace = TRUE), nrow = 15, ncol = 10)
  rownames(sorts) <- paste0("P", 1:15)
  colnames(sorts) <- paste0("S", 1:10)

  qdata <- qsort_data(sorts, validate = FALSE)
  results <- qsort_analyze(qdata, nfactors = 2)

  expect_s4_class(results, "QsortResults")
  expect_equal(results@n_factors, 2L)
  expect_true(!is.null(results@correlation))
  expect_true(!is.null(results@factor_scores))
})
