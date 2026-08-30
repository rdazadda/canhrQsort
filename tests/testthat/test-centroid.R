# Tests for centroid extraction method (Brown, 1980)

test_that("centroid_extraction produces valid loadings matrix", {
  skip_on_cran()

  set.seed(42)
  sorts <- matrix(rnorm(100), nrow = 10, ncol = 10)
  cor_matrix <- cor(t(sorts))

  # centroid_extraction returns a matrix directly
  result <- canhrQsort:::centroid_extraction(cor_matrix, nfactors = 2)

  expect_true(is.matrix(result))
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), 10)
  expect_true(all(abs(result) <= 1))  # Loadings should be in [-1, 1]
})

test_that("centroid applies positive manifold reflection", {
  # Create correlation matrix that needs reflection
  set.seed(123)
  sorts <- matrix(rnorm(50), nrow = 5, ncol = 10)
  cor_matrix <- cor(t(sorts))

  # Force some negative correlations
  cor_matrix[1, 2] <- -0.5
  cor_matrix[2, 1] <- -0.5

  result <- canhrQsort:::centroid_extraction(cor_matrix, nfactors = 2)

  # Loadings should be computed despite negative correlations
  expect_true(is.matrix(result))
  expect_true(all(is.finite(result)))
})

test_that("centroid converges within iterations", {
  set.seed(456)
  sorts <- matrix(sample(-3:3, 100, replace = TRUE), nrow = 10, ncol = 10)
  cor_matrix <- cor(t(sorts))

  result <- canhrQsort:::centroid_extraction(
    cor_matrix,
    nfactors = 3,
    max_iter = 50,
    tolerance = 1e-5
  )

  # Should complete without error and return matrix
  expect_true(is.matrix(result))
  expect_equal(ncol(result), 3)
})

test_that("centroid matches qsort_extract interface", {
  set.seed(789)
  sorts <- matrix(rnorm(80), nrow = 8, ncol = 10)
  cor_matrix <- cor(t(sorts))

  # Use main interface with centroid method
  result <- qsort_extract(cor_matrix, nfactors = 2, method = "centroid")

  expect_true(is.list(result))
  expect_equal(result$method, "centroid")
  expect_equal(ncol(result$loadings), 2)
  expect_true("variance_explained" %in% names(result))
})

test_that("centroid handles edge case of near-zero T value", {
  # Create a near-singular correlation matrix
  set.seed(101)
  sorts <- matrix(rep(1:10, each = 5), nrow = 5, ncol = 10)
  sorts <- sorts + matrix(rnorm(50, sd = 0.01), nrow = 5)
  cor_matrix <- cor(t(sorts))

  # Should handle this gracefully (may warn but not error)
  result <- tryCatch({
    canhrQsort:::centroid_extraction(cor_matrix, nfactors = 2)
  }, warning = function(w) {
    # Expected warning about numerical stability
    suppressWarnings(canhrQsort:::centroid_extraction(cor_matrix, nfactors = 2))
  })

  expect_true(is.matrix(result))
})

test_that("PCA and centroid give comparable results", {
  set.seed(202)
  sorts <- matrix(sample(-4:4, 180, replace = TRUE), nrow = 10, ncol = 18)
  cor_matrix <- cor(t(sorts))

  pca_result <- qsort_extract(cor_matrix, nfactors = 3, method = "pca")
  centroid_result <- qsort_extract(cor_matrix, nfactors = 3, method = "centroid")

  # Both should produce loadings in similar range
  expect_true(all(abs(pca_result$loadings) <= 1))
  expect_true(all(abs(centroid_result$loadings) <= 1))

  # Dimensions should match
  expect_equal(dim(pca_result$loadings), dim(centroid_result$loadings))
})
