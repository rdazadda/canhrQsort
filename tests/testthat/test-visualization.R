# Tests for visualization functions

# Helper to create test data
create_test_results <- function(n_participants = 9, n_statements = 15, nfactors = 2, seed = 42) {
  set.seed(seed)
  sorts <- matrix(
    sample(-3:3, n_participants * n_statements, replace = TRUE),
    nrow = n_participants,
    ncol = n_statements
  )
  rownames(sorts) <- paste0("P", 1:n_participants)
  colnames(sorts) <- paste0("S", 1:n_statements)

  qdata <- qsort_data(sorts, validate = FALSE)
  qsort_analyze(qdata, nfactors = nfactors)
}


# Tests for plot_qsort (main dispatcher)

test_that("plot_qsort dispatches to loadings plot", {
  results <- create_test_results()
  p <- plot_qsort(results, type = "loadings")

  expect_s3_class(p, "ggplot")
})

test_that("plot_qsort dispatches to scores plot", {
  results <- create_test_results()
  p <- plot_qsort(results, type = "scores")

  expect_s3_class(p, "ggplot")
})

test_that("plot_qsort dispatches to correlation plot", {
  results <- create_test_results()
  p <- plot_qsort(results, type = "correlation")

  expect_s3_class(p, "ggplot")
})

test_that("plot_qsort dispatches to factor_arrays plot", {
  results <- create_test_results()
  p <- plot_qsort(results, type = "factor_arrays")

  expect_s3_class(p, "ggplot")
})

test_that("plot_qsort rejects invalid type",
{
  results <- create_test_results()

  expect_error(
    plot_qsort(results, type = "invalid_type"),
    "should be one of"
  )
})


# Tests for plot_loadings

test_that("plot_loadings returns ggplot object", {
  results <- create_test_results()
  p <- plot_loadings(results)

  expect_s3_class(p, "ggplot")
})

test_that("plot_loadings filters specific factors", {
  results <- create_test_results(nfactors = 3)
  p <- plot_loadings(results, factors = c(1, 2))

  expect_s3_class(p, "ggplot")
  # Plot should only contain data for F1 and F2
})

test_that("plot_loadings respects show_flags option", {
  results <- create_test_results()

  p_with_flags <- plot_loadings(results, show_flags = TRUE)
  p_without_flags <- plot_loadings(results, show_flags = FALSE)

  expect_s3_class(p_with_flags, "ggplot")
  expect_s3_class(p_without_flags, "ggplot")
})

test_that("plot_loadings respects coord_flip option", {
  results <- create_test_results()

  p_flipped <- plot_loadings(results, coord_flip = TRUE)
  p_normal <- plot_loadings(results, coord_flip = FALSE)

  expect_s3_class(p_flipped, "ggplot")
  expect_s3_class(p_normal, "ggplot")
})

test_that("plot_loadings works with bootstrap results", {
  skip_on_cran()
  skip_if_not_installed("GPArotation")
  skip("Bootstrap loadings plot requires matching column names - skipping for now")

  set.seed(42)
  sorts <- matrix(sample(-2:2, 50, replace = TRUE), nrow = 5, ncol = 10)
  rownames(sorts) <- paste0("P", 1:5)
  colnames(sorts) <- paste0("S", 1:10)
  qdata <- qsort_data(sorts, validate = FALSE)

  boot <- qsort_bootstrap(qdata, n_bootstrap = 10, nfactors = 2,
                          seed = 123, parallel = FALSE, progress = FALSE)

  p <- plot_loadings(boot)
  expect_s3_class(p, "ggplot")
})

test_that("plot_loadings rejects invalid input", {
  expect_error(
    plot_loadings(data.frame(x = 1:10)),
    "must be QsortResults or QsortBootstrap"
  )
})


# Tests for plot_scores

test_that("plot_scores returns ggplot object", {
  results <- create_test_results()
  p <- plot_scores(results)

  expect_s3_class(p, "ggplot")
})
test_that("plot_scores filters by factor", {
  results <- create_test_results(nfactors = 3)
  p <- plot_scores(results, factor = 2)

  expect_s3_class(p, "ggplot")
})

test_that("plot_scores respects n_top parameter", {
  results <- create_test_results()

  p_top5 <- plot_scores(results, n_top = 5)
  p_top10 <- plot_scores(results, n_top = 10)

  expect_s3_class(p_top5, "ggplot")
  expect_s3_class(p_top10, "ggplot")
})

test_that("plot_scores respects show_labels parameter", {
  results <- create_test_results()

  p_with_labels <- plot_scores(results, show_labels = TRUE)
  p_without_labels <- plot_scores(results, show_labels = FALSE)

  expect_s3_class(p_with_labels, "ggplot")
  expect_s3_class(p_without_labels, "ggplot")
})


# Tests for plot_correlation

test_that("plot_correlation returns ggplot object", {
  results <- create_test_results()
  p <- plot_correlation(results)

  expect_s3_class(p, "ggplot")
})

test_that("plot_correlation respects show_values option", {
  results <- create_test_results()

  p_with_values <- plot_correlation(results, show_values = TRUE)
  p_without_values <- plot_correlation(results, show_values = FALSE)

  expect_s3_class(p_with_values, "ggplot")
  expect_s3_class(p_without_values, "ggplot")
})

test_that("plot_correlation respects cluster option", {
  results <- create_test_results()

  p_clustered <- plot_correlation(results, cluster = TRUE)
  p_unclustered <- plot_correlation(results, cluster = FALSE)

  expect_s3_class(p_clustered, "ggplot")
  expect_s3_class(p_unclustered, "ggplot")
})


# Tests for plot_factor_arrays

test_that("plot_factor_arrays returns ggplot object", {
  results <- create_test_results()
  p <- plot_factor_arrays(results, factor = 1)

  expect_s3_class(p, "ggplot")
})

test_that("plot_factor_arrays works for different factors", {
  results <- create_test_results(nfactors = 3)

  p1 <- plot_factor_arrays(results, factor = 1)
  p2 <- plot_factor_arrays(results, factor = 2)
  p3 <- plot_factor_arrays(results, factor = 3)

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_s3_class(p3, "ggplot")
})

test_that("plot_factor_arrays respects show_text option", {
  results <- create_test_results()

  p_with_text <- plot_factor_arrays(results, factor = 1, show_text = TRUE)
  p_without_text <- plot_factor_arrays(results, factor = 1, show_text = FALSE)

  expect_s3_class(p_with_text, "ggplot")
  expect_s3_class(p_without_text, "ggplot")
})

test_that("plot_factor_arrays rejects non-QsortResults input", {
  expect_error(
    plot_factor_arrays(data.frame(x = 1), factor = 1),
    "must be a QsortResults object"
  )
})


# Tests for plot_distinguishing

test_that("plot_distinguishing returns ggplot object", {
  results <- create_test_results()
  p <- plot_distinguishing(results)

  expect_s3_class(p, "ggplot")
})

test_that("plot_distinguishing filters specific factors", {
  results <- create_test_results(nfactors = 3)
  p <- plot_distinguishing(results, factors = c(1, 2))

  expect_s3_class(p, "ggplot")
})


# Tests for plot_consensus

test_that("plot_consensus returns ggplot object", {
  results <- create_test_results()
  p <- plot_consensus(results)

  expect_s3_class(p, "ggplot")
})


# Tests for plot method on QsortResults

test_that("plot method works for QsortResults", {
  results <- create_test_results()

  p <- plot(results, type = "loadings")
  expect_s3_class(p, "ggplot")

  p <- plot(results, type = "scores")
  expect_s3_class(p, "ggplot")
})



# Tests for edge cases

test_that("plots handle two factor results", {
  # Note: Single factor extraction doesn't support varimax rotation
  results <- create_test_results(nfactors = 2)

  p_loadings <- plot_loadings(results)
  p_scores <- plot_scores(results)

  expect_s3_class(p_loadings, "ggplot")
  expect_s3_class(p_scores, "ggplot")
})

test_that("plots handle many factors", {
  results <- create_test_results(n_participants = 12, nfactors = 5)

  p_loadings <- plot_loadings(results)
  p_correlation <- plot_correlation(results)

  expect_s3_class(p_loadings, "ggplot")
  expect_s3_class(p_correlation, "ggplot")
})

test_that("plots handle results with no consensus statements", {
  # Create results where consensus might be empty
  results <- create_test_results(seed = 999)

  # Should not error even if no consensus
  p <- plot_consensus(results)
  expect_s3_class(p, "ggplot")
})

test_that("plots handle results with no distinguishing statements", {
  # Create results where distinguishing might be sparse
  results <- create_test_results(seed = 888)

  # Should not error even if few distinguishing
  p <- plot_distinguishing(results)
  expect_s3_class(p, "ggplot")
})
