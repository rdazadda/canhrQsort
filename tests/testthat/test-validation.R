# Tests for data validation functions

test_that("validate_qsort identifies issues", {
  # Create valid data
  sorts <- matrix(sample(-4:4, 100, replace = TRUE), nrow = 10, ncol = 10)
  qdata <- qsort_data(sorts, validate = FALSE)

  result <- validate_qsort(qdata)

  expect_true(is.list(result))
  expect_true("valid" %in% names(result))
  expect_true("issues" %in% names(result))
})

test_that("check_distribution validates distribution conformity", {
  sorts <- matrix(c(
    -2, -1, 0, 1, 2,
    -2, -1, 0, 1, 2,
    -2, -1, 0, 1, 2
  ), nrow = 3, ncol = 5, byrow = TRUE)

  result <- check_distribution(sorts, distribution = c(1, 1, 1, 1, 1))

  expect_true(is.list(result))
  expect_true("conforms" %in% names(result))
})

test_that("detect_careless_response flags suspicious patterns", {
  # Normal data
  normal_sorts <- matrix(sample(-4:4, 80, replace = TRUE), nrow = 8, ncol = 10)

  # Add one with low variance (all same value)
  careless_sort <- rep(0, 10)
  sorts <- rbind(normal_sorts, careless_sort)
  rownames(sorts) <- paste0("P", 1:9)

  result <- detect_careless_response(sorts)

  expect_true(is.list(result))
  expect_true("flagged" %in% names(result))
})

test_that("compute_quality_metrics returns expected metrics", {
  sorts <- matrix(sample(-4:4, 100, replace = TRUE), nrow = 10, ncol = 10)
  qdata <- qsort_data(sorts, validate = FALSE)

  metrics <- compute_quality_metrics(qdata)

  expect_true(is.data.frame(metrics))
  expect_true("metric" %in% names(metrics))
  expect_true("value" %in% names(metrics))
  expect_true("N_Participants" %in% metrics$metric)
})
