# Tests for S4 classes

test_that("QsortData can be created from matrix", {
  # Create test data
  sorts <- matrix(sample(-4:4, 100, replace = TRUE), nrow = 10, ncol = 10)
  rownames(sorts) <- paste0("P", 1:10)
  colnames(sorts) <- paste0("S", 1:10)

  # Create QsortData
  qdata <- qsort_data(sorts, validate = FALSE)

  expect_s4_class(qdata, "QsortData")
  expect_equal(nrow(qdata@sorts), 10)
  expect_equal(ncol(qdata@sorts), 10)
  expect_equal(length(qdata@participants), 10)
  expect_equal(length(qdata@statements), 10)
})

test_that("QsortData handles data.frame input", {
  df <- data.frame(
    id = paste0("P", 1:5),
    s1 = sample(-2:2, 5, replace = TRUE),
    s2 = sample(-2:2, 5, replace = TRUE),
    s3 = sample(-2:2, 5, replace = TRUE)
  )

  qdata <- qsort_data(df[, -1], participants = df$id, validate = FALSE)

  expect_s4_class(qdata, "QsortData")
  expect_equal(nrow(qdata@sorts), 5)
})

test_that("QsortData generates default IDs when missing", {
  sorts <- matrix(1:20, nrow = 4, ncol = 5)

  qdata <- qsort_data(sorts, validate = FALSE)

  expect_match(qdata@participants[1], "^P")
  expect_match(qdata@statements[1], "^S")
})

test_that("show method works for QsortData", {
  sorts <- matrix(sample(-2:2, 25, replace = TRUE), nrow = 5, ncol = 5)
  qdata <- qsort_data(sorts, validate = FALSE)

  # show() should produce output without error
  # cli package uses ANSI codes so we just check it produces output
  expect_output(show(qdata), regexp = NULL)
})
