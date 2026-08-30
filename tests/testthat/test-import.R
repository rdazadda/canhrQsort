# Tests for data import functions

test_that("process_wide_format extracts numeric columns", {
  df <- data.frame(
    id = c("A", "B", "C"),
    name = c("Alice", "Bob", "Charlie"),
    s1 = c(1, 2, 3),
    s2 = c(4, 5, 6),
    s3 = c(7, 8, 9),
    stringsAsFactors = FALSE
  )

  result <- canhrQsort:::process_wide_format(df, id_col = "id")

  expect_equal(nrow(result), 3)
  # Result should have numeric columns (may or may not include name depending on detection)
  expect_true(ncol(result) >= 3)
})

test_that("process_wide_format handles numeric ID column", {
  df <- data.frame(
    id = 1:5,
    s1 = sample(-2:2, 5),
    s2 = sample(-2:2, 5)
  )

  result <- canhrQsort:::process_wide_format(df, id_col = 1)

  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 2)
})

test_that("infer_distribution works with typical Q-sort data", {
  # Create data with -4 to +4 distribution
  sorts <- matrix(
    sample(-4:4, 270, replace = TRUE),
    nrow = 10, ncol = 27
  )

  dist <- infer_distribution(sorts)

  expect_equal(length(dist), 9)  # -4 to +4
  expect_true(all(dist >= 0))
})

test_that("infer_distribution handles edge cases", {
  # Empty matrix
  expect_equal(length(infer_distribution(matrix(nrow = 0, ncol = 0))), 0)

  # Single value
  single <- matrix(1, nrow = 3, ncol = 5)
  dist <- infer_distribution(single)
  expect_equal(dist, 5)  # All 5 items have value 1

  # NULL input
  expect_equal(length(infer_distribution(NULL)), 0)
})

test_that("read_statements parses text correctly", {
  skip_on_cran()

  # Create temp file with statements
  temp_file <- tempfile(fileext = ".txt")
  writeLines(c(
    "Statement one",
    "Statement two",
    "Statement three"
  ), temp_file)

  statements <- read_statements(temp_file)

  expect_equal(length(statements), 3)
  # read_statements returns named character vector
  expect_equal(unname(statements[1]), "Statement one")
  expect_true(!is.null(names(statements)))

  unlink(temp_file)
})


test_that("excel with a statement-number column and a Statements sheet imports transposed", {
  skip_if_not_installed("writexl")

  data_sheet <- data.frame(
    `Statement Num.` = 1:6,
    `KAE-001` = c(1, -1, 0, 2, -2, 0),
    `KAE-002` = c(0, 1, -1, 0, 1, -1),
    `KAE-003` = c(-1, 0, 1, -2, 2, 0),
    check.names = FALSE
  )
  stmt_sheet <- data.frame(
    `Statement Num.` = c(3, 1, 2, 6, 4, 5),
    Statements = c("Statement three", "Statement one", "Statement two",
                   "Statement six", "Statement four", "Statement five"),
    check.names = FALSE
  )
  f <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(Data = data_sheet, Statements = stmt_sheet), f)
  on.exit(unlink(f), add = TRUE)

  qd <- suppressMessages(suppressWarnings(read_qsort_excel(f)))

  expect_identical(length(qd@participants), 3L)
  expect_identical(qd@participants, c("KAE-001", "KAE-002", "KAE-003"))
  expect_identical(length(qd@statements), 6L)
  # Statements sheet is reordered by its numeric id column
  expect_identical(qd@statements[1], "Statement one")
  expect_identical(qd@statements[6], "Statement six")
  # Values land participant x statement
  expect_identical(unname(qd@sorts["KAE-001", 1]), 1)
  expect_identical(unname(qd@sorts["KAE-003", 5]), 2)
})
