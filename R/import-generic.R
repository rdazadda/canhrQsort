#' @title Generic Data Import Functions
#' @description Functions to import Q-sort data from CSV and Excel files
#' @name import-generic
NULL

#' Smart CSV Format Detection
#'
#' @description
#' Reads a CSV/TXT file and auto-detects whether it is an HTMLQ/FlashQ export
#' or a generic Q-sort CSV. Dispatches to the appropriate reader.
#'
#' @param file Path to the CSV/TXT file
#' @param file_name Original file name (for source annotation)
#' @return A QsortData object
#' @keywords internal
smart_detect_csv <- function(file, file_name = basename(file)) {

  # Peek at headers to detect format
  headers <- tryCatch(
    tolower(names(readr::read_csv(file, n_max = 0, show_col_types = FALSE))),
    error = function(e) character(0)
  )

  # HTMLQ markers: uid + datetime columns
  has_htmlq <- any(grepl("^uid$", headers)) &&
    any(grepl("datetime|timestamp", headers))

  # FlashQ markers: id + time columns
  has_flashq <- any(grepl("^id$", headers)) &&
    any(grepl("^time$", headers))

  # Statement column pattern (s1, s2, s3, ...)
  has_stmt_cols <- any(grepl("^s\\d+$", headers))

  # Long format markers: participant + statement + value columns
  has_long <- all(c("participant", "statement", "value") %in% headers)

  # Transposed layout: qsort1, qsort2, ... columns with statement column
  qsort_cols <- grep("^qsort\\d+$", headers)
  has_transposed <- length(qsort_cols) >= 2 &&
    any(headers %in% c("statement", "statements"))

  if (has_htmlq || has_flashq || has_stmt_cols) {
    read_htmlq(file)
  } else if (has_transposed) {
    # Read full data, transpose qsort columns, extract statements
    raw <- readr::read_csv(file, show_col_types = FALSE)
    col_names_lower <- tolower(names(raw))
    qcols <- grep("^qsort\\d+$", col_names_lower)
    sorts <- t(as.matrix(raw[, qcols]))
    stmt_col <- which(col_names_lower %in% c("statement", "statements"))[1]
    stmts <- if (!is.na(stmt_col)) as.character(raw[[stmt_col]]) else NULL
    qsort_data(sorts = sorts, statements = stmts,
               source = paste0("csv:", basename(file)), validate = TRUE)
  } else if (has_long) {
    read_qsort_csv(file, format = "long",
                    participant_col = "participant",
                    statement_col = "statement",
                    value_col = "value")
  } else {
    read_qsort_csv(file)
  }
}

#' Read Q-Sort Data from CSV File
#'
#' @description
#' Import Q-sort data from a CSV file. The function handles multiple common
#' formats including wide format (participants as rows, statements as columns)
#' and long format (one row per participant-statement combination).
#'
#' @param file Path to the CSV file
#' @param format Character; data format - "wide" (default) or "long"
#' @param participant_col For long format, column name containing participant IDs
#' @param statement_col For long format, column name containing statement IDs
#' @param value_col For long format, column name containing sort values
#' @param statements Character vector of statement texts (optional)
#' @param distribution Numeric vector defining forced distribution (optional)
#' @param skip Number of rows to skip at the beginning of the file
#' @param id_col For wide format, column name/index containing participant IDs (NULL = use row names)
#' @param na_values Character vector of strings to interpret as NA
#' @param ... Additional arguments passed to readr::read_csv
#'
#' @return A QsortData object
#' @export
#'
#' @examples
#' \dontrun{
#' # Wide format (most common)
#' qdata <- read_qsort_csv("my_qsorts.csv")
#'
#' # Wide format with first column as participant ID
#' qdata <- read_qsort_csv("my_qsorts.csv", id_col = 1)
#'
#' # Long format
#' qdata <- read_qsort_csv(
#'   "my_qsorts_long.csv",
#'   format = "long",
#'   participant_col = "id",
#'   statement_col = "item",
#'   value_col = "rank"
#' )
#' }
read_qsort_csv <- function(file,
                           format = c("wide", "long"),
                           participant_col = "participant",
                           statement_col = "statement",
                           value_col = "value",
                           statements = NULL,
                           distribution = NULL,
                           skip = 0,
                           id_col = NULL,
                           na_values = c("", "NA", "N/A", "."),
                           ...) {

  format <- match.arg(format)

  cli::cli_alert_info("Reading Q-sort data from: {.file {basename(file)}}")

  # Read the file
  raw_data <- readr::read_csv(
    file,
    skip = skip,
    na = na_values,
    show_col_types = FALSE,
    ...
  )

  if (format == "wide") {
    # Wide format processing
    sorts <- process_wide_format(raw_data, id_col)
  } else {
    # Long format processing
    sorts <- process_long_format(raw_data, participant_col, statement_col, value_col)
  }

  qsort_data(
    sorts = sorts,
    statements = statements,
    distribution = distribution,
    source = paste0("csv:", basename(file)),
    validate = TRUE
  )
}

#' Read Q-Sort Data from Excel File
#'
#' @description
#' Import Q-sort data from an Excel file (.xlsx or .xls).
#'
#' @param file Path to the Excel file
#' @param sheet Sheet name or index to read (default: first sheet)
#' @param format Character; data format - "wide" (default) or "long"
#' @param participant_col For long format, column name containing participant IDs
#' @param statement_col For long format, column name containing statement IDs
#' @param value_col For long format, column name containing sort values
#' @param statements Character vector of statement texts (optional)
#' @param distribution Numeric vector defining forced distribution (optional)
#' @param skip Number of rows to skip at the beginning
#' @param id_col For wide format, column name/index containing participant IDs
#' @param ... Additional arguments passed to readxl::read_excel
#'
#' @return A QsortData object
#' @export
#'
#' @examples
#' \dontrun{
#' # Read from first sheet
#' qdata <- read_qsort_excel("my_qsorts.xlsx")
#'
#' # Read from specific sheet
#' qdata <- read_qsort_excel("my_qsorts.xlsx", sheet = "Q-Sorts")
#' }
read_qsort_excel <- function(file,
                             sheet = 1,
                             format = c("wide", "long"),
                             participant_col = "participant",
                             statement_col = "statement",
                             value_col = "value",
                             statements = NULL,
                             distribution = NULL,
                             skip = 0,
                             id_col = NULL,
                             ...) {

  format <- match.arg(format)

  cli::cli_alert_info("Reading Q-sort data from: {.file {basename(file)}}")

  # Check for Ken-Q multi-sheet Excel format before generic parsing
  sheets <- readxl::excel_sheets(file)
  sheets_lower <- tolower(sheets)
  has_sorts_sheet <- any(sheets_lower %in% c("sorts", "qsorts", "q-sorts", "q sorts"))

  if (has_sorts_sheet && length(sheets) >= 2) {
    cli::cli_alert_info(
      "Detected multi-sheet workbook ({length(sheets)} sheets) \u2014 trying Ken-Q Excel format"
    )
    return(read_kenq_excel(file))
  }

  # A sheet named like "Statements" can carry the statement texts for any
  # layout on the data sheet
  sheet_stmts <- if (length(sheets) > 1) {
    read_excel_statements_sheet(file, sheets)
  } else {
    NULL
  }
  fill_statements <- function(statements, n_stmt) {
    if (is.null(statements) && !is.null(sheet_stmts) &&
        length(sheet_stmts) == n_stmt) {
      sheet_stmts
    } else {
      statements
    }
  }

  # Read the file
  raw_data <- readxl::read_excel(
    file,
    sheet = sheet,
    skip = skip,
    ...
  )

  # Auto-detect transposed layout: columns named qsort1, qsort2, ... with
  # rows as statements (common in Q-methodology Excel exports).
  col_names <- tolower(names(raw_data))
  qsort_cols <- grep("^qsort\\d+$", col_names)
  has_statement_col <- any(col_names %in% c("statement", "statements"))

  if (length(qsort_cols) >= 2 && has_statement_col) {
    # Transposed layout: rows = statements, qsort columns = participants
    cli::cli_alert_info("Detected transposed layout (statements as rows, participants as columns)")
    n_part <- length(qsort_cols)
    sorts <- t(as.matrix(raw_data[, qsort_cols]))

    stmt_col <- col_names[col_names %in% c("statement", "statements")][1]
    stmt_idx <- which(col_names == stmt_col)
    if (is.null(statements) && length(stmt_idx) > 0) {
      statements <- as.character(raw_data[[stmt_idx]])
    }

    return(qsort_data(
      sorts = sorts,
      statements = fill_statements(statements, ncol(sorts)),
      distribution = distribution,
      source = paste0("excel:", basename(file)),
      validate = TRUE
    ))
  }

  # Transposed layout with a statement id or text first column and
  # participant ids as the remaining headers ("Statement Num." | KAE-001 |
  # KAE-002 | ...)
  first_norm <- if (ncol(raw_data) > 0) {
    gsub("[^a-z]", "", col_names[1])
  } else {
    ""
  }
  stmt_first <- grepl("^statement", first_norm) ||
    first_norm %in% c("stmt", "stmtnum", "item", "itemnum", "itemno")

  if (stmt_first && ncol(raw_data) >= 3) {
    value_cols <- 2:ncol(raw_data)
    numericish <- vapply(value_cols, function(j) {
      x <- raw_data[[j]]
      x_clean <- x[!is.na(x)]
      length(x_clean) > 0 &&
        (is.numeric(x) ||
           all(grepl("^-?[0-9]+\\.?[0-9]*$", as.character(x_clean))))
    }, logical(1))

    if (mean(numericish) >= 0.9 && sum(numericish) >= 2) {
      cli::cli_alert_info("Detected transposed layout (statements as rows, participants as columns)")
      keep <- value_cols[numericish]
      m <- vapply(keep, function(j) {
        suppressWarnings(as.numeric(raw_data[[j]]))
      }, numeric(nrow(raw_data)))
      sorts <- t(m)
      rownames(sorts) <- names(raw_data)[keep]

      # A text first column carries the statements; a numeric one is just
      # the statement index (texts can still come from a Statements sheet)
      first_vals <- raw_data[[1]]
      first_chr <- as.character(first_vals[!is.na(first_vals)])
      looks_text <- length(first_chr) > 0 &&
        !all(grepl("^-?[0-9]+\\.?[0-9]*$", first_chr)) &&
        mean(nchar(first_chr)) > 5
      if (is.null(statements) && looks_text) {
        statements <- as.character(first_vals)
      }

      return(qsort_data(
        sorts = sorts,
        statements = fill_statements(statements, ncol(sorts)),
        distribution = distribution,
        source = paste0("excel:", basename(file)),
        validate = TRUE
      ))
    }
  }

  if (format == "wide") {
    sorts <- process_wide_format(raw_data, id_col)
  } else {
    sorts <- process_long_format(raw_data, participant_col, statement_col, value_col)
  }

  qsort_data(
    sorts = sorts,
    statements = fill_statements(statements, ncol(sorts)),
    distribution = distribution,
    source = paste0("excel:", basename(file)),
    validate = TRUE
  )
}

#' Read Statement Texts from a "Statements" Sheet
#'
#' @description
#' Look for a workbook sheet named like "Statements" and return its
#' statement texts, ordered by a numeric id column when one is present.
#' Returns NULL when no such sheet exists or no usable text is found.
#'
#' @param file Path to the Excel file
#' @param sheets Character vector of the workbook's sheet names
#' @keywords internal
read_excel_statements_sheet <- function(file, sheets) {
  norm <- gsub("[^a-z]", "", tolower(sheets))
  hit <- sheets[norm %in% c("statements", "statement")]
  if (length(hit) == 0) return(NULL)

  d <- tryCatch(
    suppressMessages(readxl::read_excel(file, sheet = hit[1])),
    error = function(e) NULL
  )
  if (is.null(d) || nrow(d) == 0 || ncol(d) == 0) return(NULL)
  d <- as.data.frame(d)

  # The text column: the character column with the longest average strings
  is_chr <- vapply(d, function(x) is.character(x) || is.factor(x), logical(1))
  if (!any(is_chr)) return(NULL)
  chr_idx <- which(is_chr)
  txt_col <- chr_idx[which.max(vapply(chr_idx, function(j) {
    mean(nchar(as.character(d[[j]])), na.rm = TRUE)
  }, numeric(1)))]
  txt <- as.character(d[[txt_col]])

  # Order by a clean numeric id column when the sheet has one
  for (j in setdiff(seq_along(d), txt_col)) {
    v <- suppressWarnings(as.numeric(d[[j]]))
    if (!anyNA(v) && anyDuplicated(v) == 0) {
      txt <- txt[order(v)]
      break
    }
  }

  txt <- txt[!is.na(txt) & nzchar(trimws(txt))]
  if (length(txt) == 0) NULL else txt
}

#' Process Wide Format Data
#' @keywords internal
process_wide_format <- function(data, id_col = NULL) {

  data <- as.data.frame(data)

  if (!is.null(id_col)) {
    if (is.character(id_col)) {
      id_col <- which(names(data) == id_col)
    }
    participant_ids <- as.character(data[[id_col]])
    data <- data[, -id_col, drop = FALSE]
    rownames(data) <- participant_ids
  }

  # Identify numeric columns (Q-sort values)
  numeric_cols <- sapply(data, function(x) {
    x_clean <- na.omit(x)
    # Only consider columns with actual values (not empty)
    if (length(x_clean) == 0) return(FALSE)
    is.numeric(x) || all(grepl("^-?[0-9]+\\.?[0-9]*$", x_clean))
  })

  if (sum(numeric_cols) == 0) {
    rlang::abort("No numeric columns found in data. Check file format.")
  }

  sorts <- data[, numeric_cols, drop = FALSE]

  # Convert to numeric with NA tracking
  na_before <- sum(is.na(sorts))
  sorts <- apply(sorts, 2, as.numeric)
  na_after <- sum(is.na(sorts))
  if (na_after > na_before) {
    cli::cli_alert_warning(
      "{na_after - na_before} value(s) could not be converted to numeric and became NA"
    )
  }

  if (!is.matrix(sorts)) {
    sorts <- as.matrix(sorts)
  }

  return(sorts)
}

#' Process Long Format Data
#' @keywords internal
process_long_format <- function(data, participant_col, statement_col, value_col) {

  required_cols <- c(participant_col, statement_col, value_col)
  missing <- setdiff(required_cols, names(data))

  if (length(missing) > 0) {
    rlang::abort(glue::glue(
      "Missing required columns: {paste(missing, collapse = ', ')}"
    ))
  }

  # Pivot to wide format
  wide_data <- data[, c(participant_col, statement_col, value_col)]
  names(wide_data) <- c("participant", "statement", "value")
  wide_data <- tidyr::pivot_wider(
    wide_data,
    names_from = statement,
    values_from = value
  )

  participants <- wide_data$participant
  sorts <- as.matrix(wide_data[, -1])
  rownames(sorts) <- participants

  return(sorts)
}

#' Read Statements from File
#'
#' @description
#' Read Q-sort statement texts from a file. Supports CSV, Excel, and plain text.
#'
#' @param file Path to the file containing statements
#' @param column For CSV/Excel, column name or index containing statement texts
#' @param id_column For CSV/Excel, column name or index containing statement IDs (optional)
#' @param ... Additional arguments passed to read functions
#'
#' @return A character vector of statement texts (with optional names)
#' @export
#'
#' @examples
#' \dontrun{
#' statements <- read_statements("statements.csv", column = "text")
#' }
read_statements <- function(file, column = 1, id_column = NULL, ...) {

  ext <- tolower(tools::file_ext(file))

  if (ext %in% c("csv", "tsv")) {
    data <- readr::read_csv(file, show_col_types = FALSE, ...)
  } else if (ext %in% c("xlsx", "xls")) {
    data <- readxl::read_excel(file, ...)
  } else if (ext == "txt") {
    # Plain text: one statement per line
    statements <- readr::read_lines(file)
    statements <- stringr::str_trim(statements)
    statements <- statements[nchar(statements) > 0]
    names(statements) <- paste0("S", seq_along(statements))
    return(statements)
  } else {
    rlang::abort("Unsupported file format. Use .csv, .xlsx, .xls, or .txt")
  }

  if (is.character(column)) {
    statements <- data[[column]]
  } else {
    statements <- data[[column]]
  }

  if (!is.null(id_column)) {
    if (is.character(id_column)) {
      ids <- data[[id_column]]
    } else {
      ids <- data[[id_column]]
    }
    names(statements) <- ids
  } else {
    names(statements) <- paste0("S", seq_along(statements))
  }

  return(statements)
}

