#' @title Specialized Data Import Functions
#' @description Functions to import Q-sort data from PQMethod, HTMLQ, FlashQ, and Ken-Q
#' @name import-specialized
NULL

# PQMethod Import

#' Read Q-Sort Data from PQMethod
#'
#' @description
#' Import Q-sort data from PQMethod .DAT files. PQMethod is a widely used
#' DOS-based program for Q methodology analysis.
#'
#' @param file Path to the PQMethod .DAT file
#' @param statements_file Optional path to separate statements file (.STA)
#'
#' @return A QsortData object
#' @export
#'
#' @references
#' Schmolck, P. (2014). PQMethod (Version 2.35).
#' \url{http://schmolck.org/qmethod/}
#'
#' @examples
#' \dontrun{
#' qdata <- read_pqmethod("lipset.dat")
#' qdata <- read_pqmethod("study.dat", statements_file = "study.sta")
#' }
read_pqmethod <- function(file, statements_file = NULL) {

  cli::cli_alert_info("Reading PQMethod data from: {.file {basename(file)}}")

  # Read raw file content
  lines <- readr::read_lines(file)

  # PQMethod .DAT files have two known header formats:
  #
  # Format A (older/simple):
  #   Line 1: Title text
  #   Line 2: n_statements  min_val  max_val
  #
  # Format B (extended):
  #   Line 1: n_respondents  n_statements  title_text
  #   Line 2: min_val  max_val  distribution_counts...

  # Detect format by checking if line 1 starts with digits
  line1 <- lines[1]
  line1_nums <- as.numeric(stringr::str_extract_all(
    stringr::str_trim(line1), "-?\\d+"
  )[[1]])
  line2_nums <- as.numeric(stringr::str_extract_all(lines[2], "-?\\d+")[[1]])

  if (length(line2_nums) < 3) {
    rlang::abort("Invalid PQMethod file format: could not parse header")
  }

  # Format B: line 1 starts with numbers AND line 2 has many numbers (distribution)
  # Format A: line 2 has exactly 3 numbers (n_statements, min, max)
  is_format_b <- grepl("^\\s*\\d+\\s+\\d+\\s+", line1) && length(line2_nums) > 3

  if (is_format_b) {
    # Format B: line 2 = min_val, max_val, distribution_counts...
    # The distribution counts sum to n_statements (most reliable method)
    title <- stringr::str_trim(sub("^\\s*[\\d\\s]+", "", line1))
    min_val <- line2_nums[1]
    max_val <- line2_nums[2]
    dist_counts <- line2_nums[-(1:2)]
    n_statements <- as.integer(sum(dist_counts))
    cli::cli_alert_info("PQMethod extended format: {n_statements} statements, range [{min_val}, {max_val}]")
  } else {
    # Format A: line 1 = title, line 2 = n_statements, min, max
    title <- stringr::str_trim(line1)
    n_statements <- line2_nums[1]
    min_val <- line2_nums[2]
    max_val <- line2_nums[3]
    cli::cli_alert_info("Found {n_statements} statements, range [{min_val}, {max_val}]")
  }

  if (n_statements <= 0 || n_statements > 200) {
    rlang::abort(c(
      "Invalid PQMethod header",
      "x" = paste0("Parsed n_statements = ", n_statements),
      "i" = "Check that the .DAT file is a valid PQMethod data file"
    ))
  }

  # Parse Q-sorts from data lines (lines 3+)
  data_lines <- lines[-(1:2)]
  data_lines <- data_lines[nchar(stringr::str_trim(data_lines)) > 0]

  # Determine ID field width from first data line
  # PQMethod format: ID field (variable width) + 2-char fields per sort value
  # ID width = total line length - 2 * n_statements
  id_width <- 10L  # default
  if (length(data_lines) > 0) {
    first_len <- nchar(data_lines[1])
    computed_id <- first_len - 2L * n_statements
    if (computed_id >= 4 && computed_id <= 20) {
      id_width <- computed_id
    }
  }

  sorts_list <- list()
  participant_ids <- character()

  for (line in data_lines) {
    # PQMethod uses fixed-width format:
    # - First id_width characters: participant ID (left-aligned, space-padded)
    # - Remaining: 2-character fields per sort value (right-justified)
    # e.g., "res-1     -3 5 5-1 0-2 0 4-1 2..."
    #
    # This fixed-width format means negative values can appear without a
    # separator: "5-1" = [5, -1] in two 2-char fields " 5" + "-1"

    if (nchar(line) >= id_width + n_statements * 2) {
      # Fixed-width parsing
      pid <- stringr::str_trim(substr(line, 1, id_width))
      value_str <- substr(line, id_width + 1, id_width + n_statements * 2)
      # Split into 2-character chunks
      numeric_values <- vapply(seq_len(n_statements), function(i) {
        chunk <- substr(value_str, (i - 1) * 2 + 1, i * 2)
        suppressWarnings(as.numeric(stringr::str_trim(chunk)))
      }, numeric(1))
    } else {
      # Fallback: space-delimited parsing for shorter lines
      parts <- stringr::str_split(stringr::str_trim(line), "\\s+")[[1]]
      if (length(parts) <= 1) next

      if (grepl("^[A-Za-z]", parts[1]) || nchar(parts[1]) <= 3) {
        pid <- parts[1]
        values <- parts[-1]
      } else {
        pid <- paste0("P", length(sorts_list) + 1)
        values <- parts
      }
      numeric_values <- suppressWarnings(as.numeric(values))
    }

    if (length(numeric_values) >= n_statements) {
      numeric_values <- numeric_values[1:n_statements]
      sorts_list[[length(sorts_list) + 1]] <- numeric_values
      participant_ids <- c(participant_ids, pid)
    }
  }

  if (length(sorts_list) == 0) {
    rlang::abort("Could not parse any Q-sorts from file")
  }

  sorts <- do.call(rbind, sorts_list)
  rownames(sorts) <- participant_ids
  colnames(sorts) <- paste0("S", 1:n_statements)

  # Read statements if file provided
  statements <- NULL
  if (!is.null(statements_file) && file.exists(statements_file)) {
    statements <- read_pqmethod_statements(statements_file)
  }

  # Infer distribution
  distribution <- as.numeric(table(factor(sorts[1, ], levels = min_val:max_val)))

  cli::cli_alert_success("Successfully imported {nrow(sorts)} Q-sorts")

  qsort_data(
    sorts = sorts,
    statements = statements,
    distribution = distribution,
    metadata = list(title = title, pqmethod_range = c(min_val, max_val)),
    source = paste0("pqmethod:", basename(file)),
    validate = TRUE
  )
}

#' Read PQMethod Statements File
#' @keywords internal
read_pqmethod_statements <- function(file) {
  lines <- readr::read_lines(file)
  lines <- lines[nchar(stringr::str_trim(lines)) > 0]

  # Try to parse numbered statements
  statements <- character()
  for (line in lines) {
    # Remove leading number and punctuation
    text <- stringr::str_replace(line, "^\\s*\\d+[.):;\\s]+", "")
    text <- stringr::str_trim(text)
    if (nchar(text) > 0) {
      statements <- c(statements, text)
    }
  }

  return(statements)
}

# HTMLQ Import

#' Read Q-Sort Data from HTMLQ/FlashQ
#'
#' @description
#' Import Q-sort data from HTMLQ or FlashQ format. These tools export
#' Q-sort data in CSV format with specific column structures.
#'
#' @param file Path to the CSV file exported from HTMLQ or FlashQ
#' @param format Character; format variant - "htmlq" (default), "flashq", or "auto"
#' @param statements_file Optional path to separate statements file
#'
#' @return A QsortData object
#' @export
#'
#' @references
#' Hackert, C., & Braehler, G. HTMLQ.
#' \url{https://github.com/aproxima/htmlq}
#'
#' @examples
#' \dontrun{
#' qdata <- read_htmlq("htmlq_export.csv")
#' qdata <- read_htmlq("flashq_data.csv", format = "flashq")
#' }
read_htmlq <- function(file, format = c("auto", "htmlq", "flashq"), statements_file = NULL) {

  format <- match.arg(format)

  cli::cli_alert_info("Reading HTMLQ/FlashQ data from: {.file {basename(file)}}")

  # Read raw data
  raw_data <- readr::read_csv(file, show_col_types = FALSE)

  # Auto-detect format
  if (format == "auto") {
    format <- detect_htmlq_format(raw_data)
    cli::cli_alert_info("Detected format: {format}")
  }

  if (format == "htmlq") {
    result <- parse_htmlq_data(raw_data)
  } else {
    result <- parse_flashq_data(raw_data)
  }

  if (!is.null(statements_file) && file.exists(statements_file)) {
    result$statements <- read_statements(statements_file)
  }

  qsort_data(
    sorts = result$sorts,
    statements = result$statements,
    distribution = result$distribution,
    metadata = result$metadata,
    source = paste0(format, ":", basename(file)),
    validate = TRUE
  )
}

#' Detect HTMLQ Format Variant
#' @keywords internal
detect_htmlq_format <- function(data) {
  col_names <- tolower(names(data))

  # HTMLQ typically has columns like "uid", "datetime", and statement columns
  if (any(grepl("^uid$", col_names)) && any(grepl("datetime|timestamp", col_names))) {
    return("htmlq")
  }

  # FlashQ often has "id" and "time" columns
  if (any(grepl("^id$", col_names)) && any(grepl("^time$", col_names))) {
    return("flashq")
  }

  # Default to htmlq
  return("htmlq")
}

#' Parse HTMLQ Data
#' @keywords internal
parse_htmlq_data <- function(data) {

  # HTMLQ format:
  # Column 1: uid (participant ID)
  # Column 2: datetime
  # Columns 3-n: statement sorts (s1, s2, s3, ...)
  # Optional: additional metadata columns at end

  col_names <- names(data)

  # Find statement columns (typically s1, s2, s3... or statement_1, etc.)
  stmt_cols <- grep("^s\\d+$|^statement[_-]?\\d+$", col_names, ignore.case = TRUE)

  if (length(stmt_cols) == 0) {
    # Try to identify numeric columns that look like Q-sort values
    numeric_cols <- which(sapply(data, is.numeric))
    # Exclude columns that are likely IDs or timestamps
    stmt_cols <- numeric_cols[!col_names[numeric_cols] %in% c("uid", "id", "record_id")]
  }

  if (length(stmt_cols) == 0) {
    rlang::abort("Could not identify statement columns in HTMLQ data")
  }

  sorts <- as.matrix(data[, stmt_cols])

  id_col <- grep("^uid$|^id$|^participant", names(data), ignore.case = TRUE)
  if (length(id_col) > 0) {
    participant_ids <- as.character(data[[id_col[1]]])
  } else {
    participant_ids <- paste0("P", seq_len(nrow(data)))
  }
  rownames(sorts) <- participant_ids

  non_stmt_cols <- setdiff(seq_along(col_names), stmt_cols)
  metadata_cols <- non_stmt_cols[!col_names[non_stmt_cols] %in% c("uid", "id")]
  metadata <- if (length(metadata_cols) > 0) as.list(data[, metadata_cols, drop = FALSE]) else list()

  # Infer distribution
  distribution <- infer_distribution(sorts)

  list(
    sorts = sorts,
    statements = colnames(sorts),
    distribution = distribution,
    metadata = metadata
  )
}

#' Parse FlashQ Data
#' @keywords internal
parse_flashq_data <- function(data) {
  # FlashQ is similar to HTMLQ with minor differences
  # Reuse HTMLQ parser with FlashQ-specific adjustments

  result <- parse_htmlq_data(data)

  # FlashQ-specific adjustments if needed
  # (column naming conventions, etc.)

  return(result)
}

# Easy-HTMLQ Import

# Ken-Q Import

#' Read Q-Sort Data from Ken-Q Analysis
#'
#' @description
#' Import Q-sort data from Ken-Q Analysis export files. Ken-Q exports
#' data in JSON or CSV format.
#'
#' @param file Path to Ken-Q export file (JSON or CSV)
#' @param format Character; export format - "auto", "json", or "csv"
#'
#' @return A QsortData object
#' @export
#'
#' @references
#' Banasick, S. Ken-Q Analysis.
#' \url{https://shawnbanasick.github.io/ken-q-analysis/}
#'
#' @examples
#' \dontrun{
#' qdata <- read_kenq("kenq_export.json")
#' qdata <- read_kenq("kenq_export.csv")
#' }
read_kenq <- function(file, format = c("auto", "json", "csv")) {

  format <- match.arg(format)

  cli::cli_alert_info("Reading Ken-Q data from: {.file {basename(file)}}")

  # Auto-detect format
  if (format == "auto") {
    ext <- tolower(tools::file_ext(file))
    format <- if (ext == "json") "json" else "csv"
  }

  if (format == "json") {
    result <- parse_kenq_json(file)
  } else {
    result <- parse_kenq_csv(file)
  }

  qsort_data(
    sorts = result$sorts,
    statements = result$statements,
    distribution = result$distribution,
    metadata = result$metadata,
    source = paste0("kenq:", basename(file)),
    validate = TRUE
  )
}

#' Parse Ken-Q JSON Export
#' @keywords internal
parse_kenq_json <- function(file) {

  data <- jsonlite::fromJSON(file)

  # Ken-Q JSON structure typically includes:
  # - qSorts: array of Q-sort arrays
  # - participantNames: array of participant IDs
  # - statementText: array of statement texts
  # - sortPattern: array defining distribution

  if ("qSorts" %in% names(data)) {
    # fromJSON may return a matrix directly or a list of vectors
    if (is.matrix(data$qSorts)) {
      sorts <- data$qSorts
    } else {
      sorts <- do.call(rbind, data$qSorts)
    }
  } else if ("respondentData" %in% names(data)) {
    # fromJSON returns a data.frame with a list-column "sort"
    rd <- data$respondentData
    if (is.data.frame(rd)) {
      sorts <- do.call(rbind, rd$sort)
    } else {
      sorts <- do.call(rbind, lapply(rd, function(x) x$sort))
    }
  } else {
    # Try Easy-HTMLQ Firebase JSON format (pipe-delimited sort fields)
    result <- try_parse_firebase_json(data)
    if (!is.null(result)) return(result)

    rlang::abort("Unrecognized Ken-Q/Easy-HTMLQ JSON structure")
  }

  if ("participantNames" %in% names(data)) {
    rownames(sorts) <- data$participantNames
  } else if ("respondentData" %in% names(data)) {
    rd <- data$respondentData
    if (is.data.frame(rd)) {
      rownames(sorts) <- rd$name %||% rd$id %||% paste0("P", seq_len(nrow(sorts)))
    } else {
      rownames(sorts) <- sapply(rd, function(x) x$name %||% x$id)
    }
  }

  statements <- data$statementText %||% data$statements %||% paste0("S", 1:ncol(sorts))
  colnames(sorts) <- if (length(statements) == ncol(sorts)) statements else paste0("S", 1:ncol(sorts))

  distribution <- data$sortPattern %||% data$distribution %||% infer_distribution(sorts)

  list(
    sorts = sorts,
    statements = statements,
    distribution = as.numeric(distribution),
    metadata = list(kenq_version = data$version %||% "unknown")
  )
}

#' Try Parsing as Easy-HTMLQ Firebase JSON
#' @keywords internal
try_parse_firebase_json <- function(data) {
  # Firebase JSON: top-level keys are push IDs, each has a "sort" field
  if (is.data.frame(data)) return(NULL)
  if (!is.list(data)) return(NULL)

  has_sort <- vapply(data, function(x) {
    is.list(x) && "sort" %in% names(x)
  }, logical(1))

  if (sum(has_sort) == 0) return(NULL)

  valid_entries <- data[has_sort]
  sorts_list <- list()
  participant_names <- character()

  for (i in seq_along(valid_entries)) {
    entry <- valid_entries[[i]]
    sort_str <- gsub("\\+", "", as.character(entry$sort))
    values <- suppressWarnings(as.numeric(strsplit(sort_str, "\\|")[[1]]))
    if (length(values) == 0 || all(is.na(values))) next

    sorts_list[[length(sorts_list) + 1]] <- values
    key <- names(valid_entries)[i]
    name <- entry$name %||% entry$email %||%
      substr(key, max(1, nchar(key) - 9), nchar(key))
    participant_names <- c(participant_names, as.character(name))
  }

  if (length(sorts_list) == 0) return(NULL)

  sorts <- do.call(rbind, sorts_list)
  rownames(sorts) <- participant_names

  cli::cli_alert_info("Detected Easy-HTMLQ Firebase JSON format")

  list(
    sorts = sorts,
    statements = paste0("S", seq_len(ncol(sorts))),
    distribution = as.numeric(infer_distribution(sorts)),
    metadata = list(format = "easyhtml_firebase")
  )
}

#' Parse Ken-Q CSV Export
#' @keywords internal
parse_kenq_csv <- function(file) {
  data <- readr::read_csv(file, show_col_types = FALSE)
  result <- parse_htmlq_data(data)  # Ken-Q CSV is similar to HTMLQ
  return(result)
}

# Q-Sortware Import

# Ken-Q Analysis Multi-Sheet Excel Import (Type 1 & Type 2) ----

#' Read Q-Sort Data from Ken-Q Analysis Excel Format
#'
#' @description
#' Import Q-sort data from Ken-Q Analysis multi-sheet Excel files.
#' Supports both Type 1 (statement-number spatial layout) and
#' Type 2 (direct-score participant rows), including old and new
#' (Version 2) sub-formats.
#'
#' Ken-Q Excel files have multiple sheets:
#' \itemize{
#'   \item \strong{sorts} (required): Q-sort data
#'   \item \strong{statements}: Statement texts (column header "Statements")
#'   \item \strong{pattern}: 20-element multiplier array for forced distribution
#'   \item \strong{name}: Project name
#'   \item \strong{version}: Format version identifier
#' }
#'
#' @param file Path to the Ken-Q Analysis Excel file
#'
#' @return A QsortData object
#' @export
#'
#' @references
#' Banasick, S. Ken-Q Analysis.
#' \url{https://shawnbanasick.github.io/ken-q-analysis/}
#'
#' @examples
#' \dontrun{
#' qdata <- read_kenq_excel("kenq_study.xlsx")
#' }
read_kenq_excel <- function(file) {

  sheets <- readxl::excel_sheets(file)
  sheets_lower <- tolower(sheets)

  cli::cli_alert_info("Reading Ken-Q Analysis Excel file: {.file {basename(file)}}")
  cli::cli_alert_info("Sheets found: {paste(sheets, collapse = ', ')}")

  # --- Helper: find a sheet by name, with "Example - " fallback ---
  # Ken-Q templates have empty primary sheets + populated "Example - " sheets
  find_sheet <- function(targets) {
    # First try exact match
    idx <- which(sheets_lower %in% targets)[1]
    primary_has_data <- FALSE
    if (!is.na(idx)) {
      test <- readxl::read_excel(file, sheet = sheets[idx], col_names = FALSE)
      # Check for meaningful data: more than 1 row AND not all NA/empty
      if (nrow(test) > 1) {
        vals <- unlist(test[2:min(nrow(test), 5), ])
        real_vals <- vals[!is.na(vals) & nchar(trimws(as.character(vals))) > 0 &
                          !grepl("^\\s*\\.?\\s*$", as.character(vals))]
        primary_has_data <- length(real_vals) > 0
      }
    }
    if (primary_has_data) return(list(idx = idx, name = sheets[idx]))

    # Fall back to "example - " prefixed versions
    example_targets <- paste0("example - ", targets)
    idx2 <- which(sheets_lower %in% example_targets)[1]
    if (!is.na(idx2)) return(list(idx = idx2, name = sheets[idx2]))
    # Return primary even if sparse (caller handles empty)
    if (!is.na(idx)) return(list(idx = idx, name = sheets[idx]))
    return(NULL)
  }

  # --- Find required sorts sheet ---
  sorts_info <- find_sheet(c("sorts", "qsorts", "q-sorts", "q sorts"))
  if (is.null(sorts_info)) {
    rlang::abort(c(
      "Not a valid Ken-Q Excel file",
      "x" = "Missing required 'sorts' sheet",
      "i" = paste("Found sheets:", paste(sheets, collapse = ", "))
    ))
  }

  # --- Read statements ---
  stmts_info <- find_sheet(c("statements", "statement"))
  statements <- NULL
  if (!is.null(stmts_info)) {
    statements <- parse_kenq_statements_sheet(file, stmts_info$name)
  }
  # If primary sheet had no usable text, try broader fallbacks
  if (is.null(statements)) {
    # Try "Example - " prefixed sheets
    example_targets <- paste0("example - ", c("statements", "statement"))
    ex_idx <- which(sheets_lower %in% example_targets)[1]
    if (!is.na(ex_idx)) {
      cli::cli_alert_info("Trying '{sheets[ex_idx]}' sheet as fallback")
      statements <- parse_kenq_statements_sheet(file, sheets[ex_idx])
    }
  }
  # --- Read pattern ---
  pattern_info <- find_sheet(c("pattern", "patterns"))
  distribution <- NULL
  if (!is.null(pattern_info)) {
    mult <- parse_kenq_pattern_sheet(file, pattern_info$name)
    if (!is.null(mult)) {
      distribution <- kenq_multiplier_to_distribution(mult)
    }
  }

  # --- Read project name ---
  name_idx <- which(sheets_lower %in% c("name", "names"))[1]
  project_name <- "Ken-Q Project"
  if (!is.na(name_idx)) {
    name_raw <- readxl::read_excel(file, sheet = sheets[name_idx], col_names = FALSE)
    if (nrow(name_raw) >= 2 && !is.na(name_raw[[1]][2])) {
      project_name <- as.character(name_raw[[1]][2])
    } else if (nrow(name_raw) >= 1 && !is.na(name_raw[[1]][1])) {
      project_name <- as.character(name_raw[[1]][1])
    }
  }

  # --- Check for version and type sheets ---
  has_version <- any(sheets_lower == "version")

  # "type" sheet: Ken-Q templates use this to distinguish Type 1 vs Type 2
  type_idx <- which(sheets_lower == "type")[1]
  kenq_type <- NULL
  if (!is.na(type_idx)) {
    type_raw <- readxl::read_excel(file, sheet = sheets[type_idx], col_names = FALSE)
    if (nrow(type_raw) >= 2) {
      kenq_type <- suppressWarnings(as.integer(type_raw[[1]][2]))
    }
  }

  # --- Read sorts sheet raw ---
  sorts_raw <- as.data.frame(
    readxl::read_excel(file, sheet = sorts_info$name, col_names = FALSE),
    stringsAsFactors = FALSE
  )

  if (nrow(sorts_raw) < 2) {
    rlang::abort(c(
      "Ken-Q Excel sorts sheet is empty or has no data rows",
      "i" = paste("Sheet used:", sorts_info$name),
      "i" = "If this is a template file, fill in the 'sorts' sheet with your data"
    ))
  }

  # --- Detect format and parse ---
  n_stmts <- if (!is.null(statements)) length(statements) else NULL
  result <- detect_and_parse_kenq_sorts(sorts_raw, n_stmts, has_version,
                                         kenq_type = kenq_type)

  # Use distribution from pattern sheet first, fall back to inferred
  final_dist <- distribution %||% result$distribution

  cli::cli_alert_success(
    "Imported {nrow(result$sorts)} participants, {ncol(result$sorts)} statements ({result$format_type})"
  )

  qsort_data(
    sorts = result$sorts,
    statements = statements,
    distribution = final_dist,
    metadata = list(project_name = project_name, kenq_format = result$format_type),
    source = paste0("kenq-excel:", basename(file)),
    validate = TRUE
  )
}

#' Detect and Parse Ken-Q Sorts Sheet
#' @param raw Data frame of raw sorts sheet
#' @param n_statements Number of statements (from statements sheet, or NULL)
#' @param has_version Whether a "version" sheet exists
#' @param kenq_type Integer from "type" sheet: 1 or 2 (or NULL if absent)
#' @keywords internal
detect_and_parse_kenq_sorts <- function(raw, n_statements, has_version,
                                         kenq_type = NULL) {

  # --- Strategy 0: Use "type" sheet if available ---
  if (!is.null(kenq_type) && kenq_type == 1L) {
    # Type 1: Row 1 has header text + participant names
    cli::cli_alert_info("Ken-Q 'type' sheet says Type 1")
    return(parse_kenq_type1_ver2(raw, n_statements))
  }
  if (!is.null(kenq_type) && kenq_type == 2L) {
    # Type 2: check for "Sort Pattern" row first (old format), else Ver2
    for (i in 1:min(5, nrow(raw))) {
      for (j in 1:min(5, ncol(raw))) {
        val <- as.character(raw[i, j])
        if (!is.na(val) && grepl("sort\\s*pattern", val, ignore.case = TRUE)) {
          cli::cli_alert_info("Ken-Q 'type' sheet says Type 2 (Old format)")
          return(parse_kenq_type2_old(raw, sort_pattern_row = i))
        }
      }
    }
    cli::cli_alert_info("Ken-Q 'type' sheet says Type 2 Version 2")
    return(parse_kenq_type2_ver2(raw))
  }

  # --- Strategy 1: "Sort Pattern" text -> Type 2 Old ---
  for (i in 1:min(5, nrow(raw))) {
    for (j in 1:min(5, ncol(raw))) {
      val <- as.character(raw[i, j])
      if (!is.na(val) && grepl("sort\\s*pattern", val, ignore.case = TRUE)) {
        cli::cli_alert_info("Detected Ken-Q Excel Type 2 (Old format)")
        return(parse_kenq_type2_old(raw, sort_pattern_row = i))
      }
    }
  }

  # --- Strategy 2: First cell = header text + names in B+ -> Type 1 ---
  first_cell <- trimws(as.character(raw[1, 1]))
  first_cell_blank <- is.na(first_cell) || nchar(first_cell) == 0
  first_cell_header <- !is.na(first_cell) &&
    grepl("respondent|sort\\s*value|name", first_cell, ignore.case = TRUE)

  if ((first_cell_blank || first_cell_header) && ncol(raw) > 1) {
    second_cell <- as.character(raw[1, 2])
    if (!is.na(second_cell) && !grepl("^-?\\d+\\.?\\d*$", trimws(second_cell))) {
      cli::cli_alert_info("Detected Ken-Q Excel Type 1{if (has_version) ' Version 2' else ''}")
      return(parse_kenq_type1_ver2(raw, n_statements))
    }
  }

  # --- Strategy 3: Check for Type 1 Ver1 (22 header rows, then names + data) ---
  if (nrow(raw) > 25) {
    row2_col2 <- as.character(raw[2, 2])
    is_project_name <- !is.na(row2_col2) && !grepl("^-?\\d+$", trimws(row2_col2)) &&
      nchar(trimws(row2_col2)) > 2

    if (is_project_name) {
      sort_val_rows <- TRUE
      for (r in 3:min(22, nrow(raw))) {
        val <- suppressWarnings(as.numeric(raw[r, 1]))
        blank <- is.na(raw[r, 1]) || nchar(trimws(as.character(raw[r, 1]))) == 0
        if (!is.na(val) || blank) next
        sort_val_rows <- FALSE
        break
      }
      if (sort_val_rows) {
        cli::cli_alert_info("Detected Ken-Q Excel Type 1 Version 1 (Old format)")
        return(parse_kenq_type1_ver1(raw, n_statements))
      }
    }
  }

  # --- Strategy 4: Type 2 Ver2 (simple: name + direct scores per row) ---
  first_cell_text <- !is.na(as.character(raw[1, 1])) &&
    !grepl("^-?\\d+\\.?\\d*$", trimws(as.character(raw[1, 1])))
  if (first_cell_text && ncol(raw) > 2) {
    sample_vals <- suppressWarnings(as.numeric(raw[1, 2:ncol(raw)]))
    if (sum(!is.na(sample_vals)) > (ncol(raw) - 1) * 0.5) {
      cli::cli_alert_info("Detected Ken-Q Excel Type 2 Version 2")
      return(parse_kenq_type2_ver2(raw))
    }
  }

  rlang::abort(c(
    "Could not determine Ken-Q Excel format",
    "i" = "Expected Type 1 (spatial layout) or Type 2 (direct scores)"
  ))
}

#' Parse Ken-Q Type 1 Ver2: Row 1 = header/blank + names, Rows 2+ = sort_val + stmt numbers
#' @keywords internal
parse_kenq_type1_ver2 <- function(raw, n_statements) {

  # Row 1: blank or header text in col A, participant names in cols B+
  # Check NA on raw values BEFORE as.character (which turns NA into "NA" string)
  raw_na_mask <- vapply(raw[1, ], function(x) is.na(x), logical(1))
  names_row <- as.character(raw[1, ])
  participant_names <- names_row[-1]
  na_flags <- raw_na_mask[-1]
  participant_names <- participant_names[!na_flags &
    nchar(trimws(participant_names)) > 0]
  # Filter out any that look like numbers (not names)
  participant_names <- participant_names[!grepl("^-?\\d+\\.?\\d*$", trimws(participant_names))]
  n_participants <- length(participant_names)

  # Rows 2+: col A = sort value, cols B+ = statement numbers
  data_rows <- raw[-1, , drop = FALSE]

  sort_values <- suppressWarnings(as.numeric(data_rows[[1]]))
  valid_rows <- !is.na(sort_values)
  sort_values <- sort_values[valid_rows]
  stmt_data <- data_rows[valid_rows, 2:(n_participants + 1), drop = FALSE]

  # Determine n_statements from data if not given
  if (is.null(n_statements)) {
    all_nums <- suppressWarnings(as.integer(round(as.numeric(unlist(stmt_data)))))
    n_statements <- max(all_nums, na.rm = TRUE)
  }

  # Convert: statement-number layout -> canonical sorts matrix
  sorts <- kenq_type1_to_sorts_matrix(sort_values, stmt_data,
                                       n_participants, n_statements,
                                       participant_names)

  # Infer distribution from sort values
  dist <- as.numeric(table(factor(sort_values,
    levels = min(sort_values):max(sort_values))))

  list(sorts = sorts, distribution = dist, format_type = "type1_ver2")
}

#' Parse Ken-Q Type 1 Ver1: Rows 1-22 = header/pattern, Row 23+ = names + data
#' @keywords internal
parse_kenq_type1_ver1 <- function(raw, n_statements) {

  # Find the participant names row (first row after pattern block with text in B+)
  names_row_idx <- NULL
  for (i in 20:min(28, nrow(raw))) {
    val2 <- as.character(raw[i, 2])
    if (!is.na(val2) && nchar(trimws(val2)) > 0 &&
        !grepl("^-?\\d+\\.?\\d*$", trimws(val2))) {
      names_row_idx <- i
      break
    }
  }

  if (is.null(names_row_idx)) {
    rlang::abort("Ken-Q Type 1 Ver1: could not locate participant names row")
  }

  names_row <- as.character(raw[names_row_idx, ])
  participant_names <- names_row[-1]
  participant_names <- participant_names[!is.na(participant_names) &
    nchar(trimws(participant_names)) > 0]
  n_participants <- length(participant_names)

  # Data rows start after the names row
  data_start <- names_row_idx + 1
  data_rows <- raw[data_start:nrow(raw), , drop = FALSE]

  valid <- apply(data_rows, 1, function(row) {
    !all(is.na(row) | nchar(trimws(as.character(row))) == 0)
  })
  data_rows <- data_rows[valid, , drop = FALSE]

  # Parse sort values and statement data
  sort_values <- suppressWarnings(as.numeric(data_rows[[1]]))
  valid_rows <- !is.na(sort_values)
  sort_values <- sort_values[valid_rows]
  stmt_data <- data_rows[valid_rows, 2:(n_participants + 1), drop = FALSE]

  if (is.null(n_statements)) {
    all_nums <- suppressWarnings(as.integer(round(as.numeric(unlist(stmt_data)))))
    n_statements <- max(all_nums, na.rm = TRUE)
  }

  sorts <- kenq_type1_to_sorts_matrix(sort_values, stmt_data,
                                       n_participants, n_statements,
                                       participant_names)

  dist <- as.numeric(table(factor(sort_values,
    levels = min(sort_values):max(sort_values))))

  list(sorts = sorts, distribution = dist, format_type = "type1_ver1")
}

#' Parse Ken-Q Type 2 Old: has "Sort Pattern" row, project name, then data
#' @keywords internal
parse_kenq_type2_old <- function(raw, sort_pattern_row) {

  # Parse the expanded sort pattern (each value listed individually)
  pattern_cells <- as.character(raw[sort_pattern_row, ])
  pattern_vals <- suppressWarnings(as.numeric(pattern_cells))
  pattern_vals <- pattern_vals[!is.na(pattern_vals)]

  # Infer distribution from expanded pattern
  distribution <- NULL
  if (length(pattern_vals) > 0) {
    distribution <- as.numeric(table(factor(pattern_vals,
      levels = min(pattern_vals):max(pattern_vals))))
  }

  # Find data start: first row after sort_pattern that has a participant name
  data_start <- sort_pattern_row + 1
  while (data_start <= nrow(raw)) {
    first_cell <- trimws(as.character(raw[data_start, 1]))
    if (!is.na(first_cell) && nchar(first_cell) > 0 &&
        !grepl("^-?\\d+\\.?\\d*$", first_cell) &&
        !grepl("sort\\s*pattern", first_cell, ignore.case = TRUE)) {
      break
    }
    data_start <- data_start + 1
  }

  if (data_start > nrow(raw)) {
    rlang::abort("Ken-Q Type 2: could not find participant data after Sort Pattern row")
  }

  data_rows <- raw[data_start:nrow(raw), , drop = FALSE]

  valid <- apply(data_rows, 1, function(row) {
    first <- trimws(as.character(row[1]))
    !is.na(first) && nchar(first) > 0
  })
  data_rows <- data_rows[valid, , drop = FALSE]

  participant_names <- as.character(data_rows[[1]])
  sorts_data <- data_rows[, -1, drop = FALSE]
  sorts <- apply(sorts_data, 2, as.numeric)
  if (!is.matrix(sorts)) sorts <- as.matrix(sorts)

  all_na_cols <- apply(sorts, 2, function(col) all(is.na(col)))
  sorts <- sorts[, !all_na_cols, drop = FALSE]

  rownames(sorts) <- participant_names

  list(sorts = sorts, distribution = distribution, format_type = "type2_old")
}

#' Parse Ken-Q Type 2 Ver2: simple rows of name + direct scores
#' @keywords internal
parse_kenq_type2_ver2 <- function(raw) {

  # Filter out blank rows
  valid <- apply(raw, 1, function(row) {
    first <- trimws(as.character(row[1]))
    !is.na(first) && nchar(first) > 0
  })
  raw <- raw[valid, , drop = FALSE]

  participant_names <- as.character(raw[[1]])
  sorts_data <- raw[, -1, drop = FALSE]
  sorts <- apply(sorts_data, 2, as.numeric)
  if (!is.matrix(sorts)) sorts <- as.matrix(sorts)

  all_na_cols <- apply(sorts, 2, function(col) all(is.na(col)))
  sorts <- sorts[, !all_na_cols, drop = FALSE]

  rownames(sorts) <- participant_names

  list(sorts = sorts, distribution = NULL, format_type = "type2_ver2")
}

#' Convert Ken-Q Type 1 Statement-Number Layout to Canonical Sorts Matrix
#'
#' @description
#' In Type 1, each data row has: column A = sort value, columns B+ = statement
#' numbers placed at that sort value by each participant. This function inverts
#' that: for each participant, sorts_matrix[p, statement_num] = sort_value.
#'
#' @keywords internal
kenq_type1_to_sorts_matrix <- function(sort_values, stmt_data,
                                        n_participants, n_statements,
                                        participant_names) {

  sorts <- matrix(NA_real_, nrow = n_participants, ncol = n_statements)
  rownames(sorts) <- participant_names
  colnames(sorts) <- paste0("S", seq_len(n_statements))

  for (p in seq_len(n_participants)) {
    # as.numeric first to handle "16.0" strings, then round to integer
    stmt_nums <- suppressWarnings(as.integer(round(as.numeric(stmt_data[, p]))))
    for (row_idx in seq_along(sort_values)) {
      sn <- stmt_nums[row_idx]
      sv <- sort_values[row_idx]
      if (!is.na(sn) && sn >= 1 && sn <= n_statements && !is.na(sv)) {
        sorts[p, sn] <- sv
      }
    }
  }

  sorts
}

# ---- Ken-Q Helper Functions ----

#' Parse Ken-Q Statements Sheet
#' @keywords internal
parse_kenq_statements_sheet <- function(file, sheet_name) {
  stmts_raw <- readxl::read_excel(file, sheet = sheet_name)

  # Ken-Q expects column header "Statements"
  stmt_col <- which(tolower(names(stmts_raw)) == "statements")[1]
  if (is.na(stmt_col)) stmt_col <- 1

  statements <- as.character(stmts_raw[[stmt_col]])
  statements <- statements[!is.na(statements) & nchar(trimws(statements)) > 0]

  # Filter out placeholder entries (just periods or whitespace)
  statements <- statements[!grepl("^\\s*\\.?\\s*$", statements)]

  if (length(statements) == 0) {
    cli::cli_alert_warning("Statements sheet found but no texts extracted")
    return(NULL)
  }

  cli::cli_alert_info("Extracted {length(statements)} statements")
  return(statements)
}

#' Parse Ken-Q Pattern Sheet (20-element multiplier array)
#' @keywords internal
parse_kenq_pattern_sheet <- function(file, sheet_name) {
  pattern_raw <- readxl::read_excel(file, sheet = sheet_name, col_names = FALSE)

  # Data row contains the multiplier array (try row 2 first, then 1, then 3)
  for (row_idx in c(2, 1, 3)) {
    if (row_idx > nrow(pattern_raw)) next
    row_text <- paste(as.character(pattern_raw[row_idx, ]), collapse = ",")
    vals <- suppressWarnings(as.numeric(
      strsplit(gsub("[^0-9,.-]", "", row_text), ",")[[1]]
    ))
    vals <- vals[!is.na(vals)]
    if (length(vals) >= 5) {
      # Pad to 20 if shorter
      if (length(vals) < 20) vals <- c(vals, rep(0, 20 - length(vals)))
      cli::cli_alert_info(
        "Sort pattern multiplier: {paste(vals[vals > 0], collapse = ', ')}"
      )
      return(vals[1:20])
    }
  }

  cli::cli_alert_warning("Could not extract pattern from pattern sheet")
  return(NULL)
}

#' Convert Ken-Q 20-Element Multiplier Array to Distribution
#'
#' @description
#' Ken-Q uses a fixed 20-element array mapping to sort values -6 to +13.
#' Each element = count of cards at that sort value. This extracts the
#' contiguous non-zero portion as the forced distribution.
#'
#' @keywords internal
kenq_multiplier_to_distribution <- function(mult_array) {
  if (is.null(mult_array)) return(NULL)
  mult <- as.numeric(mult_array)
  nonzero <- which(mult > 0)
  if (length(nonzero) == 0) return(NULL)

  # Extract contiguous non-zero portion
  first <- min(nonzero)
  last <- max(nonzero)
  as.numeric(mult[first:last])
}

# KADE ZIP Format Import ----

#' Read Q-Sort Data from KADE Format
#'
#' @description
#' Import Q-sort data from KADE (Ken-Q Analysis Data Exchange) format.
#' A ZIP archive containing four text files:
#' \itemize{
#'   \item \strong{name.txt}: Project name (single line)
#'   \item \strong{sorts.txt}: Comma/semicolon-delimited, one row per participant
#'     (name followed by sort values for each statement)
#'   \item \strong{statements.txt}: One statement per line
#'   \item \strong{pattern.txt}: 20-element multiplier array
#' }
#'
#' @param file Path to the KADE .zip file
#'
#' @return A QsortData object
#' @export
#'
#' @examples
#' \dontrun{
#' qdata <- read_kade_zip("my_study.zip")
#' }
read_kade_zip <- function(file) {

  cli::cli_alert_info("Reading KADE format from: {.file {basename(file)}}")

  tmp_dir <- tempfile("kade_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  extracted <- utils::unzip(file, exdir = tmp_dir)
  basenames <- tolower(basename(extracted))

  # Find required files
  sorts_file <- extracted[grepl("^sorts", basenames)][1]
  name_file <- extracted[grepl("^name", basenames)][1]
  stmts_file <- extracted[grepl("^statement", basenames)][1]
  pattern_file <- extracted[grepl("^pattern", basenames)][1]

  if (is.na(sorts_file)) {
    rlang::abort(c(
      "Not a valid KADE ZIP file",
      "x" = "Missing sorts.txt",
      "i" = paste("Found files:", paste(basename(extracted), collapse = ", "))
    ))
  }

  # Read project name
  project_name <- "KADE Project"
  if (!is.na(name_file)) {
    name_lines <- readr::read_lines(name_file)
    name_lines <- trimws(name_lines[nchar(trimws(name_lines)) > 0])
    if (length(name_lines) > 0) project_name <- name_lines[1]
  }

  # Read statements
  statements <- NULL
  if (!is.na(stmts_file)) {
    stmts_lines <- readr::read_lines(stmts_file)
    statements <- trimws(stmts_lines[nchar(trimws(stmts_lines)) > 0])
    cli::cli_alert_info("Found {length(statements)} statements")
  }

  # Read pattern (20-element multiplier array)
  distribution <- NULL
  if (!is.na(pattern_file)) {
    pattern_lines <- readr::read_lines(pattern_file)
    pattern_text <- trimws(pattern_lines[nchar(trimws(pattern_lines)) > 0])[1]

    # Auto-detect delimiter (comma vs semicolon)
    delim <- if (stringr::str_count(pattern_text, ";") >
                 stringr::str_count(pattern_text, ",")) ";" else ","
    mult_array <- suppressWarnings(as.numeric(strsplit(pattern_text, delim)[[1]]))
    mult_array <- mult_array[!is.na(mult_array)]

    if (length(mult_array) > 0) {
      if (length(mult_array) < 20) {
        mult_array <- c(mult_array, rep(0, 20 - length(mult_array)))
      }
      distribution <- kenq_multiplier_to_distribution(mult_array[1:20])
      cli::cli_alert_info("Sort pattern: {paste(distribution, collapse = ', ')}")
    }
  }

  # Read sorts (name + direct scores per row)
  sorts_lines <- readr::read_lines(sorts_file)
  sorts_lines <- sorts_lines[nchar(trimws(sorts_lines)) > 0]

  if (length(sorts_lines) == 0) {
    rlang::abort("KADE sorts.txt is empty")
  }

  # Auto-detect delimiter
  sample_line <- sorts_lines[1]
  delim <- if (stringr::str_count(sample_line, ";") >
               stringr::str_count(sample_line, ",")) ";" else ","

  participant_names <- character()
  sorts_list <- list()

  for (line in sorts_lines) {
    parts <- trimws(strsplit(line, delim)[[1]])
    name <- parts[1]
    values <- suppressWarnings(as.numeric(parts[-1]))

    participant_names <- c(participant_names, name)
    sorts_list[[length(sorts_list) + 1]] <- values
  }

  sorts <- do.call(rbind, sorts_list)
  rownames(sorts) <- participant_names

  if (!is.null(statements) && length(statements) == ncol(sorts)) {
    colnames(sorts) <- statements
  }

  cli::cli_alert_success(
    "Imported {nrow(sorts)} participants, {ncol(sorts)} statements"
  )

  qsort_data(
    sorts = sorts,
    statements = statements,
    distribution = distribution,
    metadata = list(project_name = project_name),
    source = paste0("kade:", basename(file)),
    validate = TRUE
  )
}

# Easy-HTMLQ Firebase JSON Import ----

