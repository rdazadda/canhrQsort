#' @title Participant Attributes
#' @description Store, match, bin, and retrieve participant attributes
#'   (demographics) for subgroup analysis of Q-sorts
#' @name participant-attributes
NULL


# Accessors


#' Get Participant Attributes
#'
#' @description
#' Return the participant attribute table stored on a [QsortData] object.
#' The table has one row per participant, aligned to `qdata@participants`,
#' with `participant` as its first column.
#'
#' If no attribute table has been set but a legacy demographics table
#' exists under
#' `metadata$demographics` with a `participant` column, that table is
#' realigned to the participants (one row each, in order, NA rows for
#' unmatched participants) and returned.
#'
#' @param qdata A QsortData object
#'
#' @return A data.frame with `participant` as the first column, or NULL when
#'   no attributes are available
#' @export
#'
#' @examples
#' \dontrun{
#' attrs <- participant_attributes(qdata)
#' }
participant_attributes <- function(qdata) {

  if (!inherits(qdata, "QsortData")) {
    rlang::abort("qdata must be a QsortData object")
  }

  stored <- qdata@metadata$participant_attributes
  if (is.data.frame(stored)) {
    return(stored)
  }

  # Legacy fallback: metadata$demographics with a participant column
  demo <- qdata@metadata$demographics
  if (is.data.frame(demo) && "participant" %in% names(demo)) {
    demo <- as.data.frame(demo, stringsAsFactors = FALSE)
    demo$participant <- as.character(demo$participant)
    demo <- demo[!duplicated(demo$participant), , drop = FALSE]

    idx <- match(qdata@participants, demo$participant)
    rest <- demo[idx, setdiff(names(demo), "participant"), drop = FALSE]
    rownames(rest) <- NULL

    out <- cbind(
      data.frame(participant = as.character(qdata@participants),
                 stringsAsFactors = FALSE),
      rest
    )
    rownames(out) <- NULL
    return(out)
  }

  NULL
}


#' Check for Participant Attributes
#'
#' @description
#' Test whether a [QsortData] object carries participant attributes,
#' either stored directly or available through the legacy demographics
#' fallback.
#'
#' @param qdata A QsortData object
#'
#' @return Logical scalar
#' @export
has_participant_attributes <- function(qdata) {

  if (!inherits(qdata, "QsortData")) {
    rlang::abort("qdata must be a QsortData object")
  }

  !is.null(participant_attributes(qdata))
}


#' Set Participant Attributes
#'
#' @description
#' Store a participant attribute table on a [QsortData] object. The table
#' must have `participant` as its first column and exactly one row per
#' participant in `qdata@participants`. Rows given in a different order are
#' realigned; missing or extra participants are an error (use
#' [attach_participant_attributes()] to match an external file by id).
#'
#' A type specification is initialized under `metadata$attribute_spec` via
#' [detect_attribute_types()] for every attribute column that does not
#' already have one.
#'
#' @param qdata A QsortData object
#' @param attrs A data.frame whose first column is `participant`
#' @param spec Optional named list of attribute specifications to use instead
#'   of automatic detection; names must be attribute columns of `attrs`
#'
#' @return The modified QsortData object
#' @export
#'
#' @examples
#' \dontrun{
#' attrs <- data.frame(participant = qdata@participants,
#'                     age = c("18-30", "31-40", "41-50"))
#' qdata <- set_participant_attributes(qdata, attrs)
#' }
set_participant_attributes <- function(qdata, attrs, spec = NULL) {

  if (!inherits(qdata, "QsortData")) {
    rlang::abort("qdata must be a QsortData object")
  }
  if (!is.data.frame(attrs)) {
    rlang::abort("attrs must be a data.frame")
  }

  attrs <- as.data.frame(attrs, stringsAsFactors = FALSE)

  if (ncol(attrs) < 1 || names(attrs)[1] != "participant") {
    rlang::abort(c(
      "The first column of attrs must be named 'participant'",
      "i" = "Use attach_participant_attributes() to match an attribute file by its id column."
    ))
  }

  attrs$participant <- as.character(attrs$participant)

  if (anyDuplicated(attrs$participant) > 0) {
    dups <- unique(attrs$participant[duplicated(attrs$participant)])
    rlang::abort(c(
      "attrs must have exactly one row per participant",
      "x" = sprintf("Duplicated participant ids: %s", paste(dups, collapse = ", "))
    ))
  }

  participants <- qdata@participants
  missing_p <- setdiff(participants, attrs$participant)
  extra_p <- setdiff(attrs$participant, participants)
  if (length(missing_p) > 0 || length(extra_p) > 0) {
    rlang::abort(c(
      "attrs must contain exactly one row for every participant in qdata",
      if (length(missing_p) > 0) {
        c("x" = sprintf("Participants without a row: %s", paste(missing_p, collapse = ", ")))
      },
      if (length(extra_p) > 0) {
        c("x" = sprintf("Rows without a participant: %s", paste(extra_p, collapse = ", ")))
      },
      "i" = "Use attach_participant_attributes() to match a partial or unordered attribute file."
    ))
  }

  # Realign when complete but out of order
  if (!identical(attrs$participant, participants)) {
    attrs <- attrs[match(participants, attrs$participant), , drop = FALSE]
  }
  rownames(attrs) <- NULL

  attr_cols <- setdiff(names(attrs), "participant")

  # Validate any user-supplied spec entries
  if (!is.null(spec)) {
    if (!is.list(spec) || is.null(names(spec)) || any(names(spec) == "")) {
      rlang::abort("spec must be a named list of attribute specifications")
    }
    unknown <- setdiff(names(spec), attr_cols)
    if (length(unknown) > 0) {
      rlang::abort(sprintf(
        "spec names must be attribute columns of attrs; unknown: %s",
        paste(unknown, collapse = ", ")
      ))
    }
    for (nm in names(spec)) {
      s <- spec[[nm]]
      if (!is.list(s) || is.null(s$type) ||
          !s$type %in% c("numeric", "ordinal", "categorical")) {
        rlang::abort(sprintf(
          "spec[['%s']] must be a list with type 'numeric', 'ordinal', or 'categorical'", nm
        ))
      }
    }
  }

  # Merge: existing metadata spec, then supplied spec, then detection
  spec_all <- qdata@metadata$attribute_spec
  if (!is.list(spec_all)) spec_all <- list()
  if (!is.null(spec)) spec_all[names(spec)] <- spec

  missing_spec <- setdiff(attr_cols, names(spec_all))
  if (length(missing_spec) > 0) {
    detected <- detect_attribute_types(
      attrs[, c("participant", missing_spec), drop = FALSE]
    )
    spec_all[names(detected)] <- detected
  }
  spec_all <- spec_all[attr_cols]

  qdata@metadata$participant_attributes <- attrs
  qdata@metadata$attribute_spec <- spec_all
  qdata
}


# Import and matching


#' Read a Participant Attribute File
#'
#' @description
#' Read a participant attribute (demographics) table from a CSV, TXT, or
#' Excel file. Column names are kept exactly as written in the file. Every
#' column is returned as character except columns whose non-missing values
#' are all plain numbers, which become numeric.
#'
#' @param file Path to a .csv, .txt, .xlsx, or .xls file
#'
#' @return A data.frame with untouched column names
#' @export
#'
#' @examples
#' \dontrun{
#' attrs <- read_participant_attributes("demographics.csv")
#' qdata <- attach_participant_attributes(qdata, attrs)
#' }
read_participant_attributes <- function(file) {

  if (!is.character(file) || length(file) != 1) {
    rlang::abort("file must be a single file path")
  }
  if (!file.exists(file)) {
    rlang::abort(sprintf("File not found: %s", file))
  }

  ext <- tolower(tools::file_ext(file))

  if (ext %in% c("csv", "txt")) {
    df <- utils::read.csv(
      file,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      colClasses = "character",
      fileEncoding = "UTF-8-BOM",
      na.strings = c("", "NA")
    )
  } else if (ext %in% c("xlsx", "xls")) {
    df <- readxl::read_excel(file, sheet = 1, col_types = "text",
                             .name_repair = "minimal")
    df <- as.data.frame(df, stringsAsFactors = FALSE)
  } else {
    rlang::abort(sprintf(
      "Unsupported file format '.%s'. Use .csv, .txt, .xlsx, or .xls.", ext
    ))
  }

  # Promote true numeric columns; everything else stays character
  for (col in names(df)) {
    x <- df[[col]]
    if (!is.character(x)) next
    vals <- trimws(x[!is.na(x)])
    vals <- vals[vals != ""]
    if (length(vals) > 0 &&
        all(grepl("^-?[0-9.]+$", vals)) &&
        !anyNA(suppressWarnings(as.numeric(vals)))) {
      df[[col]] <- suppressWarnings(as.numeric(trimws(x)))
    }
  }

  df
}


#' Match Attribute Rows to Q-Sorts
#'
#' @description
#' Match the rows of an attribute table to a vector of participant ids
#' (typically `qdata@participants`, which are the sort column names of the
#' original data). The id column is chosen automatically unless `id_col`
#' names it: the column whose values overlap the participants the most (as
#' trimmed lowercase strings) wins, with ties broken in favor of columns
#' named like id, participant, or record. When no value matches exactly,
#' matching is retried after removing a leading qsort, q, p, or s prefix
#' (plus padding zeros) from both sides, so that ids like `qsort7` match `7`.
#'
#' @param participants Character vector of participant ids, in sort order
#' @param attrs A data.frame of attribute rows
#' @param id_col Optional name of the column in `attrs` that holds the ids
#'
#' @return A list with elements:
#' * `id_col`: name of the id column used
#' * `matched`: integer count of participants matched to a row
#' * `unmatched_participants`: participants with no attribute row
#' * `unmatched_rows`: id values of rows that matched no participant
#' * `duplicate_ids`: id values that appeared more than once (first kept)
#' * `normalized`: TRUE when prefix normalization was needed
#' * `aligned`: data.frame with `participant` first, then every attrs
#'   column except the id column, NA for unmatched participants
#' * `messages`: character vector of human sentences describing the match
#' @export
#'
#' @examples
#' \dontrun{
#' res <- match_participant_attributes(qdata@participants, attrs)
#' res$messages
#' }
match_participant_attributes <- function(participants, attrs, id_col = NULL) {

  participants <- as.character(participants)
  if (length(participants) == 0) {
    rlang::abort("participants must be a non-empty character vector")
  }
  if (!is.data.frame(attrs) || nrow(attrs) == 0 || ncol(attrs) == 0) {
    rlang::abort("attrs must be a data.frame with at least one row and one column")
  }
  attrs <- as.data.frame(attrs, stringsAsFactors = FALSE)

  keys_p0 <- normalize_id_key(participants)
  name_pref <- grepl("^id$|participant|record", tolower(names(attrs)))

  overlap_count <- function(kp, ka) {
    kp <- kp[!is.na(kp) & kp != ""]
    ka <- ka[!is.na(ka) & ka != ""]
    sum(kp %in% ka)
  }

  pick_best <- function(overlaps) {
    best <- max(overlaps)
    if (best <= 0) return(NULL)
    cand <- which(overlaps == best)
    if (length(cand) > 1 && any(name_pref[cand])) {
      cand <- cand[name_pref[cand]]
    }
    names(attrs)[cand[1]]
  }

  abort_no_match <- function(cand) {
    sample_vals <- utils::head(unique(as.character(attrs[[cand]])), 3)
    rlang::abort(c(
      "Could not match any attribute rows to the sorts.",
      "x" = sprintf("Sort ids look like: %s.",
                    paste(utils::head(participants, 3), collapse = ", ")),
      "x" = sprintf("Best candidate id column '%s' has values like: %s.",
                    cand, paste(sample_vals, collapse = ", ")),
      "i" = "Pass id_col to name the column that holds the sort ids, or fix the ids so they match."
    ))
  }

  normalized <- FALSE

  if (is.null(id_col)) {
    ov0 <- vapply(attrs, function(v) {
      overlap_count(keys_p0, normalize_id_key(v))
    }, numeric(1))
    id_col <- pick_best(ov0)

    if (is.null(id_col)) {
      keys_p1 <- strip_id_prefix(keys_p0)
      ov1 <- vapply(attrs, function(v) {
        overlap_count(keys_p1, strip_id_prefix(normalize_id_key(v)))
      }, numeric(1))
      id_col <- pick_best(ov1)
      normalized <- TRUE

      if (is.null(id_col)) {
        cand <- if (any(name_pref)) names(attrs)[which(name_pref)[1]] else names(attrs)[1]
        abort_no_match(cand)
      }
    }
  } else {
    if (!id_col %in% names(attrs)) {
      rlang::abort(c(
        sprintf("id_col '%s' is not a column of attrs", id_col),
        "i" = sprintf("Available columns: %s", paste(names(attrs), collapse = ", "))
      ))
    }
    if (overlap_count(keys_p0, normalize_id_key(attrs[[id_col]])) == 0) {
      keys_p1 <- strip_id_prefix(keys_p0)
      if (overlap_count(keys_p1, strip_id_prefix(normalize_id_key(attrs[[id_col]]))) == 0) {
        abort_no_match(id_col)
      }
      normalized <- TRUE
    }
  }

  # Build matching keys; blanks get unique sentinels so they never match
  key_p <- keys_p0
  raw_ids <- as.character(attrs[[id_col]])
  key_a <- normalize_id_key(raw_ids)
  if (normalized) {
    key_p <- strip_id_prefix(key_p)
    key_a <- strip_id_prefix(key_a)
  }
  blank_p <- is.na(key_p) | key_p == ""
  key_p[blank_p] <- paste0("\x01blank_participant_", which(blank_p))
  blank_a <- is.na(key_a) | key_a == ""
  key_a[blank_a] <- paste0("\x01blank_row_", which(blank_a))

  # Duplicate ids: keep the first occurrence, record the rest
  dup <- duplicated(key_a)
  duplicate_ids <- unique(raw_ids[dup])
  keep <- !dup
  attrs2 <- attrs[keep, , drop = FALSE]
  key_a2 <- key_a[keep]
  raw_ids2 <- raw_ids[keep]

  idx <- match(key_p, key_a2)
  matched <- as.integer(sum(!is.na(idx)))
  unmatched_participants <- participants[is.na(idx)]
  unused <- setdiff(seq_len(nrow(attrs2)), idx[!is.na(idx)])
  unmatched_rows <- as.character(raw_ids2[unused])

  # Identifier-like columns label rows rather than describe people: a second
  # id column (Record ID next to a sort number) or free text unique to every
  # row. Set them aside so they never surface as attributes in charts,
  # grouping, or tests.
  cand <- setdiff(names(attrs2), id_col)
  norm_names <- gsub("[^a-z0-9]", "", tolower(cand))
  id_name_set <- c(
    "id", "ids", "recordid", "record", "participantid", "participant",
    "subjectid", "subject", "respondentid", "respondent", "caseid",
    "sortid", "sortno", "sortnumber", "qsort", "qsortid", "qsortno",
    "qsortnumber", "pid", "sid", "uid", "rowid"
  )
  # The uniqueness rule needs enough rows to mean anything: in a panel of a
  # handful of sorts a genuine attribute (gender in a panel of two) can be
  # distinct on every row without being an identifier
  all_distinct <- vapply(cand, function(nm) {
    v <- attrs2[[nm]]
    v <- v[!is.na(v) & trimws(as.character(v)) != ""]
    length(v) >= 12 && anyDuplicated(v) == 0
  }, logical(1))
  is_num <- vapply(cand, function(nm) is.numeric(attrs2[[nm]]), logical(1))
  set_aside <- cand[norm_names %in% id_name_set | (all_distinct & !is_num)]

  # Aligned attribute table: participant first, then non-id columns
  rest <- attrs2[idx, setdiff(cand, set_aside), drop = FALSE]
  rownames(rest) <- NULL
  if (ncol(rest) > 0) {
    names(rest) <- make.unique(c("participant", names(rest)))[-1]
  }
  aligned <- cbind(
    data.frame(participant = participants, stringsAsFactors = FALSE),
    rest
  )
  rownames(aligned) <- NULL

  # Human-readable match report
  n_word <- function(n, singular, plural) if (n == 1) singular else plural
  id_list <- function(x) paste(x, collapse = ", ")

  messages <- sprintf(
    "Matched %d of %d %s to demographic rows using id column '%s'.",
    matched, length(participants),
    n_word(length(participants), "sort", "sorts"), id_col
  )

  if (length(unmatched_participants) > 0) {
    n <- length(unmatched_participants)
    messages <- c(messages, sprintf(
      "%d %s no demographics (%s).",
      n, n_word(n, "sort has", "sorts have"), id_list(unmatched_participants)
    ))
  }

  if (length(unmatched_rows) > 0) {
    n <- length(unmatched_rows)
    messages <- c(messages, sprintf(
      "%d demographic %s matched no sort and %s dropped (%s).",
      n, n_word(n, "row", "rows"), n_word(n, "was", "were"), id_list(unmatched_rows)
    ))
  }

  if (length(duplicate_ids) > 0) {
    n <- length(duplicate_ids)
    messages <- c(messages, sprintf(
      "%d duplicate %s in the demographics file (%s); first occurrence kept.",
      n, n_word(n, "id", "ids"), id_list(duplicate_ids)
    ))
  }

  if (length(set_aside) > 0) {
    n <- length(set_aside)
    messages <- c(messages, sprintf(
      "Set aside %s %s.",
      n_word(n, "identifier column", "identifier columns"),
      id_list(set_aside)
    ))
  }

  if (normalized && matched > 0) {
    ex <- which(!is.na(idx))[1]
    p_raw <- participants[ex]
    a_raw <- raw_ids2[idx[ex]]
    p_norm <- normalize_id_key(p_raw)
    if (strip_id_prefix(p_norm) != p_norm) {
      prefix <- regmatches(p_norm, regexpr("^(qsort|q|p|s)", p_norm))
      left <- p_raw
      right <- a_raw
    } else {
      a_norm <- normalize_id_key(a_raw)
      prefix <- regmatches(a_norm, regexpr("^(qsort|q|p|s)", a_norm))
      left <- a_raw
      right <- p_raw
    }
    messages <- c(messages, sprintf(
      "Ids matched after removing the '%s' prefix (e.g. %s = %s).",
      prefix, left, right
    ))
  }

  list(
    id_col = id_col,
    matched = matched,
    unmatched_participants = unmatched_participants,
    unmatched_rows = unmatched_rows,
    duplicate_ids = duplicate_ids,
    set_aside = set_aside,
    normalized = normalized,
    aligned = aligned,
    messages = messages
  )
}


#' Attach Participant Attributes to Q-Sort Data
#'
#' @description
#' Match an attribute table to the participants of a [QsortData] object
#' with [match_participant_attributes()], store the aligned attributes with
#' [set_participant_attributes()], and keep the match report under
#' `metadata$attribute_match`.
#'
#' @param qdata A QsortData object
#' @param attrs A data.frame of attribute rows, e.g. from
#'   [read_participant_attributes()]
#' @param id_col Optional name of the column in `attrs` that holds the
#'   participant ids; detected automatically when NULL
#'
#' @return The modified QsortData object
#' @export
#'
#' @examples
#' \dontrun{
#' attrs <- read_participant_attributes("demographics.csv")
#' qdata <- attach_participant_attributes(qdata, attrs)
#' qdata@metadata$attribute_match$messages
#' }
attach_participant_attributes <- function(qdata, attrs, id_col = NULL) {

  if (!inherits(qdata, "QsortData")) {
    rlang::abort("qdata must be a QsortData object")
  }

  report <- match_participant_attributes(qdata@participants, attrs, id_col = id_col)

  qdata <- set_participant_attributes(qdata, report$aligned)
  qdata@metadata$attribute_match <- report[setdiff(names(report), "aligned")]

  cli::cli_alert_success("{report$messages[1]}")
  for (m in report$messages[-1]) {
    cli::cli_alert_info("{m}")
  }

  qdata
}


# Attribute types and binning


#' Detect Attribute Types
#'
#' @description
#' Classify every non-participant column of an attribute table as numeric,
#' ordinal, or categorical. A column is numeric when it is numeric or all
#' its non-missing values are plain numbers. It is ordinal when all values
#' are range strings such as `"18-30"`, `"31 to 40"`, or `"71+"`, with
#' levels ordered by the parsed lower bound. Everything else is categorical
#' with alphabetically sorted levels.
#'
#' @param attrs A data.frame of attributes (a `participant` column is skipped)
#'
#' @return A named list with one entry per attribute column, each a list
#'   with elements `type`, `levels`, and `order`
#' @export
#'
#' @examples
#' detect_attribute_types(data.frame(age = c("18-30", "71+", "31-40")))
detect_attribute_types <- function(attrs) {

  if (!is.data.frame(attrs)) {
    rlang::abort("attrs must be a data.frame")
  }

  spec <- list()

  for (col in setdiff(names(attrs), "participant")) {
    x <- attrs[[col]]
    if (is.factor(x)) x <- as.character(x)
    non_na <- x[!is.na(x)]

    numeric_like <- is.numeric(x) ||
      (length(non_na) > 0 &&
         all(grepl("^-?[0-9.]+$", trimws(as.character(non_na)))) &&
         !anyNA(suppressWarnings(as.numeric(trimws(as.character(non_na))))))

    if (numeric_like) {
      spec[[col]] <- list(type = "numeric", levels = NULL, order = NULL)
      next
    }

    chr <- trimws(as.character(non_na))
    if (length(chr) > 0 && all(is_range_level(chr))) {
      lv <- unique(chr)
      lv <- lv[order(range_lower_bound(lv))]
      spec[[col]] <- list(type = "ordinal", levels = lv, order = lv)
    } else {
      spec[[col]] <- list(type = "categorical", levels = sort(unique(chr)), order = NULL)
    }
  }

  spec
}


#' Set Binning for a Participant Attribute
#'
#' @description
#' Store binning instructions for one attribute in
#' `metadata$attribute_spec`. Numeric attributes take `breaks` (strictly
#' increasing) with optional `labels`. Ordinal and categorical attributes
#' take a `map` (named character vector, original level to target label,
#' covering every level; levels sharing a target label are merged) with an
#' optional `order` of the target labels. Raw attribute values are never
#' modified; binning is applied on the fly by [attribute_groups()].
#'
#' @param qdata A QsortData object with participant attributes set
#' @param attribute Name of the attribute column to bin
#' @param breaks Numeric vector of cut points for a numeric attribute
#' @param labels Optional labels for the numeric bins
#'   (length `length(breaks) - 1`)
#' @param map Named character vector mapping original levels to target labels
#' @param order Optional character vector giving the display order of the
#'   target labels
#'
#' @return The modified QsortData object
#' @export
#'
#' @examples
#' \dontrun{
#' qdata <- set_attribute_binning(
#'   qdata, "age",
#'   map = c("18-30" = "18-40", "31-40" = "18-40",
#'           "41-50" = "41-71+", "51-60" = "41-71+",
#'           "61-70" = "41-71+", "71+" = "41-71+"),
#'   order = c("18-40", "41-71+")
#' )
#' }
set_attribute_binning <- function(qdata, attribute, breaks = NULL, labels = NULL,
                                  map = NULL, order = NULL) {

  if (!inherits(qdata, "QsortData")) {
    rlang::abort("qdata must be a QsortData object")
  }
  if (!is.character(attribute) || length(attribute) != 1) {
    rlang::abort("attribute must be a single column name")
  }

  attrs <- participant_attributes(qdata)
  if (is.null(attrs)) {
    rlang::abort(c(
      "No participant attributes are set",
      "i" = "Use set_participant_attributes() or attach_participant_attributes() first."
    ))
  }

  attr_cols <- setdiff(names(attrs), "participant")
  if (!attribute %in% attr_cols) {
    rlang::abort(c(
      sprintf("'%s' is not a participant attribute", attribute),
      "i" = sprintf("Available attributes: %s", paste(attr_cols, collapse = ", "))
    ))
  }

  spec_all <- qdata@metadata$attribute_spec
  if (!is.list(spec_all)) spec_all <- list()
  spec <- spec_all[[attribute]]
  if (is.null(spec)) {
    spec <- detect_attribute_types(attrs)[[attribute]]
  }

  if (identical(spec$type, "numeric")) {
    if (!is.null(map)) {
      rlang::abort(sprintf(
        "map applies to ordinal or categorical attributes; '%s' is numeric. Use breaks instead.",
        attribute
      ))
    }
    if (is.null(breaks)) {
      rlang::abort(sprintf("breaks are required to bin the numeric attribute '%s'", attribute))
    }
    if (!is.numeric(breaks) || length(breaks) < 2 || anyNA(breaks) ||
        any(diff(breaks) <= 0)) {
      rlang::abort("breaks must be a strictly increasing numeric vector with at least two values")
    }
    if (!is.null(labels) && length(labels) != length(breaks) - 1) {
      rlang::abort(sprintf(
        "labels must have length %d (one per bin); got %d",
        length(breaks) - 1, length(labels)
      ))
    }
    spec$breaks <- as.numeric(breaks)
    spec$labels <- if (is.null(labels)) NULL else as.character(labels)
  } else {
    if (!is.null(breaks)) {
      rlang::abort(sprintf(
        "breaks apply to numeric attributes; '%s' is %s. Use map instead.",
        attribute, spec$type
      ))
    }
    if (is.null(map)) {
      rlang::abort(sprintf(
        "map is required to bin the %s attribute '%s'", spec$type, attribute
      ))
    }
    if (!is.character(map) || is.null(names(map)) || any(names(map) == "")) {
      rlang::abort("map must be a named character vector (original level to target label)")
    }

    lv <- spec$levels
    if (is.null(lv)) {
      lv <- unique(trimws(as.character(attrs[[attribute]][!is.na(attrs[[attribute]])])))
    }
    missing_lv <- setdiff(lv, names(map))
    if (length(missing_lv) > 0) {
      rlang::abort(c(
        sprintf("map must cover every level of '%s'", attribute),
        "x" = sprintf("Missing levels: %s", paste(missing_lv, collapse = ", "))
      ))
    }

    if (!is.null(order)) {
      targets <- unique(unname(map))
      if (!setequal(order, targets)) {
        rlang::abort(c(
          "order must contain exactly the target labels of map",
          "x" = sprintf("Target labels: %s", paste(targets, collapse = ", ")),
          "x" = sprintf("order given: %s", paste(order, collapse = ", "))
        ))
      }
      spec$order <- as.character(order)
    } else {
      # A previously detected order lists raw levels, which no longer apply
      # once levels are merged; the map keeps the ordinal ordering instead
      spec$order <- NULL
    }
    spec$map <- map
  }

  spec_all[[attribute]] <- spec
  qdata@metadata$attribute_spec <- spec_all
  qdata
}


#' Reset Attribute Grouping to the Original
#'
#' @description
#' Drop any stored binning for one attribute (breaks, labels, map, order)
#' and restore the specification detected from the raw values, so the
#' attribute groups exactly as it did when first attached.
#'
#' @param qdata A QsortData object with participant attributes
#' @param attribute Name of the attribute column to reset
#'
#' @return The modified QsortData object
#' @export
#'
#' @examples
#' \dontrun{
#' qdata <- reset_attribute_binning(qdata, "age")
#' }
reset_attribute_binning <- function(qdata, attribute) {

  if (!inherits(qdata, "QsortData")) {
    rlang::abort("qdata must be a QsortData object")
  }
  if (!is.character(attribute) || length(attribute) != 1) {
    rlang::abort("attribute must be a single column name")
  }

  attrs <- participant_attributes(qdata)
  if (is.null(attrs)) {
    rlang::abort(c(
      "No participant attributes are set",
      "i" = "Use set_participant_attributes() or attach_participant_attributes() first."
    ))
  }

  attr_cols <- setdiff(names(attrs), "participant")
  if (!attribute %in% attr_cols) {
    rlang::abort(c(
      sprintf("'%s' is not a participant attribute", attribute),
      "i" = sprintf("Available attributes: %s", paste(attr_cols, collapse = ", "))
    ))
  }

  spec_all <- qdata@metadata$attribute_spec
  if (!is.list(spec_all)) spec_all <- list()
  spec_all[[attribute]] <- detect_attribute_types(attrs)[[attribute]]
  qdata@metadata$attribute_spec <- spec_all
  qdata
}


#' Bin Attribute Values by a Specification
#'
#' @description
#' Apply an attribute specification (as stored in
#' `metadata$attribute_spec`) to a vector of raw values. A numeric spec with
#' `breaks` cuts the values (`include.lowest = TRUE`). A spec with a `map`
#' recodes levels and builds a factor in the given `order` (ordered when the
#' type is ordinal or an explicit order is given). Without breaks or map,
#' the raw values become a factor, ordered by parsed lower bound for
#' ordinal attributes.
#'
#' @param values Vector of raw attribute values
#' @param spec A specification list with elements `type`, and optionally
#'   `levels`, `order`, `breaks`, `labels`, and `map`
#'
#' @return A factor of the same length as `values`
#' @export
#'
#' @examples
#' bin_attribute(c(3, 60, 85), list(type = "numeric", breaks = c(0, 50, 100)))
bin_attribute <- function(values, spec) {

  if (!is.list(spec) || is.null(spec$type)) {
    rlang::abort("spec must be a list with a 'type' element")
  }
  if (is.factor(values)) values <- as.character(values)

  # Numeric with breaks: cut into bins
  if (identical(spec$type, "numeric") && !is.null(spec$breaks)) {
    x <- suppressWarnings(as.numeric(values))
    return(cut(x, breaks = spec$breaks, labels = spec$labels,
               include.lowest = TRUE))
  }

  # Map: recode levels, then factor in order
  if (!is.null(spec$map)) {
    chr <- trimws(as.character(values))
    mapped <- unname(spec$map[chr])
    targets <- unique(unname(spec$map))
    # An order is only usable here when it names the map targets; a detected
    # ordinal order refers to the raw levels and is ignored once a map exists
    use_order <- !is.null(spec$order) && all(spec$order %in% targets)
    if (use_order) {
      lv <- spec$order
    } else if (identical(spec$type, "ordinal") && !is.null(spec$levels)) {
      lv <- unique(unname(spec$map[spec$levels]))
      lv <- lv[!is.na(lv)]
    } else {
      lv <- sort(targets)
    }
    ordered_flag <- identical(spec$type, "ordinal") || use_order
    return(factor(mapped, levels = lv, ordered = ordered_flag))
  }

  # No binning spec: factor of the raw values
  chr <- trimws(as.character(values))
  if (identical(spec$type, "ordinal")) {
    lv <- spec$levels
    if (is.null(lv)) {
      lv <- unique(chr[!is.na(chr)])
      lv <- lv[order(range_lower_bound(lv))]
    }
    return(factor(chr, levels = lv, ordered = TRUE))
  }

  lv <- if (!is.null(spec$levels)) spec$levels else sort(unique(chr[!is.na(chr)]))
  factor(chr, levels = lv, ordered = FALSE)
}


#' Attribute Grouping Vectors for Subgroup Analysis
#'
#' @description
#' Build one grouping vector per participant attribute, aligned to
#' `qdata@participants`, by applying each stored specification with
#' [bin_attribute()]. Numeric attributes without breaks are returned as
#' numeric vectors (not factors) so the caller can run a correlation
#' instead of a group test. Every vector carries the attribute type in
#' `attr(x, "type")`.
#'
#' @param qdata A QsortData object with participant attributes
#' @param min_n Minimum group size; factor levels with fewer participants
#'   are set to NA and dropped (default 0, keep all)
#'
#' @return A named list of factors and numeric vectors, one per attribute
#' @export
#'
#' @examples
#' \dontrun{
#' groups <- attribute_groups(qdata, min_n = 3)
#' }
attribute_groups <- function(qdata, min_n = 0) {

  if (!inherits(qdata, "QsortData")) {
    rlang::abort("qdata must be a QsortData object")
  }

  attrs <- participant_attributes(qdata)
  if (is.null(attrs)) {
    cli::cli_alert_warning("No participant attributes found; returning an empty list")
    return(stats::setNames(list(), character(0)))
  }

  spec_all <- qdata@metadata$attribute_spec
  if (!is.list(spec_all)) spec_all <- list()
  detected <- detect_attribute_types(attrs)

  out <- list()
  for (col in setdiff(names(attrs), "participant")) {
    spec <- spec_all[[col]]
    if (is.null(spec)) spec <- detected[[col]]
    vals <- attrs[[col]]

    # Numeric without breaks: hand back the numbers for correlation
    if (identical(spec$type, "numeric") && is.null(spec$breaks)) {
      x <- suppressWarnings(as.numeric(vals))
      names(x) <- attrs$participant
      attr(x, "type") <- spec$type
      out[[col]] <- x
      next
    }

    f <- bin_attribute(vals, spec)
    names(f) <- attrs$participant

    if (min_n > 0) {
      counts <- table(f)
      drop_lv <- names(counts)[counts < min_n]
      if (length(drop_lv) > 0) {
        f[f %in% drop_lv] <- NA
        f <- factor(f, levels = setdiff(levels(f), drop_lv),
                    ordered = is.ordered(f))
        names(f) <- attrs$participant
      }
    }

    attr(f, "type") <- spec$type
    out[[col]] <- f
  }

  out
}


# Internal helpers


#' Normalize Ids for Matching
#' @description Lowercase and trim ids so comparisons ignore case and spacing.
#' @keywords internal
normalize_id_key <- function(x) {
  tolower(trimws(as.character(x)))
}

#' Strip Common Q-Sort Id Prefixes
#' @description Remove a leading qsort, q, p, or s prefix plus padding zeros
#'   from an already-normalized id, so that qsort07 becomes 7.
#' @keywords internal
strip_id_prefix <- function(x) {
  sub("^(qsort|q|p|s)0*", "", x)
}

#' Detect Range-Style Levels
#' @description TRUE for values like "18-30", "31 to 40", or "71+".
#' @keywords internal
is_range_level <- function(x) {
  x <- trimws(as.character(x))
  grepl("^\\s*\\d+\\s*(-|to|\u2013)\\s*\\d+\\s*$", x) | grepl("^\\d+\\s*\\+$", x)
}

#' Lower Bound of a Range Level
#' @description Parse the leading number of a range string for ordering.
#' @keywords internal
range_lower_bound <- function(x) {
  suppressWarnings(as.numeric(sub("^\\s*(\\d+).*$", "\\1", trimws(as.character(x)))))
}
