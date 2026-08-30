# Tests for participant attributes (demographics) layer

make_test_qdata <- function(ids, n_statements = 8) {
  set.seed(42)
  sorts <- matrix(
    sample(-3:3, length(ids) * n_statements, replace = TRUE),
    nrow = length(ids), ncol = n_statements
  )
  rownames(sorts) <- ids
  colnames(sorts) <- paste0("S", seq_len(n_statements))
  suppressMessages(
    qsort_data(sorts, distribution = rep(1, n_statements), validate = FALSE)
  )
}


# Accessor round trip


test_that("set/get round trip works through a real qsort_data object", {
  ids <- c("KK01", "KK02", "KK03", "KK04")
  qd <- make_test_qdata(ids)

  expect_false(has_participant_attributes(qd))
  expect_null(participant_attributes(qd))

  attrs <- data.frame(
    participant = ids,
    age = c("18-30", "31-40", "41-50", "71+"),
    score = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )

  qd2 <- set_participant_attributes(qd, attrs)

  expect_true(has_participant_attributes(qd2))
  got <- participant_attributes(qd2)
  expect_s3_class(got, "data.frame")
  expect_identical(names(got)[1], "participant")
  expect_identical(got$participant, ids)
  expect_identical(got$age, attrs$age)
  expect_identical(got$score, attrs$score)

  # Spec initialized for every attribute column
  expect_named(qd2@metadata$attribute_spec, c("age", "score"))
  expect_identical(qd2@metadata$attribute_spec$age$type, "ordinal")
  expect_identical(qd2@metadata$attribute_spec$score$type, "numeric")

  # Original object untouched
  expect_false(has_participant_attributes(qd))
})

test_that("set_participant_attributes realigns unordered but complete input", {
  ids <- c("KK01", "KK02", "KK03", "KK04")
  qd <- make_test_qdata(ids)

  attrs <- data.frame(
    participant = ids,
    age = c("18-30", "31-40", "41-50", "71+"),
    stringsAsFactors = FALSE
  )
  shuffled <- attrs[c(3, 1, 4, 2), ]

  qd2 <- set_participant_attributes(qd, shuffled)
  got <- participant_attributes(qd2)
  expect_identical(got$participant, ids)
  expect_identical(got$age, attrs$age)
})

test_that("set_participant_attributes rejects bad input", {
  ids <- c("KK01", "KK02", "KK03")
  qd <- make_test_qdata(ids)
  attrs <- data.frame(participant = ids, x = 1:3, stringsAsFactors = FALSE)

  # Incomplete
  expect_error(set_participant_attributes(qd, attrs[1:2, ]), "every participant")
  # Extra row
  extra <- rbind(attrs, data.frame(participant = "ZZ99", x = 4))
  expect_error(set_participant_attributes(qd, extra), "every participant")
  # First column not participant
  bad <- data.frame(id = ids, x = 1:3, stringsAsFactors = FALSE)
  expect_error(set_participant_attributes(qd, bad), "participant")
  # Duplicate participant rows
  dup <- attrs[c(1, 1, 2, 3), ]
  expect_error(set_participant_attributes(qd, dup), "exactly one row")
  # Not a data.frame
  expect_error(set_participant_attributes(qd, "nope"), "data.frame")
})


# Legacy demographics fallback


test_that("legacy metadata$demographics is realigned and returned", {
  ids <- c("A", "B", "C")
  qd <- make_test_qdata(ids)
  qd@metadata$demographics <- data.frame(
    participant = c("C", "A", "Z"),
    gender = c("Female", "Male", "Other"),
    site = c("s3", "s1", "s9"),
    stringsAsFactors = FALSE
  )

  expect_true(has_participant_attributes(qd))
  got <- participant_attributes(qd)
  expect_identical(names(got)[1], "participant")
  expect_true(is.character(got$participant))
  expect_identical(got$participant, ids)
  expect_identical(got$gender, c("Male", NA, "Female"))
  expect_identical(got$site, c("s1", NA, "s3"))
})

test_that("stored participant_attributes wins over legacy demographics", {
  ids <- c("A", "B")
  qd <- make_test_qdata(ids)
  qd@metadata$demographics <- data.frame(
    participant = ids, gender = c("x", "y"), stringsAsFactors = FALSE
  )
  qd <- set_participant_attributes(
    qd,
    data.frame(participant = ids, gender = c("Male", "Female"),
               stringsAsFactors = FALSE)
  )
  expect_identical(participant_attributes(qd)$gender, c("Male", "Female"))
})


# Id column detection


test_that("id detection picks 'id' over 'record_id' on a Kake-shaped frame", {
  ids <- sprintf("KK%02d", 1:6)
  attrs <- data.frame(
    record_id = 1:6,
    id = ids,
    age = c("18-30", "31-40", "41-50", "51-60", "61-70", "71+"),
    gender = rep(c("Male", "Female"), 3),
    stringsAsFactors = FALSE
  )

  res <- match_participant_attributes(ids, attrs)

  expect_identical(res$id_col, "id")
  expect_identical(res$matched, 6L)
  expect_false(res$normalized)
  expect_identical(res$unmatched_participants, character(0))
  expect_identical(res$unmatched_rows, character(0))
  expect_identical(res$duplicate_ids, character(0))
  expect_identical(names(res$aligned)[1], "participant")
  expect_true(all(c("age", "gender") %in% names(res$aligned)))
  expect_false("id" %in% names(res$aligned))
  # The losing id column is an identifier, not an attribute
  expect_false("record_id" %in% names(res$aligned))
  expect_identical(res$set_aside, "record_id")
  expect_identical(res$aligned$participant, ids)
  expect_identical(res$aligned$age[1], "18-30")
  expect_match(res$messages[1], "Matched 6 of 6 sorts", fixed = TRUE)
  expect_match(res$messages[1], "using id column 'id'.", fixed = TRUE)
})

test_that("id detection tie-break prefers id-like column names", {
  parts <- c("1", "2", "3")
  attrs <- data.frame(
    junk = c("1", "2", "3"),
    id = c("1", "2", "3"),
    v = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  res <- match_participant_attributes(parts, attrs)
  expect_identical(res$id_col, "id")
  expect_identical(res$matched, 3L)
})

test_that("id detection aborts with a useful message when nothing matches", {
  err <- expect_error(
    match_participant_attributes(
      c("X1", "X2"),
      data.frame(id = c("zzz", "yyy"), v = 1:2, stringsAsFactors = FALSE)
    ),
    "Could not match"
  )
  expect_match(conditionMessage(err), "X1")
  expect_match(conditionMessage(err), "zzz")
})


# Prefix normalization


test_that("prefix normalization matches qsort7 to 7", {
  parts <- c("qsort1", "qsort2", "qsort7")
  attrs <- data.frame(
    id = c("1", "2", "7"),
    grp = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )

  res <- match_participant_attributes(parts, attrs)

  expect_true(res$normalized)
  expect_identical(res$matched, 3L)
  expect_identical(res$aligned$participant, parts)
  expect_identical(res$aligned$grp, c("a", "b", "c"))
  expect_true(any(grepl("removing the 'qsort' prefix", res$messages, fixed = TRUE)))
  expect_true(any(grepl("qsort1 = 1", res$messages, fixed = TRUE)))
})

test_that("prefix normalization also strips zero padding and works both ways", {
  # Attribute file has the prefixed ids, sorts have bare numbers
  parts <- c("7", "8")
  attrs <- data.frame(
    id = c("q07", "q08"),
    grp = c("a", "b"),
    stringsAsFactors = FALSE
  )
  res <- match_participant_attributes(parts, attrs)
  expect_true(res$normalized)
  expect_identical(res$matched, 2L)
  expect_identical(res$aligned$grp, c("a", "b"))
  expect_true(any(grepl("removing the 'q' prefix", res$messages, fixed = TRUE)))
})


# Duplicates and unmatched


test_that("duplicate ids keep the first occurrence and are recorded", {
  ids <- c("KK01", "KK02", "KK03")
  attrs <- data.frame(
    id = c("KK01", "KK02", "KK02", "KK03", "KK02"),
    v = c("a", "b", "b2", "c", "b3"),
    stringsAsFactors = FALSE
  )

  res <- match_participant_attributes(ids, attrs)

  expect_identical(res$duplicate_ids, "KK02")
  expect_identical(res$aligned$v, c("a", "b", "c"))
  expect_identical(res$matched, 3L)
  expect_true(any(grepl(
    "1 duplicate id in the demographics file (KK02); first occurrence kept.",
    res$messages, fixed = TRUE
  )))
})

test_that("unmatched participants get NA rows and unmatched rows are dropped", {
  ids <- c("KK01", "KK02", "KK03", "KK17")
  attrs <- data.frame(
    id = c("KK01", "KK02", "A101"),
    v = c(10, 20, 30),
    stringsAsFactors = FALSE
  )

  res <- match_participant_attributes(ids, attrs)

  expect_identical(res$matched, 2L)
  expect_identical(res$unmatched_participants, c("KK03", "KK17"))
  expect_identical(res$unmatched_rows, "A101")
  expect_identical(res$aligned$participant, ids)
  expect_identical(res$aligned$v, c(10, 20, NA, NA))
  expect_true(any(grepl(
    "2 sorts have no demographics (KK03, KK17).",
    res$messages, fixed = TRUE
  )))
  expect_true(any(grepl(
    "1 demographic row matched no sort and was dropped (A101).",
    res$messages, fixed = TRUE
  )))
})


# Type detection


test_that("detect_attribute_types flags range strings as ordinal", {
  spec <- detect_attribute_types(data.frame(
    participant = c("a", "b", "c", "d"),
    age = c("41-50", "18-30", "71+", "31-40"),
    gender = c("Male", "Female", "Female", "Male"),
    score = c("1", "2", "3.5", "4"),
    stringsAsFactors = FALSE
  ))

  expect_named(spec, c("age", "gender", "score"))

  expect_identical(spec$age$type, "ordinal")
  expect_identical(spec$age$levels, c("18-30", "31-40", "41-50", "71+"))
  expect_identical(spec$age$order, spec$age$levels)

  expect_identical(spec$gender$type, "categorical")
  expect_identical(spec$gender$levels, c("Female", "Male"))

  expect_identical(spec$score$type, "numeric")
})

test_that("detect_attribute_types handles 'to' ranges and NA values", {
  spec <- detect_attribute_types(data.frame(
    span = c("31 to 40", NA, "1 to 5", "18 to 30"),
    stringsAsFactors = FALSE
  ))
  expect_identical(spec$span$type, "ordinal")
  expect_identical(spec$span$levels, c("1 to 5", "18 to 30", "31 to 40"))
})


# Binning: merge map


test_that("merge map collapses age into 18-40 vs 41-71+", {
  ids <- sprintf("KK%02d", 1:6)
  qd <- make_test_qdata(ids)
  raw_age <- c("18-30", "31-40", "41-50", "51-60", "61-70", "71+")
  qd <- set_participant_attributes(
    qd, data.frame(participant = ids, age = raw_age, stringsAsFactors = FALSE)
  )

  qd <- set_attribute_binning(
    qd, "age",
    map = c("18-30" = "18-40", "31-40" = "18-40",
            "41-50" = "41-71+", "51-60" = "41-71+",
            "61-70" = "41-71+", "71+" = "41-71+"),
    order = c("18-40", "41-71+")
  )

  groups <- attribute_groups(qd)
  expect_s3_class(groups$age, "factor")
  expect_identical(levels(groups$age), c("18-40", "41-71+"))
  expect_true(is.ordered(groups$age))
  expect_identical(
    as.character(groups$age),
    c("18-40", "18-40", "41-71+", "41-71+", "41-71+", "41-71+")
  )
  expect_identical(attr(groups$age, "type"), "ordinal")
  expect_identical(names(groups$age), ids)

  # Raw values are never modified
  expect_identical(participant_attributes(qd)$age, raw_age)

  # Map must cover every level
  expect_error(
    set_attribute_binning(qd, "age", map = c("18-30" = "young")),
    "cover every level"
  )
  # Order must match the target labels
  expect_error(
    set_attribute_binning(
      qd, "age",
      map = c("18-30" = "a", "31-40" = "a", "41-50" = "b",
              "51-60" = "b", "61-70" = "b", "71+" = "b"),
      order = c("a", "zzz")
    ),
    "target labels"
  )
})


# Binning: numeric breaks


test_that("numeric attributes bin with custom breaks and stay numeric without", {
  ids <- c("A", "B", "C", "D")
  qd <- make_test_qdata(ids)
  qd <- set_participant_attributes(
    qd, data.frame(participant = ids, score = c(0, 25, 50, 90),
                   stringsAsFactors = FALSE)
  )

  # Without breaks: numeric vector for correlation, not a factor
  g1 <- attribute_groups(qd)
  expect_true(is.numeric(g1$score))
  expect_false(is.factor(g1$score))
  expect_identical(attr(g1$score, "type"), "numeric")
  expect_identical(as.vector(g1$score), c(0, 25, 50, 90))

  # With breaks and labels: binned factor, include.lowest keeps the 0
  qd <- set_attribute_binning(qd, "score", breaks = c(0, 50, 100),
                              labels = c("low", "high"))
  g2 <- attribute_groups(qd)
  expect_s3_class(g2$score, "factor")
  expect_identical(as.character(g2$score), c("low", "low", "low", "high"))

  # Validation
  expect_error(set_attribute_binning(qd, "score", breaks = c(10, 5)),
               "strictly increasing")
  expect_error(set_attribute_binning(qd, "score", breaks = c(0, 50, 50)),
               "strictly increasing")
  expect_error(set_attribute_binning(qd, "score", breaks = c(0, 50, 100),
                                     labels = "only-one"),
               "labels")
  expect_error(set_attribute_binning(qd, "score", map = c("a" = "b")),
               "numeric")
  expect_error(set_attribute_binning(qd, "nope", breaks = c(0, 1)),
               "not a participant attribute")
})

test_that("bin_attribute is pure and honors include.lowest", {
  f <- bin_attribute(
    c(0, 50, 51, NA),
    list(type = "numeric", breaks = c(0, 50, 100), labels = c("low", "high"))
  )
  expect_identical(as.character(f), c("low", "low", "high", NA))

  # No binning spec: ordinal raw values become an ordered factor by lower bound
  f2 <- bin_attribute(
    c("71+", "18-30", "41-50"),
    list(type = "ordinal", levels = c("18-30", "41-50", "71+"))
  )
  expect_true(is.ordered(f2))
  expect_identical(levels(f2), c("18-30", "41-50", "71+"))
  expect_true(f2[2] < f2[3])

  # Categorical with no spec details: sorted unique levels, unordered
  f3 <- bin_attribute(c("b", "a", "b"), list(type = "categorical"))
  expect_false(is.ordered(f3))
  expect_identical(levels(f3), c("a", "b"))
})

test_that("attribute_groups drops small groups when min_n is set", {
  ids <- c("A", "B", "C", "D", "E")
  qd <- make_test_qdata(ids)
  qd <- set_participant_attributes(
    qd, data.frame(participant = ids,
                   gender = c("Male", "Male", "Female", "Female", "Other"),
                   stringsAsFactors = FALSE)
  )

  g <- attribute_groups(qd, min_n = 2)
  expect_identical(levels(g$gender), c("Female", "Male"))
  expect_true(is.na(g$gender[["E"]]))
})


# Attach and metadata persistence


test_that("attach_participant_attributes stores attributes, spec, and report", {
  ids <- sprintf("KK%02d", 1:5)
  qd <- make_test_qdata(ids)
  file_attrs <- data.frame(
    record_id = 1:6,
    id = c(ids, "ZZ99"),
    age = c("18-30", "31-40", "41-50", "51-60", "61-70", "71+"),
    stringsAsFactors = FALSE
  )

  qd2 <- suppressMessages(attach_participant_attributes(qd, file_attrs))

  # Attributes stored and aligned
  got <- participant_attributes(qd2)
  expect_identical(got$participant, ids)
  expect_identical(got$age, file_attrs$age[1:5])
  # The losing id column is set aside, never stored as an attribute
  expect_false("record_id" %in% names(got))

  # Spec persisted through the metadata slot
  expect_true(is.list(qd2@metadata$attribute_spec))
  expect_identical(qd2@metadata$attribute_spec$age$type, "ordinal")

  # Match report stored without the aligned table
  report <- qd2@metadata$attribute_match
  expect_false("aligned" %in% names(report))
  expect_identical(report$id_col, "id")
  expect_identical(report$matched, 5L)
  expect_identical(report$unmatched_rows, "ZZ99")
  expect_true(is.character(report$messages))

  # Binning set after attach persists through the same metadata slot
  qd3 <- set_attribute_binning(
    qd2, "age",
    map = c("18-30" = "18-40", "31-40" = "18-40",
            "41-50" = "41-71+", "51-60" = "41-71+",
            "61-70" = "41-71+", "71+" = "41-71+")
  )
  expect_identical(qd3@metadata$attribute_spec$age$map[["71+"]], "41-71+")
  g <- attribute_groups(qd3)
  expect_identical(levels(g$age), c("18-40", "41-71+"))
})


# File reading


test_that("read_participant_attributes keeps names and types faithful", {
  tf <- tempfile(fileext = ".csv")
  on.exit(unlink(tf), add = TRUE)
  writeLines(c(
    "Record ID,Participant ID Number,Participant Age,score",
    "1,KAE-001,18-30,10",
    "2,KAE-002,71+,20.5",
    "3,KAE-003,41-50,"
  ), tf)

  df <- read_participant_attributes(tf)

  expect_identical(
    names(df),
    c("Record ID", "Participant ID Number", "Participant Age", "score")
  )
  expect_true(is.numeric(df$`Record ID`))
  expect_true(is.numeric(df$score))
  expect_identical(df$score, c(10, 20.5, NA))
  expect_true(is.character(df$`Participant Age`))
  expect_identical(df$`Participant ID Number`, c("KAE-001", "KAE-002", "KAE-003"))

  # Detection on this frame picks the participant id column, not Record ID
  res <- match_participant_attributes(c("KAE-001", "KAE-002", "KAE-003"), df)
  expect_identical(res$id_col, "Participant ID Number")
  expect_identical(res$matched, 3L)

  expect_error(read_participant_attributes(tempfile(fileext = ".csv")),
               "not found")
  bad <- tempfile(fileext = ".pdf")
  file.create(bad)
  on.exit(unlink(bad), add = TRUE)
  expect_error(read_participant_attributes(bad), "Unsupported")
})


# contrast_text_color helper


test_that("contrast_text_color picks readable text by luminance", {
  expect_identical(contrast_text_color("#FFFFFF"), "black")
  expect_identical(contrast_text_color("#000000"), "white")
  expect_identical(contrast_text_color("#FFC72C"), "black")
  expect_identical(contrast_text_color("#002554"), "white")
  expect_identical(contrast_text_color(c("#FFFFFF", "#000000")),
                   c("black", "white"))
  # Threshold is adjustable
  expect_identical(contrast_text_color("#808080", threshold = 0.4), "black")
  expect_identical(contrast_text_color("#808080", threshold = 0.9), "white")
})


# Identifier set-aside


test_that("identifier-like columns are set aside, not kept as attributes", {
  ids <- sprintf("KK%02d", 1:14)
  qd <- make_test_qdata(ids)

  attrs <- data.frame(
    sort = ids,
    `Record ID` = 101:114,
    gender = rep(c("Female", "Male"), 7),
    note = paste0("comment ", 1:14),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  rep <- suppressMessages(
    match_participant_attributes(qd@participants, attrs, id_col = "sort")
  )
  expect_setequal(rep$set_aside, c("Record ID", "note"))
  expect_true(any(grepl("Set aside", rep$messages)))
  expect_named(rep$aligned, c("participant", "gender"))

  qd2 <- suppressMessages(
    attach_participant_attributes(qd, attrs, id_col = "sort")
  )
  expect_named(participant_attributes(qd2), c("participant", "gender"))
})

test_that("uniqueness set-aside needs enough rows: tiny panels keep attributes", {
  ids <- c("A1", "B2")
  qd <- make_test_qdata(ids)
  attrs <- data.frame(
    sort = ids,
    gender = c("Male", "Female"),
    stringsAsFactors = FALSE
  )
  rep <- suppressMessages(
    match_participant_attributes(qd@participants, attrs, id_col = "sort")
  )
  expect_length(rep$set_aside, 0)
  expect_true("gender" %in% names(rep$aligned))
})


# reset_attribute_binning


test_that("reset_attribute_binning restores the detected spec", {
  ids <- sprintf("KK%02d", 1:6)
  qd <- make_test_qdata(ids)
  attrs <- data.frame(
    participant = ids,
    age = c("18-30", "31-40", "41-50", "51-60", "61-70", "71+"),
    score = c(10, 20, 30, 40, 50, 60),
    stringsAsFactors = FALSE
  )
  qd <- set_participant_attributes(qd, attrs)

  qd2 <- set_attribute_binning(
    qd, "age",
    map = c("18-30" = "18-40", "31-40" = "18-40", "41-50" = "41+",
            "51-60" = "41+", "61-70" = "41+", "71+" = "41+")
  )
  expect_identical(levels(attribute_groups(qd2)$age), c("18-40", "41+"))

  qd3 <- reset_attribute_binning(qd2, "age")
  expect_null(qd3@metadata$attribute_spec$age$map)
  expect_identical(levels(attribute_groups(qd3)$age),
                   c("18-30", "31-40", "41-50", "51-60", "61-70", "71+"))

  qd4 <- set_attribute_binning(qd, "score", breaks = c(0, 30, 60))
  expect_true(is.factor(attribute_groups(qd4)$score))
  qd5 <- reset_attribute_binning(qd4, "score")
  expect_null(qd5@metadata$attribute_spec$score$breaks)
  expect_true(is.numeric(attribute_groups(qd5)$score))

  expect_error(reset_attribute_binning(qd, "nope"),
               "not a participant attribute")
})


test_that("an NA map target sets a level aside without touching the rest", {
  ids <- sprintf("KK%02d", 1:6)
  qd <- make_test_qdata(ids)
  qd <- set_participant_attributes(qd, data.frame(
    participant = ids,
    age = c("18-30", "18-30", "31-40", "41-50", "41-50", "71+"),
    stringsAsFactors = FALSE
  ))

  qd2 <- set_attribute_binning(qd, "age", map = c(
    "18-30" = NA_character_, "31-40" = "31-40",
    "41-50" = "41-50", "71+" = "71+"
  ))
  g <- attribute_groups(qd2)$age
  expect_identical(levels(g), c("31-40", "41-50", "71+"))
  expect_identical(sum(is.na(g)), 2L)

  # Reset brings the level back
  g3 <- attribute_groups(reset_attribute_binning(qd2, "age"))$age
  expect_identical(levels(g3), c("18-30", "31-40", "41-50", "71+"))
  expect_identical(sum(is.na(g3)), 0L)
})
