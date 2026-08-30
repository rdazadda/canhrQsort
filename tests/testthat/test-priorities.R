# Tests for the priorities compute layer (R/priorities.R)

# ---------------------------------------------------------------------------
# Deterministic fixture: J = 12 statements, C = 5 columns, quotas 1-3-4-3-1,
# N = 24 forced sorts built by rotating a base sort. Statement S1 is planted
# high (value +2) for group A (participants 1-12) and low (value -2) for
# group B (participants 13-24). Sorts are duplicated in pairs so that the
# odd/even "rand" attribute splits into two identical groups (a true null).
# ---------------------------------------------------------------------------

make_fixture <- function() {
  quotas <- c(1, 3, 4, 3, 1)
  base_a <- c(-2, rep(-1, 3), rep(0, 4), rep(1, 3))
  base_b <- c(rep(-1, 3), rep(0, 4), rep(1, 3), 2)
  rot <- function(x, k) {
    k <- k %% length(x)
    if (k == 0) x else c(x[(k + 1):length(x)], x[seq_len(k)])
  }
  sorts <- matrix(NA_real_, nrow = 24, ncol = 12)
  for (i in 1:24) {
    pair <- ceiling(i / 2)
    if (pair <= 6) {
      sorts[i, 1] <- 2
      sorts[i, 2:12] <- rot(base_a, pair - 1)
    } else {
      sorts[i, 1] <- -2
      sorts[i, 2:12] <- rot(base_b, pair - 7)
    }
  }
  suppressMessages(qsort_data(sorts, distribution = quotas, validate = FALSE))
}

fixture_groups <- function() {
  grp <- factor(rep(c("A", "B"), each = 12))
  attr(grp, "type") <- "categorical"
  rand <- factor(rep(c("X", "Y"), times = 12))
  attr(rand, "type") <- "categorical"
  age3 <- factor(c(rep("18-40", 10), rep("41-70", 10), rep("71+", 4)))
  attr(age3, "type") <- "categorical"
  list(grp = grp, rand = rand, age3 = age3)
}

fixture_qdata <- function() {
  qdata <- make_fixture()
  g <- fixture_groups()
  qdata@metadata$participant_attributes <- data.frame(
    participant = qdata@participants,
    grp = as.character(g$grp),
    rand = as.character(g$rand),
    age3 = as.character(g$age3),
    stringsAsFactors = FALSE
  )
  qdata
}

make_mini <- function() {
  # 2 sorts x 3 statements, quotas 1-1-1 (values -1, 0, 1); S2 is constant
  # and every statement mean is 0, so all ranks tie at 1
  suppressMessages(qsort_data(
    matrix(c(-1, 0, 1,
             1, 0, -1), nrow = 2, byrow = TRUE),
    distribution = c(1, 1, 1), validate = FALSE
  ))
}

has_coin <- requireNamespace("coin", quietly = TRUE)
fixture <- fixture_qdata()
fixture_grp <- fixture_groups()
pr <- if (has_coin) {
  compute_priorities(fixture, groups = fixture_grp, q = 0.05,
                     n_perm = 1999, seed = 1)
} else {
  NULL
}


test_that("deck_neutral derives mu0 from the deck", {
  # symmetric decks are neutral at zero
  expect_equal(deck_neutral(c(1, 3, 4, 3, 1)), 0)
  expect_equal(deck_neutral(c(1, 2, 2, 1)), 0)
  # asymmetric decks shift the neutral point
  expect_equal(deck_neutral(c(1, 2, 4, 3, 2)), 0.25)
  expect_equal(deck_neutral(c(1, 2, 3, 2)), 3 / 8)
  expect_error(deck_neutral(c(-1, 2)), "non-negative")
})

test_that("every fixture sort respects the forced distribution", {
  quota_ok <- apply(fixture@sorts, 1, function(r) {
    all(table(factor(r, levels = -2:2)) == c(1, 3, 4, 3, 1))
  })
  expect_true(all(quota_ok))
})

test_that("compute_priorities returns a qsort_priorities object with metadata", {
  skip_if_not_installed("coin")
  expect_s3_class(pr, "qsort_priorities")
  expect_named(pr, c("table", "tests", "gates", "concordance", "meta"))
  expect_equal(pr$meta$n, 24)
  expect_equal(pr$meta$J, 12)
  expect_equal(pr$meta$mu0, 0)
  expect_equal(pr$meta$q, 0.05)
  expect_setequal(pr$meta$families, c("neutral", "grp", "rand", "age3"))
})

test_that("table is ordered by descending mean with min-rank and tie flags", {
  skip_if_not_installed("coin")
  tbl <- pr$table
  expect_equal(nrow(tbl), 12)
  expect_false(is.unsorted(rev(tbl$mean)))
  expect_equal(tbl$rank, rank(-tbl$mean, ties.method = "min"))
  expect_equal(tbl$tied,
               duplicated(tbl$mean) | duplicated(tbl$mean, fromLast = TRUE))
})

test_that("tied means share min-rank and constant statements are guarded", {
  skip_if_not_installed("coin")
  mini <- make_mini()
  res <- compute_priorities(mini, groups = list())
  expect_equal(res$table$rank, c(1L, 1L, 1L))
  expect_true(all(res$table$tied))
  neu <- res$tests[res$tests$attribute == "neutral", ]
  s2 <- neu[neu$stmt == "S2", ]
  expect_true(is.na(s2$p_raw))
  expect_match(s2$note, "no variation, not testable")
  expect_equal(s2$flagged, FALSE)
})

test_that("n_above, n_at, n_below sum to N", {
  skip_if_not_installed("coin")
  sums <- pr$table$n_above + pr$table$n_at + pr$table$n_below
  expect_equal(sums, rep(24L, 12))
})

test_that("the planted group difference is flagged and the null attribute is not", {
  skip_if_not_installed("coin")
  tt <- pr$tests
  s1 <- tt[tt$attribute == "grp" & tt$stmt == "S1", ]
  expect_equal(s1$test, "mann_whitney")
  expect_true(s1$flagged)
  expect_true(s1$q_adj <= 0.05)
  expect_match(s1$direction, "^A higher$")
  expect_equal(s1$effect, 1)          # P(A > B) = 1 for the planted statement
  expect_equal(s1$hl_shift, 4)        # Hodges-Lehmann shift +2 vs -2

  # table label wording for the flagged two-level comparison
  lab <- pr$table$grp[pr$table$stmt == "S1"]
  expect_match(lab, "^A higher \\(P = 1\\.00, q ")

  # the balanced random attribute never flags and its gate fails,
  # so its table cells read as suppressed
  rand_rows <- tt[tt$attribute == "rand", ]
  expect_false(any(rand_rows$flagged))
  expect_true(all(!is.na(rand_rows$q_adj) | is.na(rand_rows$p_raw)))
  expect_true(all(pr$table$rand == "No overall group difference"))
})

test_that("levels below n = 5 are excluded with the right note", {
  skip_if_not_installed("coin")
  age_rows <- pr$tests[pr$tests$attribute == "age3", ]
  expect_equal(nrow(age_rows), 12)
  expect_true(all(grepl("level '71\\+' not compared, n = 4", age_rows$note)))
  # the two remaining usable levels are compared with Mann-Whitney
  expect_true(all(age_rows$test == "mann_whitney"))
  expect_true(all(age_rows$n_levels == 2, na.rm = TRUE))
  expect_true(all(age_rows$n_used <= 20, na.rm = TRUE))
  # the same note also lands on the gate row
  expect_match(pr$gates$note[pr$gates$attribute == "age3"],
               "level '71\\+' not compared, n = 4")
})

test_that("attributes with fewer than 2 usable levels are skipped with a note", {
  skip_if_not_installed("coin")
  lop <- factor(c(rep("big", 21), rep("tiny", 3)))
  attr(lop, "type") <- "categorical"
  res <- compute_priorities(fixture, groups = list(lop = lop), n_perm = 199)
  expect_false("lop" %in% res$tests$attribute)
  expect_false("lop" %in% names(res$table))
  g <- res$gates[res$gates$attribute == "lop", ]
  expect_false(g$pass)
  expect_match(g$note, "fewer than 2 usable levels")
  expect_true(any(grepl("skipped", res$meta$warnings)))
})

test_that("PERMANOVA gates report r2 in (0, 1] and a logical pass", {
  skip_if_not_installed("coin")
  g <- pr$gates
  expect_s3_class(g, "data.frame")
  expect_setequal(g$attribute, c("grp", "rand", "age3"))
  expect_true(is.logical(g$pass))
  expect_true(all(is.na(g$r2) | (g$r2 >= 0 & g$r2 <= 1)))

  grp_gate <- g[g$attribute == "grp", ]
  expect_true(grp_gate$r2 > 0 && grp_gate$r2 <= 1)
  expect_true(grp_gate$p <= 0.05)
  expect_true(grp_gate$pass)

  # identical groups by construction: zero between-group variance, gate fails
  rand_gate <- g[g$attribute == "rand", ]
  expect_equal(rand_gate$r2, 0, tolerance = 1e-12)
  expect_false(rand_gate$pass)
})

test_that("concordance returns tie-corrected W in [0, 1] with bands", {
  skip_if_not_installed("coin")
  cc <- pr$concordance
  expect_true(cc$W >= 0 && cc$W <= 1)
  expect_equal(cc$df, 11L)
  expect_true(cc$band %in% c("very weak", "weak", "moderate", "strong"))
  expect_equal(cc$chisq, 24 * 11 * cc$W, tolerance = 1e-9)

  pl <- cc$per_level
  expect_s3_class(pl, "data.frame")
  expect_true(all(pl$n >= 5))
  expect_false(any(pl$level == "71+"))          # small level skipped
  expect_true(all(pl$W >= 0 & pl$W <= 1, na.rm = TRUE))
})

test_that("ordinal attributes use linear-by-linear and numeric use spearman", {
  skip_if_not_installed("coin")
  ordv <- factor(rep(c("L", "M", "H"), each = 8),
                 levels = c("L", "M", "H"), ordered = TRUE)
  attr(ordv, "type") <- "ordinal"
  numv <- as.numeric(1:24)
  res <- compute_priorities(fixture, groups = list(ord = ordv, num = numv),
                            n_perm = 499)
  ord_rows <- res$tests[res$tests$attribute == "ord", ]
  num_rows <- res$tests[res$tests$attribute == "num", ]
  expect_true(all(ord_rows$test == "linear_by_linear"))
  expect_true(all(num_rows$test == "spearman"))
  expect_true(any(!is.na(ord_rows$p_raw)))
  expect_true(any(!is.na(num_rows$p_raw)))
  expect_true(all(grepl("rho = ", num_rows$effect_label[!is.na(num_rows$effect)])))
  num_gate <- res$gates[res$gates$attribute == "num", ]
  expect_match(num_gate$note, "gate uses a median split")
})

test_that("compute_priorities with no attributes still returns the overall table", {
  skip_if_not_installed("coin")
  res <- compute_priorities(make_fixture(), groups = NULL, n_perm = 199)
  expect_s3_class(res, "qsort_priorities")
  expect_equal(names(res$table), c(
    "rank", "tied", "stmt", "statement", "mean", "sd", "median",
    "n_above", "n_at", "n_below", "hl", "hl_lower", "hl_upper",
    "neutral_label", "neutral_q", "neutral_effect"
  ))
  expect_equal(res$meta$families, "neutral")
  expect_equal(nrow(res$gates), 0)
})

test_that("priorities_top_n returns n rows per level with tie flags", {
  top_tbl <- priorities_top_n(fixture, groups = fixture_grp, n = 5)
  expect_named(top_tbl, c("attribute", "level", "rank", "stmt",
                          "statement", "mean", "tied"))
  # 7 levels: A/B, X/Y, 18-40/41-70/71+ (top-n is descriptive, small levels stay)
  expect_equal(nrow(top_tbl), 5 * 7)
  counts <- table(paste(top_tbl$attribute, top_tbl$level))
  expect_true(all(counts == 5))
  expect_true(all(top_tbl$rank %in% 1:5))
  expect_true(is.logical(top_tbl$tied))

  # subgroup means are ordered within each level block
  by_level <- split(top_tbl, paste(top_tbl$attribute, top_tbl$level))
  expect_true(all(vapply(by_level, function(d) {
    !is.unsorted(rev(d$mean[order(d$rank)]))
  }, logical(1))))

  # bottom direction: lowest means first
  bot <- priorities_top_n(fixture, groups = fixture_grp["grp"], n = 3,
                          direction = "bottom")
  expect_equal(nrow(bot), 6)
  a_bot <- bot[bot$level == "A", ]
  expect_false(is.unsorted(a_bot$mean[order(a_bot$rank)]))
})

test_that("tie runs in the top block keep stable order and flag every rank", {
  g1 <- factor(c("A", "A"))
  attr(g1, "type") <- "categorical"
  tiny <- priorities_top_n(make_mini(), groups = list(g = g1), n = 2)
  # all three statement means are 0: rank 2 ties the unseen rank 3 too
  expect_equal(tiny$stmt, c("S1", "S2"))   # ties.method = "first" stability
  expect_true(all(tiny$tied))
})

test_that("plot_priorities_flow builds a ggplot in the prototype grammar", {
  skip_if_not_installed("coin")
  skip_if_not_installed("ggalluvial")
  top_tbl <- priorities_top_n(fixture, groups = fixture_grp[c("grp", "age3")],
                              n = 4)
  p <- plot_priorities_flow(
    top_tbl,
    overall_means = stats::setNames(pr$table$mean, pr$table$stmt),
    gates = pr$gates
  )
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))

  # a gate-failed attribute adds the descriptive caption
  top_rand <- priorities_top_n(fixture, groups = fixture_grp["rand"], n = 3)
  p2 <- plot_priorities_flow(top_rand, gates = pr$gates)
  expect_match(p2$labels$caption, "are descriptive: no overall group difference")
  expect_no_error(ggplot2::ggplot_build(p2))
})

test_that("write_priorities_workbook writes the expected sheets", {
  skip_if_not_installed("coin")
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("ggalluvial")
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  suppressMessages(
    write_priorities_workbook(fixture, f, priorities = pr,
                              groups = fixture_grp, top_n = 3)
  )
  expect_true(file.exists(f))
  expect_equal(
    openxlsx::getSheetNames(f),
    c("Ranked", "Group comparisons", "Top by group", "Attributes",
      "Priorities ranking", "Panel composition", "Priorities flow")
  )
})

test_that("misaligned groups abort with an informative error", {
  skip_if_not_installed("coin")
  short <- factor(rep("A", 5))
  attr(short, "type") <- "categorical"
  expect_error(
    compute_priorities(fixture, groups = list(bad = short), n_perm = 99),
    "align with participants"
  )
  expect_error(
    compute_priorities(fixture, groups = list(unname_me = NULL, 1:24)),
    "named"
  )
})
