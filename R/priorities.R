#' @title Priorities Compute Layer
#' @description Ranked priorities for Q-sort data: deck-derived neutral point,
#'   per-statement location tests against neutral, nonparametric subgroup
#'   comparisons with attribute-level PERMANOVA gates, Kendall's W concordance,
#'   top-n rankings per group level, an alluvial flow chart, and an Excel
#'   workbook export. Generalizes the Kake ranked-priorities analysis.
#' @name priorities
NULL


# Internal helpers ----

# Core (non-attribute) columns of the priorities table, in display order
.priorities_core_cols <- c(
  "rank", "tied", "stmt", "statement", "mean", "sd", "median",
  "n_above", "n_at", "n_below", "hl", "hl_lower", "hl_upper",
  "neutral_label", "neutral_q", "neutral_effect"
)

#' Require an Optional Package for the Priorities Layer
#' @keywords internal
#' @noRd
priorities_require <- function(pkg, purpose) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    rlang::abort(c(
      paste0("Package '", pkg, "' is required ", purpose),
      "i" = paste0("Install with: install.packages(\"", pkg, "\")")
    ))
  }
  invisible(TRUE)
}

#' Deck Column Values Centered on Zero
#' @description Column values for a C-column deck: if C is odd,
#'   -(C-1)/2 .. (C-1)/2; if C is even, -C/2 .. -1, 1 .. C/2.
#' @keywords internal
#' @noRd
deck_values <- function(n_cols) {
  if (n_cols %% 2 == 1) {
    seq(-(n_cols - 1) / 2, (n_cols - 1) / 2)
  } else {
    c(seq(-n_cols / 2, -1), seq(1, n_cols / 2))
  }
}

#' Deck-Derived Neutral Point
#'
#' @description The quota-weighted mean of the deck column values, used as the
#'   neutral reference mu0 for the vs-neutral tests. Column values are the
#'   zero-centered sequence for the deck (odd C: -(C-1)/2 .. (C-1)/2; even C:
#'   -C/2 .. -1, 1 .. C/2); for a symmetric distribution mu0 is 0, for
#'   asymmetric quotas it shifts accordingly.
#'
#' @param distribution Numeric vector of column quotas (the forced
#'   distribution), one entry per deck column.
#'
#' @return A single numeric value, mu0.
#' @keywords internal
deck_neutral <- function(distribution) {
  if (!is.numeric(distribution) || length(distribution) < 1 ||
      anyNA(distribution) || any(distribution < 0)) {
    rlang::abort(c(
      "`distribution` must be a numeric vector of non-negative column quotas",
      "i" = "Example: c(1, 3, 4, 3, 1)"
    ))
  }
  if (sum(distribution) == 0) {
    rlang::abort("`distribution` quotas sum to zero; the deck has no cards")
  }
  values <- deck_values(length(distribution))
  sum(values * distribution) / sum(distribution)
}

#' Format an Adjusted q Value for Labels
#' @keywords internal
#' @noRd
priorities_fmt_q <- function(qv) {
  if (is.na(qv)) return("q = NA")
  if (qv < 0.001) return("q < 0.001")
  if (qv < 0.01) return(sprintf("q = %.3f", qv))
  sprintf("q = %.2f", qv)
}

#' Benjamini-Hochberg Adjustment Over the Non-Missing Subset
#' @keywords internal
#' @noRd
priorities_bh <- function(p) {
  q <- rep(NA_real_, length(p))
  ok <- !is.na(p)
  if (any(ok)) q[ok] <- stats::p.adjust(p[ok], method = "BH")
  q
}

#' Matched-Pairs Rank-Biserial Correlation (Pratt Ranking)
#' @description (T+ - T-) / (T+ + T-) where |differences| are ranked with
#'   zeros included (Pratt) and zero differences contribute to neither sum.
#' @keywords internal
#' @noRd
rank_biserial_pratt <- function(x, mu0) {
  d <- x - mu0
  r <- rank(abs(d))
  t_pos <- sum(r[d > 0])
  t_neg <- sum(r[d < 0])
  if (t_pos + t_neg == 0) return(NA_real_)
  (t_pos - t_neg) / (t_pos + t_neg)
}

#' Probability of Superiority From Mean Ranks
#' @description PS = (U + 0.5 * ties) / (n1 * n2), computed directly from the
#'   joint mid-ranks so ties are split evenly.
#' @keywords internal
#' @noRd
prob_superiority <- function(x1, x2) {
  n1 <- length(x1)
  n2 <- length(x2)
  if (n1 < 1 || n2 < 1) return(NA_real_)
  r <- rank(c(x1, x2))
  u1 <- sum(r[seq_len(n1)]) - n1 * (n1 + 1) / 2
  u1 / (n1 * n2)
}

#' Two-Sample Hodges-Lehmann Shift With Confidence Interval
#' @description `exact = FALSE`: forced Q-sorts always carry heavy ties, and
#'   the exact tied CI search in R >= 4.4 is orders of magnitude slower for
#'   an interval that is descriptive here.
#' @keywords internal
#' @noRd
hl_two_sample <- function(x1, x2) {
  res <- tryCatch(
    suppressWarnings(stats::wilcox.test(x1, x2, conf.int = TRUE,
                                        exact = FALSE)),
    error = function(e) NULL
  )
  if (is.null(res)) {
    res <- tryCatch(
      suppressWarnings(stats::wilcox.test(x1, x2, conf.int = TRUE)),
      error = function(e) NULL
    )
  }
  if (is.null(res) || is.null(res$estimate)) {
    return(c(NA_real_, NA_real_, NA_real_))
  }
  c(unname(res$estimate), res$conf.int[1], res$conf.int[2])
}

#' One-Sample Hodges-Lehmann Pseudo-Median With Confidence Interval
#' @keywords internal
#' @noRd
hl_one_sample <- function(x, mu0) {
  x <- x[!is.na(x)]
  res <- tryCatch(
    suppressWarnings(stats::wilcox.test(x, mu = mu0, conf.int = TRUE,
                                        exact = FALSE)),
    error = function(e) NULL
  )
  if (is.null(res)) {
    res <- tryCatch(
      suppressWarnings(stats::wilcox.test(x, mu = mu0, conf.int = TRUE)),
      error = function(e) NULL
    )
  }
  if (is.null(res) || is.null(res$estimate)) {
    return(c(NA_real_, NA_real_, NA_real_))
  }
  c(unname(res$estimate), res$conf.int[1], res$conf.int[2])
}

#' Kendall's W Band Label
#' @keywords internal
#' @noRd
priorities_w_band <- function(w) {
  if (is.na(w)) return(NA_character_)
  if (w < 0.1) return("very weak")
  if (w < 0.3) return("weak")
  if (w < 0.5) return("moderate")
  "strong"
}

#' Tie-Corrected Kendall's W Across Sorts
#' @description Sorters rank the statements within their own sort; W is
#'   computed from the column sums of those within-sort ranks with the
#'   standard tie correction. Under a forced distribution the tie correction
#'   is identical for every sorter (one tie group per deck column).
#' @keywords internal
#' @noRd
kendall_w_stats <- function(X) {
  ok <- stats::complete.cases(X)
  m <- sum(ok)
  J <- ncol(X)
  out <- list(W = NA_real_, chisq = NA_real_, df = NA_integer_,
              p = NA_real_, band = NA_character_, n_used = m)
  if (m < 2 || J < 2) return(out)
  R <- t(apply(X[ok, , drop = FALSE], 1, rank))
  Rj <- colSums(R)
  S <- sum((Rj - mean(Rj))^2)
  tie_sum <- sum(apply(R, 1, function(r) {
    tt <- table(r)
    sum(tt^3 - tt)
  }))
  denom <- m^2 * (J^3 - J) - m * tie_sum
  if (denom <= 0) return(out)
  W <- 12 * S / denom
  chisq <- m * (J - 1) * W
  df <- J - 1
  list(
    W = W, chisq = chisq, df = as.integer(df),
    p = stats::pchisq(chisq, df, lower.tail = FALSE),
    band = priorities_w_band(W), n_used = m
  )
}

#' PERMANOVA Pseudo-F Gate on Euclidean Distances Between Sorts
#' @description Between/within group sums of squares of the raw sort rows,
#'   pseudo-F, R2 = SSB/SST, and a permutation p value from `n_perm` label
#'   permutations. Assumes the RNG has already been seeded by the caller.
#' @keywords internal
#' @noRd
permanova_euclidean <- function(X, g, n_perm = 9999) {
  N <- nrow(X)
  a <- nlevels(g)
  D2 <- as.matrix(stats::dist(X))^2
  if (anyNA(D2)) {
    return(list(r2 = NA_real_, p = NA_real_, f = NA_real_, n_used = N,
                note = "distances not computable (missing values)"))
  }
  sst <- sum(D2) / (2 * N)
  if (sst <= 0) {
    return(list(r2 = NA_real_, p = NA_real_, f = NA_real_, n_used = N,
                note = "all sorts identical; gate not computable"))
  }
  glab <- as.integer(g)
  n_l <- tabulate(glab, nbins = a)
  ssw <- sum(vapply(seq_len(a), function(k) {
    idx <- which(glab == k)
    sum(D2[idx, idx]) / (2 * length(idx))
  }, numeric(1)))
  ssb <- sst - ssw
  f_obs <- if (ssw > 0) (ssb / (a - 1)) / (ssw / (N - a)) else Inf
  # Vectorized label permutations: one column per permutation, within-group
  # sums via BLAS (z' D2 z for the 0/1 membership of each level)
  perm <- replicate(n_perm, sample.int(N))
  G <- matrix(glab[perm], nrow = N, ncol = n_perm)
  ssw_p <- numeric(n_perm)
  for (k in seq_len(a)) {
    Z <- G == k
    ssw_p <- ssw_p + colSums((D2 %*% Z) * Z) / (2 * n_l[k])
  }
  f_p <- ifelse(ssw_p > 0,
                ((sst - ssw_p) / (a - 1)) / (ssw_p / (N - a)),
                Inf)
  list(
    r2 = ssb / sst,
    p = (sum(f_p >= f_obs) + 1) / (n_perm + 1),
    f = f_obs,
    n_used = N,
    note = NA_character_
  )
}

#' Dunn Post Hoc: Top Significant Contrast
#' @description Manual Dunn test on the joint ranks with the standard tie
#'   correction; pairwise p values are BH-adjusted across the pairwise set.
#'   Returns NULL when no contrast survives, otherwise the winning level of
#'   the most significant contrast.
#' @keywords internal
#' @noRd
dunn_top_contrast <- function(v, g, q_threshold) {
  N <- length(v)
  r <- rank(v)
  tab <- table(g)
  levs <- names(tab)
  if (length(levs) < 2) return(NULL)
  rbar <- tapply(r, g, mean)
  tie_tab <- table(v)
  tie_corr <- sum(tie_tab^3 - tie_tab) / (12 * (N - 1))
  base_var <- N * (N + 1) / 12 - tie_corr
  if (base_var <= 0) return(NULL)
  pairs <- utils::combn(levs, 2)
  n_pair <- ncol(pairs)
  z <- numeric(n_pair)
  p <- numeric(n_pair)
  for (k in seq_len(n_pair)) {
    i <- pairs[1, k]
    j <- pairs[2, k]
    se <- sqrt(base_var * (1 / tab[[i]] + 1 / tab[[j]]))
    z[k] <- (rbar[[i]] - rbar[[j]]) / se
    p[k] <- 2 * stats::pnorm(-abs(z[k]))
  }
  p_adj <- stats::p.adjust(p, method = "BH")
  sig <- which(p_adj <= q_threshold)
  if (length(sig) == 0) return(NULL)
  k <- sig[which.min(p_adj[sig])]
  winner <- if (z[k] > 0) pairs[1, k] else pairs[2, k]
  list(winner = winner, q = p_adj[k])
}

#' Resolve the Groups Argument Against the Attributes Module
#' @description NULL delegates to `attribute_groups()` when the attributes
#'   module is loaded and the data carries attributes; otherwise an empty
#'   list. An explicit named list is validated for alignment.
#' @keywords internal
#' @noRd
resolve_priorities_groups <- function(qdata, groups) {
  if (is.null(groups)) {
    has_fun <- get0("has_participant_attributes", mode = "function")
    grp_fun <- get0("attribute_groups", mode = "function")
    if (!is.null(has_fun) && !is.null(grp_fun) && isTRUE(has_fun(qdata))) {
      groups <- grp_fun(qdata)
    } else {
      groups <- list()
    }
  }
  if (!is.list(groups)) {
    rlang::abort(c(
      "`groups` must be NULL or a named list of participant attributes",
      "i" = "Use the shape returned by attribute_groups(): factors or numeric vectors aligned to participants"
    ))
  }
  groups <- groups[!vapply(groups, is.null, logical(1))]
  if (length(groups) > 0) {
    if (is.null(names(groups)) || any(!nzchar(names(groups)))) {
      rlang::abort("Every entry of `groups` must be named after its attribute")
    }
    n <- length(qdata@participants)
    lens <- vapply(groups, length, integer(1))
    bad <- names(groups)[lens != n]
    if (length(bad) > 0) {
      rlang::abort(c(
        "Group attributes must align with participants",
        "x" = paste0(
          "Attribute(s) ", paste0("'", bad, "'", collapse = ", "),
          " do not have length ", n
        )
      ))
    }
  }
  groups
}

#' Prepare One Attribute for Testing
#' @keywords internal
#' @noRd
prep_priorities_attribute <- function(a, name) {
  if (is.numeric(a) && !is.factor(a)) {
    return(list(
      kind = "numeric", type = "numeric", values = as.numeric(a),
      usable_levels = character(0), notes = character(0),
      skip = FALSE, skip_note = NULL
    ))
  }
  if (is.character(a) || is.logical(a)) a <- factor(a)
  if (!is.factor(a)) {
    rlang::abort(paste0(
      "Attribute '", name, "' must be a factor or a numeric vector"
    ))
  }
  type <- attr(a, "type") %||% if (is.ordered(a)) "ordinal" else "categorical"
  counts <- table(a[!is.na(a)])
  counts <- counts[counts > 0]
  small <- counts[counts < 5]
  notes <- if (length(small) > 0) {
    sprintf("level '%s' not compared, n = %d", names(small), as.integer(small))
  } else {
    character(0)
  }
  usable <- names(counts)[counts >= 5]
  skip <- length(usable) < 2
  list(
    kind = "factor", type = type, values = a,
    usable_levels = usable, notes = notes, skip = skip,
    skip_note = if (skip) {
      sprintf("attribute '%s' skipped: fewer than 2 usable levels (n >= 5)", name)
    } else {
      NULL
    }
  )
}

#' Vs-Neutral Wilcoxon Signed-Rank (Pratt) Test for One Statement
#' @keywords internal
#' @noRd
neutral_test_one <- function(x, mu0) {
  x <- x[!is.na(x)]
  n_used <- length(x)
  out <- list(statistic = NA_real_, p = NA_real_, effect = NA_real_,
              n_used = n_used, note = NA_character_)
  if (n_used < 2) {
    out$note <- "fewer than 2 observations, not testable"
    return(out)
  }
  if (length(unique(x)) < 2) {
    out$note <- "no variation, not testable"
    return(out)
  }
  n_nonzero <- sum(x != mu0)
  dist_kind <- if (n_nonzero <= 25) "exact" else "asymptotic"
  dd <- data.frame(x = x, y = rep(mu0, n_used))
  res <- tryCatch(
    coin::wilcoxsign_test(x ~ y, data = dd, zero.method = "Pratt",
                          distribution = dist_kind),
    error = function(e) NULL
  )
  if (is.null(res) && identical(dist_kind, "exact")) {
    res <- tryCatch(
      coin::wilcoxsign_test(x ~ y, data = dd, zero.method = "Pratt",
                            distribution = "asymptotic"),
      error = function(e) NULL
    )
  }
  if (is.null(res)) {
    out$note <- "vs-neutral test failed"
    return(out)
  }
  out$statistic <- as.numeric(coin::statistic(res))
  out$p <- as.numeric(coin::pvalue(res))
  out$effect <- rank_biserial_pratt(x, mu0)
  out
}


# Compute priorities ----

#' Compute Ranked Priorities With Subgroup Tests
#'
#' @description
#' Builds the full priorities layer for a Q-sort dataset: a ranked table of
#' statement means with vs-neutral tests, per-attribute nonparametric
#' subgroup comparisons guarded by an attribute-level PERMANOVA gate,
#' Kendall's W concordance overall and per group level, and metadata.
#'
#' Test engine, per family:
#' * Vs neutral: `coin::wilcoxsign_test()` against the deck neutral mu0 with
#'   `zero.method = "Pratt"`; exact distribution when at most 25 values differ
#'   from mu0, asymptotic otherwise. Effect is the matched-pairs rank-biserial
#'   correlation.
#' * Two levels: `coin::wilcox_test()`; exact when the smaller group has fewer
#'   than 10 members (falling back to `approximate(nresample = 100000)` if the
#'   exact distribution is infeasible), asymptotic otherwise. Effect is the
#'   probability of superiority computed from joint mid-ranks; the
#'   Hodges-Lehmann shift and interval come from `stats::wilcox.test()`.
#' * Three or more unordered levels: `coin::kruskal_test()`, using
#'   `approximate(nresample = 100000)` when any included level has 5 to 9
#'   members. Effect is epsilon squared (statistic / (n_used - 1)); direction
#'   is only reported after a manual Dunn post hoc (BH across the pairwise
#'   set) confirms a top contrast.
#' * Ordered factors with three or more levels: `coin::lbl_test()`
#'   (linear-by-linear association, raw sort values as response scores);
#'   direction follows the sign of the standardized statistic.
#' * Numeric attributes: `stats::cor.test(method = "spearman", exact = FALSE)`;
#'   effect is rho.
#'
#' Guards: attribute levels with fewer than 5 members are excluded from
#' testing and noted; attributes left with fewer than 2 usable levels are
#' skipped; missing attribute values are excluded per test with `n_used`
#' recorded; constant statement vectors are marked "no variation, not
#' testable". BH false discovery rate adjustment is applied within each family
#' separately (vs neutral is one family, each attribute is one family).
#'
#' The gate per attribute is a whole-sort PERMANOVA on Euclidean distances
#' between sort vectors (pseudo-F, `n_perm` label permutations,
#' R2 = SSB/SST); numeric attributes are median-split solely for the gate.
#' When a gate fails (p > 0.05) the attribute's table cells read "No overall
#' group difference" and per-statement flags are suppressed, though adjusted
#' q values are kept in `$tests`.
#'
#' @param qdata A `QsortData` object.
#' @param groups NULL to use `attribute_groups(qdata)` when participant
#'   attributes exist (an empty set otherwise), or a named list in the shape
#'   `attribute_groups()` returns: per attribute either a factor aligned to
#'   participants (with `attr(x, "type")` of "categorical" or "ordinal",
#'   ordered factors for ordinal) or a numeric vector.
#' @param q False discovery rate threshold for flagging (default 0.05).
#' @param n_perm Number of label permutations for the PERMANOVA gate
#'   (default 9999).
#' @param seed Fixed seed for the gate permutations and any Monte Carlo
#'   test distributions (default 1). The caller's RNG state is restored.
#'
#' @return An object of class `"qsort_priorities"`: a list with elements
#'   `$table` (ranked statement table with one label column per tested
#'   attribute), `$tests` (long per-statement test results), `$gates`
#'   (per-attribute PERMANOVA gate), `$concordance` (Kendall's W overall and
#'   `$per_level`), and `$meta` (n, J, mu0, q, timestamp, warnings, families).
#' @export
#'
#' @examples
#' \dontrun{
#' pr <- compute_priorities(qdata, groups = attribute_groups(qdata))
#' head(pr$table)
#' pr$gates
#' }
compute_priorities <- function(qdata, groups = NULL, q = 0.05,
                               n_perm = 9999, seed = 1) {

  if (!inherits(qdata, "QsortData")) {
    rlang::abort("qdata must be a QsortData object")
  }
  if (!is.numeric(q) || length(q) != 1 || is.na(q) || q <= 0 || q >= 1) {
    rlang::abort("`q` must be a single number strictly between 0 and 1")
  }
  if (!is.numeric(n_perm) || length(n_perm) != 1 || is.na(n_perm) || n_perm < 1) {
    rlang::abort("`n_perm` must be a single positive number")
  }
  n_perm <- as.integer(n_perm)
  priorities_require("coin", "for the priorities tests")

  X <- qdata@sorts
  N <- nrow(X)
  J <- ncol(X)
  if (N < 2 || J < 2) {
    rlang::abort("compute_priorities() needs at least 2 sorts and 2 statements")
  }

  warns <- character(0)
  mu0 <- deck_neutral(qdata@distribution)

  if (sum(qdata@distribution) != J) {
    warns <- c(warns, sprintf(
      "distribution quotas sum to %d but there are %d statements",
      as.integer(sum(qdata@distribution)), J
    ))
  }
  obs_vals <- unique(as.vector(X[!is.na(X)]))
  if (length(obs_vals) > 0 &&
      !all(obs_vals %in% deck_values(length(qdata@distribution)))) {
    warns <- c(warns, paste0(
      "sort values do not match the zero-centered deck column values; ",
      "vs-neutral tests use mu0 = ", format(mu0)
    ))
  }

  groups <- resolve_priorities_groups(qdata, groups)

  # Preserve the caller's RNG state; seed our permutations and Monte Carlo
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    old_seed <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  } else {
    on.exit(
      suppressWarnings(rm(".Random.seed", envir = globalenv())),
      add = TRUE
    )
  }
  set.seed(seed)

  stmt_ids <- paste0("S", seq_len(J))
  stmt_text <- if (length(qdata@statements) == J) {
    as.character(qdata@statements)
  } else {
    stmt_ids
  }

  # Core per-statement descriptives ------------------------------------------
  means <- colMeans(X, na.rm = TRUE)
  sds <- apply(X, 2, stats::sd, na.rm = TRUE)
  meds <- apply(X, 2, stats::median, na.rm = TRUE)
  n_above <- colSums(X > mu0, na.rm = TRUE)
  n_at <- colSums(X == mu0, na.rm = TRUE)
  n_below <- colSums(X < mu0, na.rm = TRUE)
  hl_mat <- t(vapply(seq_len(J), function(j) hl_one_sample(X[, j], mu0),
                     numeric(3)))

  # Vs-neutral family ---------------------------------------------------------
  neutral <- lapply(seq_len(J), function(j) neutral_test_one(X[, j], mu0))
  p_neutral <- vapply(neutral, function(r) r$p, numeric(1))
  q_neutral <- priorities_bh(p_neutral)
  flag_neutral <- !is.na(q_neutral) & q_neutral <= q

  neutral_dir_val <- vapply(seq_len(J), function(j) {
    eff <- neutral[[j]]$effect
    if (!is.na(eff) && eff != 0) eff else means[j] - mu0
  }, numeric(1))
  neutral_label <- ifelse(
    flag_neutral,
    ifelse(neutral_dir_val > 0, "Prioritized", "Deprioritized"),
    "No evidence"
  )

  tests_rows <- list()
  tests_rows[["neutral"]] <- data.frame(
    stmt = stmt_ids,
    attribute = "neutral",
    test = "wilcoxon_pratt",
    statistic = vapply(neutral, function(r) r$statistic, numeric(1)),
    p_raw = p_neutral,
    q_adj = q_neutral,
    flagged = flag_neutral,
    direction = ifelse(
      is.na(p_neutral), NA_character_,
      ifelse(neutral_dir_val > 0, "above neutral",
             ifelse(neutral_dir_val < 0, "below neutral", NA_character_))
    ),
    effect = vapply(neutral, function(r) r$effect, numeric(1)),
    effect_label = ifelse(
      is.na(vapply(neutral, function(r) r$effect, numeric(1))),
      NA_character_,
      sprintf("rank-biserial = %.2f",
              vapply(neutral, function(r) r$effect, numeric(1)))
    ),
    hl_shift = hl_mat[, 1] - mu0,
    hl_lower = hl_mat[, 2] - mu0,
    hl_upper = hl_mat[, 3] - mu0,
    n_used = vapply(neutral, function(r) r$n_used, numeric(1)),
    n_levels = NA_integer_,
    note = vapply(neutral, function(r) r$note, character(1)),
    stringsAsFactors = FALSE
  )

  # Attributes: gates, per-statement tests, label columns ---------------------
  gate_rows <- list()
  label_cols <- list()
  families <- "neutral"

  for (name in names(groups)) {
    prep <- prep_priorities_attribute(groups[[name]], name)

    if (prep$kind == "factor" && prep$skip) {
      note_all <- paste(c(prep$notes, prep$skip_note), collapse = "; ")
      gate_rows[[name]] <- data.frame(
        attribute = name, r2 = NA_real_, p = NA_real_, pass = FALSE,
        n_used = sum(!is.na(prep$values)), note = note_all,
        stringsAsFactors = FALSE
      )
      warns <- c(warns, prep$skip_note)
      next
    }

    # Gate ---------------------------------------------------------------
    gate_notes <- prep$notes
    if (prep$kind == "numeric") {
      gate_notes <- c(gate_notes, "gate uses a median split")
      a_num <- prep$values
      ok <- !is.na(a_num)
      med <- stats::median(a_num[ok])
      gate_g <- factor(ifelse(a_num[ok] <= med, "low", "high"),
                       levels = c("low", "high"))
      gate_rows_idx <- which(ok)
    } else {
      a_chr <- as.character(prep$values)
      ok <- !is.na(a_chr) & a_chr %in% prep$usable_levels
      gate_g <- factor(a_chr[ok], levels = prep$usable_levels)
      gate_rows_idx <- which(ok)
    }
    gate_tab <- table(gate_g)
    if (nlevels(gate_g) < 2 || any(gate_tab < 2)) {
      gate <- list(r2 = NA_real_, p = NA_real_, n_used = length(gate_rows_idx),
                   note = "gate not computable (degenerate grouping)")
    } else {
      gate <- permanova_euclidean(X[gate_rows_idx, , drop = FALSE], gate_g,
                                  n_perm = n_perm)
    }
    if (!is.na(gate$note %||% NA_character_)) {
      gate_notes <- c(gate_notes, gate$note)
    }
    gate_pass <- !is.na(gate$p) && gate$p <= 0.05
    gate_rows[[name]] <- data.frame(
      attribute = name, r2 = gate$r2, p = gate$p, pass = gate_pass,
      n_used = gate$n_used,
      note = if (length(gate_notes) > 0) {
        paste(gate_notes, collapse = "; ")
      } else {
        NA_character_
      },
      stringsAsFactors = FALSE
    )

    # Per-statement tests --------------------------------------------------
    if (prep$kind == "numeric") {
      att <- test_attribute_numeric(name, prep$values, X, stmt_ids, q,
                                    gate_pass, prep$notes)
    } else {
      att <- test_attribute_factor(name, prep, X, stmt_ids, q, gate_pass)
    }
    if (is.null(att)) {
      # Attribute became untestable at test time; record a warning
      warns <- c(warns, sprintf("attribute '%s' skipped: not testable", name))
      next
    }
    tests_rows[[name]] <- att$rows
    label_cols[[name]] <- att$labels
    families <- c(families, name)
  }

  tests <- do.call(rbind, tests_rows)
  rownames(tests) <- NULL

  gates <- if (length(gate_rows) > 0) {
    out <- do.call(rbind, gate_rows)
    rownames(out) <- NULL
    out
  } else {
    data.frame(
      attribute = character(0), r2 = numeric(0), p = numeric(0),
      pass = logical(0), n_used = numeric(0), note = character(0),
      stringsAsFactors = FALSE
    )
  }

  # Ranked table ---------------------------------------------------------------
  ord <- order(-means, seq_len(J))
  tbl <- data.frame(
    rank = rank(-means, ties.method = "min")[ord],
    tied = (duplicated(means) | duplicated(means, fromLast = TRUE))[ord],
    stmt = stmt_ids[ord],
    statement = stmt_text[ord],
    mean = means[ord],
    sd = sds[ord],
    median = meds[ord],
    n_above = as.integer(n_above[ord]),
    n_at = as.integer(n_at[ord]),
    n_below = as.integer(n_below[ord]),
    hl = hl_mat[ord, 1],
    hl_lower = hl_mat[ord, 2],
    hl_upper = hl_mat[ord, 3],
    neutral_label = neutral_label[ord],
    neutral_q = q_neutral[ord],
    neutral_effect = vapply(neutral, function(r) r$effect, numeric(1))[ord],
    stringsAsFactors = FALSE
  )
  rownames(tbl) <- NULL
  for (name in names(label_cols)) {
    tbl[[name]] <- label_cols[[name]][ord]
  }
  names(tbl) <- make.unique(names(tbl))

  # Concordance ----------------------------------------------------------------
  w_all <- kendall_w_stats(X)
  if (w_all$n_used < N) {
    warns <- c(warns, sprintf(
      "%d sort(s) with missing values excluded from Kendall's W",
      N - w_all$n_used
    ))
  }
  per_level <- list()
  for (name in names(groups)) {
    a <- groups[[name]]
    if (is.numeric(a) && !is.factor(a)) next
    if (is.character(a) || is.logical(a)) a <- factor(a)
    if (!is.factor(a)) next
    counts <- table(a[!is.na(a)])
    counts <- counts[counts >= 5]
    for (lev in names(counts)) {
      idx <- which(!is.na(a) & as.character(a) == lev)
      wl <- kendall_w_stats(X[idx, , drop = FALSE])
      per_level[[length(per_level) + 1]] <- data.frame(
        attribute = name, level = lev, n = length(idx),
        W = wl$W, chisq = wl$chisq, df = wl$df, p = wl$p, band = wl$band,
        stringsAsFactors = FALSE
      )
    }
  }
  per_level_df <- if (length(per_level) > 0) {
    out <- do.call(rbind, per_level)
    rownames(out) <- NULL
    out
  } else {
    data.frame(
      attribute = character(0), level = character(0), n = integer(0),
      W = numeric(0), chisq = numeric(0), df = integer(0), p = numeric(0),
      band = character(0), stringsAsFactors = FALSE
    )
  }

  structure(
    list(
      table = tbl,
      tests = tests,
      gates = gates,
      concordance = list(
        W = w_all$W, chisq = w_all$chisq, df = w_all$df, p = w_all$p,
        band = w_all$band, per_level = per_level_df
      ),
      meta = list(
        n = N, J = J, mu0 = mu0, q = q,
        timestamp = Sys.time(),
        warnings = warns,
        families = families
      )
    ),
    class = "qsort_priorities"
  )
}


#' Per-Statement Tests for a Factor Attribute
#' @keywords internal
#' @noRd
test_attribute_factor <- function(name, prep, X, stmt_ids, q, gate_pass) {
  J <- ncol(X)
  usable <- prep$usable_levels
  is_ordinal <- identical(prep$type, "ordinal") && length(usable) > 2
  engine <- if (length(usable) == 2) {
    "mann_whitney"
  } else if (is_ordinal) {
    "linear_by_linear"
  } else {
    "kruskal_wallis"
  }
  a_chr <- as.character(prep$values)
  attr_note <- if (length(prep$notes) > 0) {
    paste(prep$notes, collapse = "; ")
  } else {
    NA_character_
  }

  res <- vector("list", J)
  for (j in seq_len(J)) {
    x <- X[, j]
    mask <- !is.na(x) & !is.na(a_chr) & a_chr %in% usable
    v <- x[mask]
    g <- factor(a_chr[mask], levels = usable, ordered = is_ordinal)
    g <- droplevels(g)
    r <- list(statistic = NA_real_, p = NA_real_, effect = NA_real_,
              effect_label = NA_character_, direction = NA_character_,
              hl_shift = NA_real_, hl_lower = NA_real_, hl_upper = NA_real_,
              n_used = length(v), n_levels = nlevels(g),
              note = NA_character_, winner = NA_character_,
              ps = NA_real_, z_sign = NA_real_)

    if (length(v) < 2 || length(unique(v)) < 2) {
      r$note <- "no variation, not testable"
    } else if (nlevels(g) < 2) {
      r$note <- "fewer than 2 groups with data, not compared"
    } else if (engine == "mann_whitney" || nlevels(g) == 2) {
      lv <- levels(g)
      x1 <- v[g == lv[1]]
      x2 <- v[g == lv[2]]
      dist_kind <- if (min(length(x1), length(x2)) < 10) "exact" else "asymptotic"
      dd <- data.frame(v = v, g = factor(as.character(g), levels = lv))
      fit <- tryCatch(
        coin::wilcox_test(v ~ g, data = dd, distribution = dist_kind),
        error = function(e) NULL
      )
      if (is.null(fit) && identical(dist_kind, "exact")) {
        fit <- tryCatch(
          coin::wilcox_test(v ~ g, data = dd,
                            distribution = coin::approximate(nresample = 100000)),
          error = function(e) NULL
        )
      }
      if (is.null(fit)) {
        r$note <- "two-level test failed"
      } else {
        ps <- prob_superiority(x1, x2)
        hl <- hl_two_sample(x1, x2)
        r$statistic <- as.numeric(coin::statistic(fit))
        r$p <- as.numeric(coin::pvalue(fit))
        r$ps <- ps
        r$effect <- ps
        r$effect_label <- sprintf("P(%s > %s) = %.2f", lv[1], lv[2], ps)
        r$hl_shift <- hl[1]
        r$hl_lower <- hl[2]
        r$hl_upper <- hl[3]
        if (!is.na(ps) && ps != 0.5) {
          r$winner <- if (ps > 0.5) lv[1] else lv[2]
          r$direction <- paste(r$winner, "higher")
        }
      }
      if (engine != "mann_whitney") {
        r$note <- paste(
          stats::na.omit(c(r$note, "reduced to 2 groups with data")),
          collapse = "; "
        )
      }
    } else if (engine == "kruskal_wallis" ||
               (engine == "linear_by_linear" && nlevels(g) > 2 && !is.ordered(g))) {
      tab <- table(g)
      dist_obj <- if (any(tab < 10)) {
        coin::approximate(nresample = 100000)
      } else {
        "asymptotic"
      }
      dd <- data.frame(v = v, g = g)
      fit <- tryCatch(
        coin::kruskal_test(v ~ g, data = dd, distribution = dist_obj),
        error = function(e) NULL
      )
      if (is.null(fit)) {
        r$note <- "kruskal-wallis test failed"
      } else {
        r$statistic <- as.numeric(coin::statistic(fit))
        r$p <- as.numeric(coin::pvalue(fit))
        r$effect <- r$statistic / (length(v) - 1)
        r$effect_label <- sprintf("epsilon squared = %.2f", r$effect)
      }
    } else {
      # linear-by-linear on an ordered factor with 3 or more levels;
      # response enters as an ordered factor scored by its raw sort values
      vf <- factor(v, ordered = TRUE)
      dd <- data.frame(v = vf, g = g)
      fit <- tryCatch(
        coin::lbl_test(v ~ g, data = dd,
                       scores = list(v = sort(unique(v)))),
        error = function(e) NULL
      )
      if (is.null(fit)) {
        r$note <- "linear-by-linear test failed"
      } else {
        z <- as.numeric(coin::statistic(fit))
        r$statistic <- z
        r$p <- as.numeric(coin::pvalue(fit))
        r$effect <- z
        r$effect_label <- sprintf("z = %.2f", z)
        r$z_sign <- sign(z)
        if (!is.na(z) && z != 0) {
          r$direction <- paste(if (z > 0) "rises with" else "falls with", name)
        }
      }
    }
    res[[j]] <- r
  }

  p_raw <- vapply(res, function(r) r$p, numeric(1))
  q_adj <- priorities_bh(p_raw)
  flagged <- !is.na(q_adj) & q_adj <= q & gate_pass

  labels <- character(J)
  directions <- vapply(res, function(r) r$direction, character(1))

  for (j in seq_len(J)) {
    r <- res[[j]]
    if (!gate_pass) {
      labels[j] <- "No overall group difference"
      if (engine == "kruskal_wallis") directions[j] <- NA_character_
      next
    }
    if (is.na(p_raw[j])) {
      labels[j] <- if (identical(r$note, "no variation, not testable")) {
        "Not testable (no variation)"
      } else {
        "Not compared (insufficient data)"
      }
      next
    }
    if (!flagged[j]) {
      labels[j] <- "No evidence of a difference"
      if (engine == "kruskal_wallis") directions[j] <- NA_character_
      next
    }
    qtxt <- priorities_fmt_q(q_adj[j])
    if (!is.na(r$ps)) {
      p_win <- max(r$ps, 1 - r$ps)
      labels[j] <- sprintf("%s higher (P = %.2f, %s)", r$winner, p_win, qtxt)
    } else if (engine == "kruskal_wallis") {
      x <- X[, j]
      mask <- !is.na(x) & !is.na(a_chr) & a_chr %in% usable
      v <- x[mask]
      g <- droplevels(factor(a_chr[mask], levels = usable))
      dunn <- dunn_top_contrast(v, g, q)
      if (!is.null(dunn)) {
        directions[j] <- paste(dunn$winner, "highest")
        labels[j] <- sprintf("%s highest (%s)", dunn$winner, qtxt)
      } else {
        directions[j] <- NA_character_
        labels[j] <- sprintf("Groups differ (%s)", qtxt)
      }
    } else {
      word <- if (!is.na(r$z_sign) && r$z_sign < 0) "Falls" else "Rises"
      labels[j] <- sprintf("%s with %s (%s)", word, name, qtxt)
    }
  }

  rows <- data.frame(
    stmt = stmt_ids,
    attribute = name,
    test = engine,
    statistic = vapply(res, function(r) r$statistic, numeric(1)),
    p_raw = p_raw,
    q_adj = q_adj,
    flagged = flagged,
    direction = directions,
    effect = vapply(res, function(r) r$effect, numeric(1)),
    effect_label = vapply(res, function(r) r$effect_label, character(1)),
    hl_shift = vapply(res, function(r) r$hl_shift, numeric(1)),
    hl_lower = vapply(res, function(r) r$hl_lower, numeric(1)),
    hl_upper = vapply(res, function(r) r$hl_upper, numeric(1)),
    n_used = vapply(res, function(r) r$n_used, numeric(1)),
    n_levels = vapply(res, function(r) as.integer(r$n_levels), integer(1)),
    note = vapply(res, function(r) {
      paste(stats::na.omit(unique(c(attr_note, r$note))), collapse = "; ")
    }, character(1)),
    stringsAsFactors = FALSE
  )
  rows$note[rows$note == ""] <- NA_character_

  list(rows = rows, labels = labels)
}


#' Per-Statement Spearman Tests for a Numeric Attribute
#' @keywords internal
#' @noRd
test_attribute_numeric <- function(name, a, X, stmt_ids, q, gate_pass,
                                   attr_notes) {
  J <- ncol(X)
  attr_note <- if (length(attr_notes) > 0) {
    paste(attr_notes, collapse = "; ")
  } else {
    NA_character_
  }
  res <- vector("list", J)
  for (j in seq_len(J)) {
    x <- X[, j]
    mask <- !is.na(x) & !is.na(a)
    v <- x[mask]
    av <- a[mask]
    r <- list(statistic = NA_real_, p = NA_real_, effect = NA_real_,
              effect_label = NA_character_, direction = NA_character_,
              n_used = length(v), note = NA_character_)
    if (length(v) < 5) {
      r$note <- sprintf("not compared, n = %d", length(v))
    } else if (length(unique(v)) < 2) {
      r$note <- "no variation, not testable"
    } else if (length(unique(av)) < 2) {
      r$note <- "attribute has no variation, not testable"
    } else {
      ct <- tryCatch(
        suppressWarnings(
          stats::cor.test(v, av, method = "spearman", exact = FALSE)
        ),
        error = function(e) NULL
      )
      if (is.null(ct)) {
        r$note <- "spearman test failed"
      } else {
        rho <- unname(ct$estimate)
        r$statistic <- unname(ct$statistic)
        r$p <- ct$p.value
        r$effect <- rho
        r$effect_label <- sprintf("rho = %.2f", rho)
        if (!is.na(rho) && rho != 0) {
          r$direction <- paste(if (rho > 0) "rises with" else "falls with",
                               name)
        }
      }
    }
    res[[j]] <- r
  }

  p_raw <- vapply(res, function(r) r$p, numeric(1))
  q_adj <- priorities_bh(p_raw)
  flagged <- !is.na(q_adj) & q_adj <= q & gate_pass

  labels <- character(J)
  for (j in seq_len(J)) {
    r <- res[[j]]
    if (!gate_pass) {
      labels[j] <- "No overall group difference"
    } else if (is.na(p_raw[j])) {
      labels[j] <- if (identical(r$note, "no variation, not testable")) {
        "Not testable (no variation)"
      } else {
        "Not compared (insufficient data)"
      }
    } else if (!flagged[j]) {
      labels[j] <- "No evidence of a difference"
    } else {
      word <- if (!is.na(r$effect) && r$effect < 0) "Falls" else "Rises"
      labels[j] <- sprintf("%s with %s (%s)", word, name,
                           priorities_fmt_q(q_adj[j]))
    }
  }

  rows <- data.frame(
    stmt = stmt_ids,
    attribute = name,
    test = "spearman",
    statistic = vapply(res, function(r) r$statistic, numeric(1)),
    p_raw = p_raw,
    q_adj = q_adj,
    flagged = flagged,
    direction = vapply(res, function(r) r$direction, character(1)),
    effect = vapply(res, function(r) r$effect, numeric(1)),
    effect_label = vapply(res, function(r) r$effect_label, character(1)),
    hl_shift = NA_real_,
    hl_lower = NA_real_,
    hl_upper = NA_real_,
    n_used = vapply(res, function(r) r$n_used, numeric(1)),
    n_levels = NA_integer_,
    note = vapply(res, function(r) {
      paste(stats::na.omit(unique(c(attr_note, r$note))), collapse = "; ")
    }, character(1)),
    stringsAsFactors = FALSE
  )
  rows$note[rows$note == ""] <- NA_character_

  list(rows = rows, labels = labels)
}


# Top-n per group level ----

#' Top or Bottom Statements Per Group Level
#'
#' @description
#' For every level of every factor attribute in `groups`, the `n` statements
#' with the highest (or lowest) subgroup mean. Ties inside the block keep
#' stable statement order, and `tied` is TRUE for every rank in a run of
#' equal means, including the block boundary (rank `n` tying rank `n + 1`).
#' Numeric attributes have no levels and are skipped. Levels are descriptive
#' here, so small levels are included; interpretability is governed by the
#' PERMANOVA gates in [compute_priorities()].
#'
#' @param qdata A `QsortData` object.
#' @param groups A named list in the shape `attribute_groups()` returns, or
#'   NULL to use `attribute_groups(qdata)` when attributes exist.
#' @param n Number of statements per level (default 5).
#' @param direction "top" for the highest means, "bottom" for the lowest.
#'
#' @return A data.frame with columns `attribute`, `level`, `rank`, `stmt`,
#'   `statement`, `mean` (subgroup mean), and `tied`.
#' @export
priorities_top_n <- function(qdata, groups, n = 5,
                             direction = c("top", "bottom")) {

  if (!inherits(qdata, "QsortData")) {
    rlang::abort("qdata must be a QsortData object")
  }
  direction <- match.arg(direction)
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1) {
    rlang::abort("`n` must be a single positive number")
  }
  groups <- resolve_priorities_groups(qdata, groups)

  X <- qdata@sorts
  J <- ncol(X)
  n <- min(as.integer(n), J)
  stmt_ids <- paste0("S", seq_len(J))
  stmt_text <- if (length(qdata@statements) == J) {
    as.character(qdata@statements)
  } else {
    stmt_ids
  }

  out <- list()
  for (name in names(groups)) {
    a <- groups[[name]]
    if (is.numeric(a) && !is.factor(a)) next
    if (is.character(a) || is.logical(a)) a <- factor(a)
    if (!is.factor(a)) next
    for (lev in levels(a)) {
      idx <- which(!is.na(a) & as.character(a) == lev)
      if (length(idx) == 0) next
      m <- colMeans(X[idx, , drop = FALSE], na.rm = TRUE)
      ord <- if (direction == "top") {
        order(-m, seq_len(J))
      } else {
        order(m, seq_len(J))
      }
      sorted_m <- m[ord]
      run_tied <- duplicated(sorted_m) | duplicated(sorted_m, fromLast = TRUE)
      take <- seq_len(n)
      out[[length(out) + 1]] <- data.frame(
        attribute = name,
        level = lev,
        rank = take,
        stmt = stmt_ids[ord[take]],
        statement = stmt_text[ord[take]],
        mean = unname(sorted_m[take]),
        tied = run_tied[take],
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(out) == 0) {
    rlang::abort(c(
      "No factor attributes with usable levels found in `groups`",
      "i" = "priorities_top_n() needs at least one categorical or ordinal attribute"
    ))
  }
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}


# Flow chart ----

#' Contrast Text Color for a Fill
#' @description Black on light fills, white on dark fills, by weighted
#'   luminance. Delegates to `contrast_text_color()` when the package
#'   defines one.
#' @keywords internal
#' @noRd
priorities_text_color <- function(hex) {
  fun <- get0("contrast_text_color", mode = "function")
  if (!is.null(fun)) {
    return(vapply(hex, function(h) as.character(fun(h))[1], character(1)))
  }
  vapply(hex, function(h) {
    rgbv <- as.numeric(grDevices::col2rgb(h))
    lum <- (0.299 * rgbv[1] + 0.587 * rgbv[2] + 0.114 * rgbv[3]) / 255
    if (lum > 0.6) "black" else "white"
  }, character(1))
}

#' Alluvial Flow Chart of Top Statements Across Group Levels
#'
#' @description
#' Renders a [priorities_top_n()] table as an alluvial flow chart in the Kake
#' prototype's visual grammar: one column per group level (order of
#' appearance), strata stacked from Rank 1 downward, cubic flows with
#' front-back lode guidance, white-bordered strata carrying bold statement
#' codes, and a statement-text legend.
#'
#' The palette covers only the statements present in `top_tbl`, ordered by
#' `overall_means` (descending) when supplied, otherwise by first appearance.
#' Colors come from the theme factor colors extended with the prototype's
#' extra hexes and, beyond that, a ramp over the first six brand colors.
#'
#' @param top_tbl A data.frame from [priorities_top_n()] (columns `attribute`,
#'   `level`, `rank`, `stmt`, and ideally `statement`).
#' @param statements Optional statement text lookup: a named character vector
#'   (names are statement codes) or a data.frame whose first two columns are
#'   code and text. Defaults to the `statement` column of `top_tbl`.
#' @param overall_means Optional named numeric vector of overall statement
#'   means (names are statement codes) used to order the palette.
#' @param gates Optional `$gates` data.frame from [compute_priorities()].
#'   A caption marks panels of attributes whose gate failed as descriptive
#'   only; blocks keep their full color either way.
#' @param label_width Wrap width for legend labels (default 45).
#' @param palette `"brand"` (the default: theme factor colors extended with
#'   the prototype's hexes) or `"ramp"`: the Q-sort rank ramp, where the
#'   statement with the highest overall mean takes the warm end and the
#'   lowest the navy end, matching the app's pyramid color language.
#' @param show_legend Draw the statement-text legend (default TRUE). The
#'   dashboard passes FALSE and reveals statements on click instead.
#'
#' @return A ggplot object. For interactive use, the attributes
#'   `priorities_levels` (x-axis column keys in order), `priorities_colors`
#'   (statement fill colors), and `priorities_nmax` (ranks per column) are
#'   attached so a click position maps back to a statement.
#' @export
# Human label for an attribute name in captions ("agegroup" -> "age group")
priorities_pretty_label <- function(x) {
  x <- gsub("[._]+", " ", x)
  gsub("agegroup", "age group", x, ignore.case = TRUE)
}

plot_priorities_flow <- function(top_tbl, statements = NULL,
                                 overall_means = NULL, gates = NULL,
                                 label_width = 45,
                                 palette = c("brand", "ramp"),
                                 show_legend = TRUE) {

  palette <- match.arg(palette)

  priorities_require("ggalluvial", "for the priorities flow chart")

  needed <- c("attribute", "level", "rank", "stmt")
  missing_cols <- setdiff(needed, names(top_tbl))
  if (length(missing_cols) > 0) {
    rlang::abort(c(
      "`top_tbl` must be a priorities_top_n() table",
      "x" = paste0("Missing column(s): ", paste(missing_cols, collapse = ", "))
    ))
  }
  if (nrow(top_tbl) == 0) {
    rlang::abort("`top_tbl` has no rows to plot")
  }

  # Column key: level name, disambiguated by attribute when levels repeat
  pairs <- unique(top_tbl[, c("attribute", "level")])
  lvl_key <- if (anyDuplicated(pairs$level) > 0) {
    paste(top_tbl$attribute, top_tbl$level, sep = ": ")
  } else {
    as.character(top_tbl$level)
  }

  # Statement text lookup for the legend
  used <- unique(as.character(top_tbl$stmt))
  lookup <- NULL
  if (!is.null(statements)) {
    if (is.data.frame(statements) && ncol(statements) >= 2) {
      lookup <- stats::setNames(as.character(statements[[2]]),
                                as.character(statements[[1]]))
    } else if (is.character(statements) && !is.null(names(statements))) {
      lookup <- statements
    } else {
      rlang::abort(paste(
        "`statements` must be a named character vector or a data.frame",
        "with code and text columns"
      ))
    }
  } else if ("statement" %in% names(top_tbl)) {
    first <- !duplicated(top_tbl$stmt)
    lookup <- stats::setNames(as.character(top_tbl$statement[first]),
                              as.character(top_tbl$stmt[first]))
  }
  legend_text <- if (is.null(lookup)) used else {
    txt <- unname(lookup[used])
    ifelse(is.na(txt), used, txt)
  }
  names(legend_text) <- used

  # Palette over the used statements, ordered by overall mean when available
  if (!is.null(overall_means)) {
    if (is.null(names(overall_means))) {
      rlang::abort(
        "`overall_means` must be named by statement code (e.g. names 'S1', 'S2', ...)"
      )
    }
    om <- overall_means[used]
    used <- used[order(-om, seq_along(used), na.last = TRUE)]
  }
  if (identical(palette, "ramp")) {
    # The Q-sort rank ramp: `used` is ordered by overall mean descending, so
    # the top statement takes the warm end and the lowest the navy end
    pal <- rev(grDevices::colorRampPalette(ov2_ramp_anchors)(length(used)))
  } else {
    brand <- get_theme_colors()$factor_colors
    extras <- c(
      "#111C4E", "#774D28", "#66665D", "#F6DFA4", "#C4CFDA", "#C8C8C8",
      "#7030A0", "#A64D79", "#C00000", "#44546A", "#2F9599"
    )
    pal <- c(brand, extras)
    if (length(used) > length(pal)) {
      pal <- c(pal, grDevices::colorRampPalette(brand[1:6])(length(used) - length(pal)))
    }
  }
  colors_used <- stats::setNames(pal[seq_along(used)], used)
  txt_colors <- priorities_text_color(colors_used)

  # Gate handling: a caption marks failed attributes as descriptive only.
  # Blocks stay at full color; the ramp is the same language as the Q-sort
  # pyramid tiles and dimming it reads as a rendering fault, not a signal.
  failed_attrs <- character(0)
  if (!is.null(gates) && is.data.frame(gates) &&
      all(c("attribute", "pass") %in% names(gates))) {
    failed_attrs <- gates$attribute[!vapply(gates$pass, isTRUE, logical(1))]
  }
  failed_here <- intersect(failed_attrs, unique(top_tbl$attribute))
  caption <- if (length(failed_here) > 0) {
    sprintf(
      "Panels for %s are descriptive: no overall group difference.",
      paste(priorities_pretty_label(failed_here), collapse = ", ")
    )
  } else {
    NULL
  }

  flow <- data.frame(
    x_col = factor(lvl_key, levels = unique(lvl_key)),
    neg_rank = -as.integer(top_tbl$rank),
    code = as.character(top_tbl$stmt),
    freq = 1,
    stringsAsFactors = FALSE
  )
  flow$txt <- unname(txt_colors[flow$code])

  n_max <- max(top_tbl$rank)
  y_labels <- c("Rank 1 (Most Important)",
                if (n_max > 1) paste("Rank", 2:n_max))
  wrap_labels <- stringr::str_wrap(
    paste0(names(colors_used), ": ", legend_text[names(colors_used)]),
    width = label_width
  )

  p <- ggplot2::ggplot(
    flow,
    ggplot2::aes(x = .data$x_col, stratum = .data$neg_rank,
                 alluvium = .data$code, y = .data$freq,
                 fill = .data$code, label = .data$code)
  ) +
    ggalluvial::geom_flow(
      stat = "alluvium", lode.guidance = "frontback",
      curve_type = "cubic", width = 0.55, alpha = 0.6
    ) +
    ggalluvial::geom_stratum(
      width = 0.55, color = "white", linewidth = 3
    ) +
    ggplot2::geom_text(
      stat = ggalluvial::StatStratum,
      ggplot2::aes(color = .data$txt),
      size = 4.5, fontface = "bold", show.legend = FALSE
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_fill_manual(
      values = colors_used,
      breaks = names(colors_used),
      name = "Statement",
      labels = wrap_labels,
      guide = if (show_legend) {
        ggplot2::guide_legend(ncol = 1, byrow = TRUE)
      } else "none"
    ) +
    ggplot2::scale_x_discrete(expand = c(0.15, 0.05)) +
    # Rank k occupies the band [n_max - k, n_max - k + 1]; label band centers
    # (the prototype's negative breaks fall outside the stacked range and
    # would render no labels at all)
    ggplot2::scale_y_continuous(breaks = n_max - seq_len(n_max) + 0.5,
                                labels = y_labels) +
    ggplot2::labs(x = "Group", y = "Ranking", caption = caption) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 12, face = "bold",
                                          margin = ggplot2::margin(t = 8)),
      axis.text.y = ggplot2::element_text(size = 10, face = "bold", hjust = 1),
      axis.ticks.length.y = ggplot2::unit(0, "pt"),
      axis.title = ggplot2::element_text(face = "bold"),
      legend.position = if (show_legend) "right" else "none",
      legend.title = ggplot2::element_text(face = "bold"),
      legend.text = ggplot2::element_text(lineheight = 1.2),
      panel.grid.major.y = ggplot2::element_line(color = "gray90",
                                                 linewidth = 0.5),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.caption = ggplot2::element_text(color = "#4a5568", hjust = 0)
    )

  attr(p, "priorities_levels") <- unique(lvl_key)
  attr(p, "priorities_colors") <- colors_used
  attr(p, "priorities_nmax") <- n_max
  p
}


# Workbook export ----

#' Write the Priorities Workbook
#'
#' @description
#' Exports the priorities layer to an Excel workbook with sheets "Ranked"
#' (the core ranked table), "Group comparisons" (the ranked table with one
#' label column per tested attribute plus a legend explaining q), "Top by
#' group" (the [priorities_top_n()] long table), "Attributes" (the
#' participant attribute table plus any match report sentences stored in
#' `metadata$attribute_match`), and, when ggalluvial is available and group
#' attributes exist, "Flow" with the flow chart inserted as a PNG.
#'
#' @param qdata A `QsortData` object.
#' @param file Output .xlsx path.
#' @param priorities Optional precomputed [compute_priorities()] result; when
#'   NULL it is computed from `qdata` and `groups`.
#' @param groups NULL or a named list in the shape `attribute_groups()`
#'   returns (see [compute_priorities()]).
#' @param top_n Statements per level on the "Top by group" and "Flow" sheets
#'   (default 5).
#'
#' @return Invisibly TRUE on success.
#' @export
write_priorities_workbook <- function(qdata, file, priorities = NULL,
                                      groups = NULL, top_n = 5) {

  if (!inherits(qdata, "QsortData")) {
    rlang::abort("qdata must be a QsortData object")
  }
  priorities_require("openxlsx", "for the priorities workbook")

  groups <- resolve_priorities_groups(qdata, groups)
  if (is.null(priorities)) {
    priorities <- compute_priorities(qdata, groups = groups)
  }
  if (!inherits(priorities, "qsort_priorities")) {
    rlang::abort("`priorities` must be a compute_priorities() result")
  }

  tbl <- priorities$table
  core_cols <- intersect(.priorities_core_cols, names(tbl))
  ranked <- tbl[, core_cols, drop = FALSE]

  has_factor_groups <- any(vapply(groups, function(a) {
    is.factor(a) || is.character(a) || is.logical(a)
  }, logical(1)))
  top_tbl <- NULL
  if (has_factor_groups) {
    top_tbl <- tryCatch(
      priorities_top_n(qdata, groups = groups, n = top_n),
      error = function(e) NULL
    )
  }

  wb <- openxlsx::createWorkbook()
  header_style <- openxlsx::createStyle(
    textDecoration = "bold", border = "bottom"
  )
  number_style <- openxlsx::createStyle(halign = "right", numFmt = "0.00")
  int_cols <- c("rank", "n_above", "n_at", "n_below", "n_used", "n_levels",
                "df", "n")

  write_sheet <- function(sheet, df, start_row = 1) {
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, df, startRow = start_row,
                        headerStyle = header_style)
    openxlsx::freezePane(wb, sheet, firstActiveRow = start_row + 1)
    num_cols <- which(vapply(df, is.numeric, logical(1)) &
                        !(names(df) %in% int_cols))
    if (length(num_cols) > 0 && nrow(df) > 0) {
      openxlsx::addStyle(
        wb, sheet, number_style,
        rows = (start_row + 1):(start_row + nrow(df)),
        cols = num_cols, gridExpand = TRUE
      )
    }
    openxlsx::setColWidths(wb, sheet, cols = seq_along(df), widths = "auto")
  }

  # 1. Ranked
  write_sheet("Ranked", ranked)

  # 2. Group comparisons: statement keys plus the per-group columns, only
  # when attributes produced any
  grp_cols <- setdiff(names(tbl), core_cols)
  if (length(grp_cols) > 0) {
    key_cols <- intersect(c("rank", "stmt", "statement"), names(tbl))
    write_sheet("Group comparisons",
                tbl[, c(key_cols, grp_cols), drop = FALSE])
  }

  # 3. Top by group
  if (!is.null(top_tbl)) {
    write_sheet("Top by group", top_tbl)
  }

  # 4. Attributes
  attr_fun <- get0("participant_attributes", mode = "function")
  attr_df <- if (!is.null(attr_fun)) {
    tryCatch(attr_fun(qdata), error = function(e) NULL)
  } else {
    NULL
  }
  if (is.null(attr_df)) {
    meta_attr <- qdata@metadata$participant_attributes
    if (is.data.frame(meta_attr)) attr_df <- meta_attr
  }
  if (is.data.frame(attr_df)) {
    write_sheet("Attributes", attr_df)
  }

  # 5. Figures: the same builders and dimensions as the dashboard downloads,
  # one figure per sheet
  add_figure <- function(sheet, width, height, render) {
    tryCatch({
      png_file <- tempfile(fileext = ".png")
      grDevices::png(png_file, width = width, height = height,
                     units = "in", res = 200, bg = "white")
      tryCatch(render(), finally = grDevices::dev.off())
      openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)
      openxlsx::insertImage(wb, sheet, png_file, width = width,
                            height = height, units = "in",
                            startRow = 2, startCol = 2)
    }, error = function(e) {
      cli::cli_alert_warning("{sheet} sheet skipped: {conditionMessage(e)}")
    })
    invisible(NULL)
  }

  J <- length(qdata@statements)
  add_figure("Priorities ranking", 9, max(7, J * 0.2), function() {
    print(create_plot_priorities_ranking(qdata))
  })

  if (has_participant_attributes(qdata)) {
    k_attrs <- length(attribute_groups(qdata))
    add_figure("Panel composition", 13, 1 + 4.2 * ceiling(k_attrs / 2),
               function() {
                 g <- create_plot_panel_composition(qdata)
                 if (is.null(g)) stop("no composition panels")
                 grid::grid.draw(g)
               })
  }

  if (!is.null(top_tbl) && requireNamespace("ggalluvial", quietly = TRUE)) {
    add_figure("Priorities flow", 12, 7.5, function() {
      print(plot_priorities_flow(
        top_tbl,
        statements = stats::setNames(qdata@statements,
                                     paste0("S", seq_along(qdata@statements))),
        overall_means = stats::setNames(tbl$mean, tbl$stmt),
        gates = priorities$gates,
        palette = "ramp",
        show_legend = FALSE
      ))
    })
  }

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  cli::cli_alert_success("Priorities workbook written to: {.file {file}}")
  invisible(TRUE)
}
