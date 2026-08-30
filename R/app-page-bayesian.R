#' @title Bayesian Analysis Page Module
#' @description Bayesian Q methodology through the bayesqm package: the exact
#' rank-order (partition) likelihood for forced sorts, a parameter-expanded
#' Gibbs sampler with a convergence gate, MatchAlign post-processing, and one
#' posterior false-discovery rule behind every published claim. The page is
#' decision-first: in ladder mode the two-signal choice-of-K display leads,
#' and the reported rung is switched with one click. A complete multi-sheet
#' Excel workbook of the analysis is one download away.
#' @name app-page-bayesian
NULL


# bayesqm helpers (dashboard-side) ----

#' Compute the full posterior table set for one bayesqm fit
#' @keywords internal
bq_table_bundle <- function(fit, q = 0.05) {
  K <- fit$brief$K
  list(
    loadings = bayesqm::compute_loadings(fit),
    flags    = suppressMessages(bayesqm::compute_flags(fit, q = q)),
    zscores  = bayesqm::compute_zscores(fit),
    array    = bayesqm::compute_factor_array(fit),
    chars    = bayesqm::factor_characteristics(fit, q = q),
    qdc      = if (K >= 2) bayesqm::compute_qdc(fit, q = q) else NULL,
    claims   = bayesqm::claims(fit, q = q),
    checks   = bayesqm::check_fit(fit, draws = 100),
    persons  = bayesqm::check_persons(fit)
  )
}

#' Drop the raw draws and sampler state from a fit (serialization weight)
#' @keywords internal
bq_strip_fit <- function(fit) {
  fit$draws_raw <- NULL
  fit$state <- NULL
  fit
}

#' Grid-column index -> conventional score labels (-4..4 style)
#' @keywords internal
bq_grid_labels <- function(C) {
  if (C %% 2 == 0) c(seq(-C / 2, -1), seq(1, C / 2)) else seq_len(C) - (C + 1) / 2
}

#' Join full statement text onto S# ids, truncated for display
#' @keywords internal
bq_statement_text <- function(ids, statements, width = 60) {
  if (is.null(statements)) return(NULL)
  idx <- suppressWarnings(as.integer(sub("^S", "", ids)))
  if (anyNA(idx) || max(idx, na.rm = TRUE) > length(statements)) return(NULL)
  txt <- statements[idx]
  if (!is.null(width)) {
    txt <- ifelse(nchar(txt) > width, paste0(substr(txt, 1, width - 3), "…"), txt)
  }
  txt
}

#' Run one bayesqm dashboard job (single fit or ladder) and bundle results
#' @keywords internal
run_bayesqm_job <- function(Y, distribution, mode, K, k_min, k_max, q,
                            iterations, burn, thin, max_iterations,
                            seed, sigma_scale) {
  qd <- bayesqm::qsort_data(Y, distribution = distribution, validate = FALSE)

  if (mode == "single") {
    fit <- bayesqm::fit_bayesian(
      qd, K = K, iterations = iterations, burn = burn, thin = thin,
      max_iterations = max_iterations, seed = seed,
      sigma_scale = sigma_scale, quiet = TRUE
    )
    tables <- bq_table_bundle(fit, q = q)
    list(
      engine = "bayesqm", mode = "single", q = q,
      K = fit$brief$K, K_source = "user",
      fit = bq_strip_fit(fit), tables = tables,
      ladder = NULL
    )
  } else {
    lad <- bayesqm::fit_ladder(
      qd, K_min = k_min, K_max = k_max, q = q,
      iterations = iterations, burn = burn, thin = thin,
      max_iterations = max_iterations, seed = seed,
      sigma_scale = sigma_scale, quiet = TRUE
    )
    sel <- bayesqm::select_k(lad)

    rungs <- stats::setNames(
      lapply(lad$rungs, function(r) {
        list(K = r$K,
             fit = bq_strip_fit(r$fit),
             tables = bq_table_bundle(r$fit, q = q),
             converged = r$converged)
      }),
      as.character(lad$K)
    )

    K_star <- sel$K
    K_active <- if (!is.na(sel$K)) sel$K
                else if (any(sel$table$adequate)) sel$table$K[which(sel$table$adequate)[1]]
                else lad$K[1]
    # honest provenance of the displayed K: the model selected it, or it
    # is merely on display for inspection because nothing was selected
    K_source <- if (!is.na(sel$K)) "model" else "inspect"

    # When no rung is selected, the reportable default is the one-factor
    # solution (K = 1 cannot be a ladder rung; support needs factor pairs).
    # For single_viewpoint it is the model's own conclusion; for the other
    # no-selection verdicts it is the default report, with the banner
    # explaining the situation. Tension is the exception: two defensible
    # multi-factor candidates stand, so the choice stays with the user.
    if (is.na(sel$K) && !identical(sel$verdict, "tension")) {
      fit1 <- bayesqm::fit_bayesian(
        qd, K = 1, iterations = iterations, burn = burn, thin = thin,
        max_iterations = max_iterations, seed = seed,
        sigma_scale = sigma_scale, quiet = TRUE
      )
      rungs[["1"]] <- list(K = 1L, fit = bq_strip_fit(fit1),
                           tables = bq_table_bundle(fit1, q = q),
                           converged = isTRUE(fit1$gate$converged))
      K_active <- 1L
      if (identical(sel$verdict, "single_viewpoint")) {
        K_star <- 1L
        K_source <- "model"
      } else {
        K_source <- "fallback"
      }
    }

    active <- rungs[[as.character(K_active)]]

    list(
      engine = "bayesqm", mode = "ladder", q = q,
      K = K_active, K_source = K_source,
      fit = active$fit, tables = active$tables,
      ladder = list(K = lad$K, selection = sel, K_star = K_star,
                    verdict = sel$verdict, rungs = rungs)
    )
  }
}

#' Mutual-unspanned cluster signal from a stored person-check table
#' @keywords internal
bq_unspanned_cluster <- function(pc) {
  idx <- which(pc$verdict == "unspanned")
  pi_ <- if (!is.null(pc$partner_index)) pc$partner_index
         else match(pc$partner, pc$participant)
  length(idx) >= 2 && any(vapply(idx, function(i)
    pi_[i] %in% idx && pi_[pi_[i]] == i, logical(1)))
}

#' Re-judge the two-signal selection at another q from stored draws (no refit)
#' @keywords internal
bq_rejudge <- function(bundle, q2) {
  ks <- sort(as.integer(names(bundle$ladder$rungs)))
  ks <- ks[ks >= 2]                     # a folded K = 1 rung has no support machinery
  rungs <- lapply(ks, function(k) {
    r <- bundle$ladder$rungs[[as.character(k)]]
    cl <- r$tables$claims
    fac_ids <- dimnames(r$fit$draws$sigma)[[2]]
    supported <- vapply(fac_ids, function(f)
      sum(cl$flags$factor == f) >= 2 && any(cl$distinguishing$factor == f),
      logical(1))
    list(K = k, fit = r$fit,
         t1b = r$tables$checks$extra_factor$percentile,
         cluster = bq_unspanned_cluster(r$tables$persons),
         supported = supported, converged = r$converged,
         flags = cl$flags, distinguishing = cl$distinguishing)
  })
  lad <- structure(list(K = ks, rungs = rungs, q = bundle$q,
                        N = bundle$fit$brief$N),
                   class = "bayesqm_ladder")
  sel <- bayesqm::select_k(lad, q = q2)

  for (k in ks) {
    key <- as.character(k)
    r <- bundle$ladder$rungs[[key]]
    tb <- r$tables
    tb$flags <- suppressMessages(bayesqm::compute_flags(r$fit, q = q2))
    tb$chars <- bayesqm::factor_characteristics(r$fit, q = q2)
    tb$qdc <- if (r$fit$brief$K >= 2) bayesqm::compute_qdc(r$fit, q = q2) else NULL
    tb$claims <- bayesqm::claims(r$fit, q = q2)
    bundle$ladder$rungs[[key]]$tables <- tb
  }

  bundle$q <- q2
  bundle$ladder$selection <- sel
  bundle$ladder$K_star <- sel$K
  bundle$ladder$verdict <- sel$verdict
  if (!is.na(sel$K)) {
    act <- bundle$ladder$rungs[[as.character(sel$K)]]
    bundle$K <- sel$K
    bundle$K_source <- "model"
    bundle$fit <- act$fit
    bundle$tables <- act$tables
  } else {
    bundle$K_source <- "inspect"
    if (!is.null(bundle$ladder$rungs[[as.character(bundle$K)]])) {
      bundle$tables <- bundle$ladder$rungs[[as.character(bundle$K)]]$tables
    }
  }
  bundle
}

#' Bridge a bayesqm bundle to a QsortResults object (posterior means),
#' so the shared visualization studio charts can draw the Bayesian solution.
#' @keywords internal
bayesqm_to_results <- function(bundle, qdata) {

  fit <- bundle$fit
  tb <- bundle$tables
  call <- match.call()

  K <- fit$brief$K
  n_participants <- nrow(qdata@sorts)
  n_statements <- ncol(qdata@sorts)
  fac_ids <- setdiff(gsub("_loading$", "", grep("_loading$", names(tb$loadings), value = TRUE)), "")

  participants <- qdata@participants %||% rownames(qdata@sorts) %||% paste0("P", seq_len(n_participants))
  statements <- qdata@statements %||% colnames(qdata@sorts) %||% paste0("S", seq_len(n_statements))

  # Posterior-mean bounded loadings
  loadings_matrix <- as.matrix(tb$loadings[, paste0(fac_ids, "_loading"), drop = FALSE])
  rownames(loadings_matrix) <- participants
  colnames(loadings_matrix) <- paste0("F", seq_len(K))

  # Factor scores: posterior-mean z-scores + quota-respecting grid scores
  distribution <- fit$distribution
  C <- length(distribution)
  labels_grid <- bq_grid_labels(C)
  factor_scores <- data.frame(statement_num = seq_len(n_statements),
                              statement = statements,
                              stringsAsFactors = FALSE)
  for (j in seq_len(K)) {
    fname <- paste0("F", j)
    factor_scores[[paste0(fname, "_zscore")]] <- tb$zscores[[paste0(fac_ids[j], "_zsc")]]
    factor_scores[[paste0(fname, "_score")]] <- labels_grid[tb$array[[paste0(fac_ids[j], "_grid")]]]
  }

  # Flagging matrix from the FDR-selected flags
  flagging <- matrix(FALSE, n_participants, K,
                     dimnames = list(participants, paste0("F", seq_len(K))))
  fl <- tb$flags
  sel <- which(fl$selected)
  if (length(sel)) {
    kk <- match(fl$factor[sel], fac_ids)
    flagging[cbind(sel, kk)] <- TRUE
  }

  # Participant correlation + eigen decomposition for the shared charts
  correlation <- stats::cor(t(as.matrix(qdata@sorts)))
  ev <- eigen(correlation, symmetric = TRUE, only.values = TRUE)$values
  extraction <- list(
    eigenvalues = ev,
    variance_explained = ev / length(ev),
    method = "bayesqm exact partition likelihood"
  )

  factor_chars_result <- tryCatch(
    compute_factor_characteristics(
      sorts = qdata@sorts, flags = flagging, loadings = loadings_matrix,
      extraction = extraction, factor_scores = factor_scores
    ),
    error = function(e) list(characteristics = NULL, se_factor = NULL, sed_matrix = NULL)
  )

  distinction <- tryCatch(
    qsort_distinguish(
      factor_scores, statements = qdata@statements,
      sed_matrix = factor_chars_result$sed_matrix,
      se_factors = factor_chars_result$characteristics$se_factor
    ),
    error = function(e) list(distinguishing = list(), consensus = character(0),
                             distinguishing_significance = NULL, comparison_df = NULL)
  )

  method_details <- list(
    extraction = "bayesqm", rotation = "matchalign",
    cor_method = "pearson", flagging = "posterior FDR",
    flag_threshold = NULL, forced = TRUE,
    sed_matrix = factor_chars_result$sed_matrix,
    se_factors = factor_chars_result$characteristics$se_factor,
    distinguishing_significance = distinction$distinguishing_significance,
    comparison_details = distinction$comparison_df,
    bayesqm_q = bundle$q
  )

  qsort_results(
    data = qdata, correlation = correlation, extraction = extraction,
    rotation = list(loadings = loadings_matrix, method = "bayesqm_posterior_mean"),
    flagging = flagging, factor_scores = factor_scores,
    distinguishing = distinction$distinguishing, consensus = distinction$consensus,
    factor_characteristics = factor_chars_result, n_factors = K,
    method_details = method_details, call = call
  )
}


# The workbook: the complete analysis in one Excel file ----

#' Write the complete bayesqm analysis to a multi-sheet Excel workbook
#' @keywords internal
bq_write_workbook <- function(file, bundle, qdata) {

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("The workbook export needs the openxlsx package.")
  }

  fit <- bundle$fit
  tb <- bundle$tables
  fac_ids <- dimnames(fit$draws$sigma)[[2]]
  K <- fit$brief$K
  distribution <- fit$distribution
  labels_grid <- bq_grid_labels(length(distribution))
  statements <- qdata@statements

  wb <- openxlsx::createWorkbook()

  header_style <- openxlsx::createStyle(
    textDecoration = "bold", halign = "left", border = "bottom"
  )
  title_style <- openxlsx::createStyle(fontSize = 13, textDecoration = "bold")
  note_style <- openxlsx::createStyle(textDecoration = "italic")

  add_sheet <- function(name, df, note = NULL, start_row = 1) {
    openxlsx::addWorksheet(wb, name)
    row <- start_row
    if (!is.null(note)) {
      openxlsx::writeData(wb, name, note, startRow = row)
      openxlsx::addStyle(wb, name, note_style, rows = row, cols = 1)
      row <- row + 2
    }
    openxlsx::writeData(wb, name, df, startRow = row, headerStyle = header_style)
    openxlsx::freezePane(wb, name, firstActiveRow = row + 1)
    openxlsx::setColWidths(wb, name, cols = seq_len(ncol(df)), widths = "auto")
    invisible(row + nrow(df) + 1)
  }

  add_block <- function(name, title, df, row) {
    openxlsx::writeData(wb, name, title, startRow = row)
    openxlsx::addStyle(wb, name, title_style, rows = row, cols = 1)
    row <- row + 1
    if (is.null(df) || nrow(df) == 0) {
      openxlsx::writeData(wb, name, "(none selected)", startRow = row)
      openxlsx::addStyle(wb, name, note_style, rows = row, cols = 1)
      return(row + 2)
    }
    openxlsx::writeData(wb, name, df, startRow = row, headerStyle = header_style)
    row + nrow(df) + 2
  }

  round_df <- function(df, digits = 3) {
    num <- vapply(df, is.numeric, logical(1))
    df[num] <- lapply(df[num], round, digits)
    df
  }

  # 1. Overview -----------------------------------------------------------
  g <- fit$gate
  overview <- data.frame(
    Item = c("Engine", "Model", "Mode", "Reported K", "K provenance", "Participants (N)",
             "Statements (J)", "Grid distribution", "Kept draws",
             "Iterations", "Burn-in", "Thin", "Warm-extended",
             "Convergence gate", "Max R-hat", "Min bulk ESS", "Min tail ESS",
             "Alignment: mean congruence", "Alignment: pivot draw",
             "FDR level q", "Credible-interval probability", "Seed", "Date"),
    Value = c("bayesqm (exact rank-order likelihood)",
              fit$brief$model %||% "exact partition likelihood, PX-Gibbs",
              if (identical(bundle$mode, "ladder")) "Ladder (two-signal choice of K)" else "Single K",
              bundle$K,
              switch(bundle$K_source %||% "user",
                     model = "selected by the model",
                     user = "chosen by the user",
                     fallback = "reported by default (no K selected)",
                     "not selected (inspection view)"),
              fit$brief$N, fit$brief$J,
              paste(distribution, collapse = "-"),
              dim(fit$draws$F)[1],
              g$iterations, fit$brief$settings$burn, fit$brief$settings$thin,
              ifelse(isTRUE(g$extended), "yes", "no"),
              ifelse(isTRUE(g$converged), "passed", "NOT met"),
              round(g$rhat, 3), round(g$ess_bulk, 0), round(g$ess_tail, 0),
              round(mean(fit$align$congruence), 3), fit$align$pivot,
              bundle$q, fit$brief$prob %||% 0.95,
              fit$brief$seed %||% "none", format(Sys.time(), "%Y-%m-%d %H:%M")),
    stringsAsFactors = FALSE
  )
  add_sheet("Overview", overview,
            note = "canhrQsort Bayesian analysis via the bayesqm engine")

  # 2. Loadings & Flags ---------------------------------------------------
  lo <- tb$loadings; fl <- tb$flags
  lo_df <- data.frame(Participant = lo$participant, stringsAsFactors = FALSE)
  for (f in fac_ids) {
    lo_df[[paste0(toupper(f), "_loading")]] <- lo[[paste0(f, "_loading")]]
    lo_df[[paste0(toupper(f), "_lower")]] <- lo[[paste0(f, "_lower")]]
    lo_df[[paste0(toupper(f), "_upper")]] <- lo[[paste0(f, "_upper")]]
  }
  lo_df$Spread <- lo$spread
  lo_df$Modal_flag <- paste0(toupper(fl$factor), ifelse(fl$sign > 0, "+", "-"))
  lo_df$P_flag <- fl$flag_prob
  lo_df$P_unclassified <- fl$unclassified_prob
  lo_df$Selected <- ifelse(fl$selected, "yes", "")
  add_sheet("Loadings & Flags", round_df(lo_df),
            note = paste0("Bounded loadings with ",
                          round(100 * (fit$brief$prob %||% 0.95)),
                          "% credible intervals; flags selected at posterior FDR q = ",
                          bundle$q))

  # 3. Statement Scores ---------------------------------------------------
  z <- tb$zscores; ar <- tb$array
  z_df <- data.frame(Statement = z$statement, stringsAsFactors = FALSE)
  txt <- bq_statement_text(z$statement, statements, width = NULL)
  if (!is.null(txt)) z_df$Text <- txt
  for (f in fac_ids) {
    z_df[[paste0(toupper(f), "_z")]] <- z[[paste0(f, "_zsc")]]
    z_df[[paste0(toupper(f), "_lower")]] <- z[[paste0(f, "_lower")]]
    z_df[[paste0(toupper(f), "_upper")]] <- z[[paste0(f, "_upper")]]
    z_df[[paste0(toupper(f), "_grid")]] <- labels_grid[ar[[paste0(f, "_grid")]]]
  }
  add_sheet("Statement Scores", round_df(z_df),
            note = "Posterior mean z-scores with credible intervals and the quota-respecting grid column")

  # 4. Factor Arrays ------------------------------------------------------
  cert <- attr(ar, "certainty")
  ar_df <- data.frame(Statement = ar$statement, stringsAsFactors = FALSE)
  if (!is.null(txt)) ar_df$Text <- txt
  for (k in seq_along(fac_ids)) {
    f <- fac_ids[k]
    ar_df[[paste0(toupper(f), "_column")]] <- labels_grid[ar[[paste0(f, "_grid")]]]
    ar_df[[paste0(toupper(f), "_certainty")]] <- cert[, k]
  }
  add_sheet("Factor Arrays", round_df(ar_df),
            note = "Quota-exact arrays; certainty is the posterior probability of the reported column")

  # 5-7. Distinguishing, Consensus, Contrasts ----------------------------
  if (!is.null(tb$qdc)) {
    qdc <- tb$qdc

    d_df <- data.frame(Statement = qdc$statement, stringsAsFactors = FALSE)
    if (!is.null(txt)) d_df$Text <- txt
    for (f in fac_ids) {
      cn <- paste0(f, "_dist_prob")
      if (cn %in% names(qdc)) d_df[[paste0("P_dist_", toupper(f))]] <- qdc[[cn]]
    }
    d_df$Selected_for <- ifelse(grepl("^distinguishing", qdc$verdict),
                                sub("^distinguishing ", "", qdc$verdict), "")
    add_sheet("Distinguishing", round_df(d_df),
              note = paste0("Distinguishing listings against the posterior critical difference; ",
                            "selected via posterior FDR q = ", bundle$q))

    c_df <- data.frame(Statement = qdc$statement, stringsAsFactors = FALSE)
    if (!is.null(txt)) c_df$Text <- txt
    c_df$P_consensus <- qdc$consensus_prob
    c_df$Selected <- ifelse(qdc$verdict == "consensus", "yes", "")
    add_sheet("Consensus", round_df(c_df),
              note = paste0("Consensus within one grid column across every factor; ",
                            "selected via posterior FDR q = ", bundle$q))

    ct <- attr(qdc, "contrasts")
    if (!is.null(ct)) {
      ct_df <- ct
      names(ct_df) <- c("Statement", "Pair", "Median", "Lower", "Upper",
                        "P_exceed", "P_diff_column", "Selected", "Stars")[seq_len(ncol(ct_df))]
      add_sheet("Contrasts", round_df(ct_df),
                note = paste0("Per-pair score contrasts; delta_kl = ",
                              paste(paste0(names(attr(qdc, "delta_kl")), " ",
                                           round(attr(qdc, "delta_kl"), 2)), collapse = ", ")))
    }
  }

  # 7. Claims -------------------------------------------------------------
  cl <- tb$claims
  openxlsx::addWorksheet(wb, "Claims")
  openxlsx::writeData(wb, "Claims",
                      paste0("Every family selected by one posterior false-discovery rule at q = ", cl$q),
                      startRow = 1)
  openxlsx::addStyle(wb, "Claims", note_style, rows = 1, cols = 1)
  row <- 3
  ef <- cl$expected_false
  row <- add_block("Claims", paste0("Participant flags (expected false ",
                                    sprintf("%.2f", ef[["flags"]]), ")"),
                   round_df(cl$flags %||% data.frame()), row)
  row <- add_block("Claims", paste0("Distinguishing listings (expected false ",
                                    sprintf("%.2f", ef[["distinguishing"]]), ")"),
                   round_df(cl$distinguishing %||% data.frame()), row)
  row <- add_block("Claims", paste0("Consensus statements (expected false ",
                                    sprintf("%.2f", ef[["consensus"]]), ")"),
                   round_df(cl$consensus %||% data.frame()), row)
  add_block("Claims", paste0("Pairwise stars (expected false ",
                             sprintf("%.2f", ef[["stars"]]), ")"),
            round_df(cl$stars %||% data.frame()), row)
  openxlsx::setColWidths(wb, "Claims", cols = 1:6, widths = 22)

  # 8. Characteristics ----------------------------------------------------
  chars <- tb$chars
  openxlsx::addWorksheet(wb, "Characteristics")
  openxlsx::writeData(wb, "Characteristics", round_df(as.data.frame(chars)),
                      startRow = 1, headerStyle = header_style)
  sc <- attr(chars, "score_correlations")
  if (!is.null(sc)) {
    r0 <- nrow(chars) + 3
    openxlsx::writeData(wb, "Characteristics", "Posterior mean statement-score correlations",
                        startRow = r0)
    openxlsx::addStyle(wb, "Characteristics", title_style, rows = r0, cols = 1)
    openxlsx::writeData(wb, "Characteristics",
                        round_df(as.data.frame(cbind(factor = rownames(sc), as.data.frame(sc)))),
                        startRow = r0 + 1, headerStyle = header_style)
  }
  openxlsx::setColWidths(wb, "Characteristics", cols = 1:10, widths = "auto")

  # 9. Diagnostics --------------------------------------------------------
  ck <- tb$checks
  diag_df <- data.frame(
    Check = c("Convergence gate", "Max R-hat", "Min bulk ESS", "Min tail ESS",
              "Iterations", "Warm-extended", "Mean alignment congruence",
              "Agreement check T1a (p)", "Extra-factor check T1b (percentile)",
              "Paired-comparison check T2 (p)", "T2 worst statement (p)"),
    Value = c(ifelse(isTRUE(g$converged), "passed", "NOT met"),
              round(g$rhat, 3), round(g$ess_bulk, 0), round(g$ess_tail, 0),
              g$iterations, ifelse(isTRUE(g$extended), "yes", "no"),
              round(mean(fit$align$congruence), 3),
              round(ck$agreement$p, 2), round(ck$extra_factor$percentile, 2),
              round(ck$paired$p, 2), round(min(ck$paired$p_j), 2)),
    stringsAsFactors = FALSE
  )
  add_sheet("Diagnostics", diag_df,
            note = "Posterior-predictive checks are diagnostics to read, not tests to pass")

  # 10. Person Check ------------------------------------------------------
  pc <- tb$persons
  pc_df <- data.frame(
    Participant = pc$participant,
    Model_agreement_m = round(pc$m, 3),
    Person_agreement_w = round(pc$w, 3),
    Nearest_partner = pc$partner,
    Verdict = pc$verdict,
    stringsAsFactors = FALSE
  )
  add_sheet("Person Check", pc_df,
            note = "Verdicts against mixed-replication bands; unspanned persons name their nearest partner")

  # 11. Choice of K (ladder only) ----------------------------------------
  if (!is.null(bundle$ladder)) {
    sel <- bundle$ladder$selection
    tab <- sel$table
    sel_df <- data.frame(
      K = tab$K,
      Gate_passed = ifelse(tab$converged, "yes", "no"),
      Extra_factor_percentile = round(tab$extra_factor, 2),
      Unspanned_cluster = ifelse(tab$cluster, "yes", "no"),
      Adequate = ifelse(tab$adequate, "yes", "no"),
      Factors_supported = paste0(tab$factors_supported, " / ", tab$K),
      All_supported = ifelse(tab$all_supported, "yes", "no"),
      stringsAsFactors = FALSE
    )
    last <- add_sheet("Choice of K", sel_df,
                      note = paste0("Two-signal verdict: ", gsub("_", " ", sel$verdict),
                                    if (!is.na(sel$K)) paste0(" - selected K = ", sel$K) else ""))
    det <- sel$detail
    if (!is.null(det)) {
      openxlsx::writeData(wb, "Choice of K", "Per-factor support evidence", startRow = last + 2)
      openxlsx::addStyle(wb, "Choice of K", title_style, rows = last + 2, cols = 1)
      openxlsx::writeData(wb, "Choice of K", round_df(as.data.frame(det)),
                          startRow = last + 3, headerStyle = header_style)
    }
  }

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}


# UI ----

#' Bayesian Analysis Page UI
#' @param id Module ID
#' @return Shiny UI elements
#' @keywords internal
bayesian_page_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(

    # Control bar: who chooses K, one number, and the page's actions
    htmltools::div(
      class = "bq2-bar",
      htmltools::div(
        class = "bq2-group",
        htmltools::div(class = "bq2-label", "Choice of K"),
        shiny::uiOutput(ns("mode_toggle"))
      ),
      shiny::uiOutput(ns("k_controls"), inline = TRUE),
      htmltools::div(class = "bq2-bar-spacer"),
      htmltools::div(
        class = "bq2-bar-actions",
        shiny::actionButton(
          ns("toggle_settings"),
          htmltools::tagList("Sampler settings", shiny::icon("chevron-down")),
          class = "btn-bq2-quiet"
        ),
        shiny::uiOutput(ns("workbook_button_area"), inline = TRUE),
        shiny::uiOutput(ns("run_button_area"), inline = TRUE)
      )
    ),

    # Collapsible sampler settings (launch parameters only; the claim
    # strictness q lives on the results, where it can change without a refit)
    htmltools::div(
      id = ns("settings_panel"),
      class = "bq2-settings collapsed",
      htmltools::div(
        class = "bq2-settings-grid",
        htmltools::div(
          class = "bq2-setting",
          htmltools::div(class = "bq2-label", "Iterations"),
          shiny::numericInput(ns("iterations"), NULL, value = 12000, min = 600,
                              max = 60000, step = 1000, width = "100%")
        ),
        htmltools::div(
          class = "bq2-setting",
          htmltools::div(class = "bq2-label", "Burn-in"),
          shiny::numericInput(ns("burn"), NULL, value = 2000, min = 100,
                              max = 20000, step = 500, width = "100%")
        ),
        htmltools::div(
          class = "bq2-setting",
          htmltools::div(class = "bq2-label", "Thin"),
          shiny::numericInput(ns("thin"), NULL, value = 5, min = 1, max = 50,
                              width = "100%")
        ),
        htmltools::div(
          class = "bq2-setting",
          htmltools::div(class = "bq2-label", "Max iterations"),
          shiny::numericInput(ns("max_iterations"), NULL, value = 48000, min = 600,
                              max = 200000, step = 4000, width = "100%")
        ),
        htmltools::div(
          class = "bq2-setting",
          htmltools::div(class = "bq2-label", "Seed"),
          shiny::numericInput(ns("seed"), NULL, value = 42, min = 1, width = "100%")
        )
      ),
      shiny::uiOutput(ns("settings_readout"))
    ),

    # Main content area
    shiny::uiOutput(ns("main_content"))
  )
}


# Server ----

#' Bayesian Analysis Page Server
#' @param id Module ID
#' @param rv Reactive values from parent
#' @param parent_session Parent session for navigation
#' @keywords internal
bayesian_page_server <- function(id, rv, parent_session) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    state <- shiny::reactiveValues(running = FALSE, last_settings = NULL,
                                   merge_into_ladder = FALSE)

    # Settings panel toggle
    shiny::observeEvent(input$toggle_settings, {
      session$sendCustomMessage("toggleClass", list(
        id = ns("settings_panel"),
        className = "collapsed"
      ))
    })

    # Async ExtendedTask ----
    has_async <- requireNamespace("future", quietly = TRUE) &&
                 requireNamespace("promises", quietly = TRUE)

    # Source path of a dev-loaded canhrQsort (pkgload), so background
    # workers can load the same code; NULL when running the installed package.
    dev_pkg_path <- tryCatch({
      if (requireNamespace("pkgload", quietly = TRUE) &&
          pkgload::is_dev_package("canhrQsort")) {
        find.package("canhrQsort")
      } else NULL
    }, error = function(e) NULL)

    if (has_async) {
      future::plan(future::multisession, workers = 2)

      bayes_task <- shiny::ExtendedTask$new(function(Y, distribution, mode, K,
                                                     k_min, k_max, q,
                                                     iterations, burn, thin,
                                                     max_iterations, seed,
                                                     sigma_scale, lib_paths,
                                                     pkg_path) {
        promises::future_promise({
          .libPaths(lib_paths)
          if (!is.null(pkg_path) && file.exists(file.path(pkg_path, "DESCRIPTION"))) {
            pkgload::load_all(pkg_path, quiet = TRUE)
          } else {
            library(canhrQsort)
          }
          canhrQsort:::run_bayesqm_job(
            Y, distribution, mode, K, k_min, k_max, q,
            iterations, burn, thin, max_iterations, seed, sigma_scale
          )
        }, seed = TRUE)
      })
    }

    # Page-local view state ----
    evidence_open <- shiny::reactiveVal(NULL)   # NULL = decide from the verdict
    stmt_filter <- shiny::reactiveVal("all")

    # Settings footer: what the sampler will actually keep
    output$settings_readout <- shiny::renderUI({
      it <- as.integer(input$iterations %||% 12000)
      bu <- as.integer(input$burn %||% 2000)
      th <- max(1L, as.integer(input$thin %||% 5))
      kept <- max(0L, (it - bu)) %/% th
      htmltools::div(
        class = "bq2-settings-note",
        sprintf("%s iterations − %s burn-in, thinned by %d: %s kept draws",
                format(it, big.mark = ","), format(bu, big.mark = ","),
                th, format(kept, big.mark = ","))
      )
    })

    # Mode: the automatic path is the product ----
    mode_rv <- shiny::reactiveVal("auto")

    output$mode_toggle <- shiny::renderUI({
      mode <- mode_rv()
      htmltools::div(
        class = "bq2-seg",
        shiny::actionButton(
          ns("seg_auto"), "Suggest K",
          class = paste("btn bq2-seg-btn", if (mode == "auto") "active")
        ),
        shiny::actionButton(
          ns("seg_manual"), "I choose K",
          class = paste("btn bq2-seg-btn", if (mode == "manual") "active")
        )
      )
    })

    shiny::observeEvent(input$seg_auto, mode_rv("auto"))
    shiny::observeEvent(input$seg_manual, mode_rv("manual"))

    output$k_controls <- shiny::renderUI({
      if (mode_rv() == "auto") {
        htmltools::div(
          class = "bq2-group",
          htmltools::div(class = "bq2-label", "Search up to"),
          shiny::numericInput(ns("k_max"), NULL,
                              value = shiny::isolate(input$k_max) %||% 5,
                              min = 2, max = 8, width = "72px")
        )
      } else {
        htmltools::div(
          class = "bq2-group",
          htmltools::div(class = "bq2-label", "Factors (K)"),
          shiny::numericInput(ns("k_single"), NULL,
                              value = shiny::isolate(input$k_single) %||% 3,
                              min = 1, max = 8, width = "72px")
        )
      }
    })

    # Run, Workbook, and Methods buttons ----
    output$run_button_area <- shiny::renderUI({
      if (isTRUE(state$running)) {
        return(htmltools::span(
          class = "bq2-status-text",
          shiny::icon("rotate", class = "spin-icon"), " Sampling"
        ))
      }
      if (!rv$data_loaded || is.null(rv$qdata)) {
        return(htmltools::span(class = "bq2-status-text", "Load data first"))
      }
      shiny::actionButton(
        ns("run_bayesian"),
        if (!is.null(rv$bayesian)) "Re-run" else "Run",
        class = "btn-ov2-blue bq2-run",
        title = if (mode_rv() == "auto") {
          paste0("Search K = 2 to ", input$k_max %||% 5)
        } else {
          paste0("Fit exactly K = ", input$k_single %||% 3)
        }
      )
    })

    output$workbook_button_area <- shiny::renderUI({
      shiny::req(rv$data_loaded, !isTRUE(state$running), rv$bayesian)
      shiny::downloadButton(
        ns("download_workbook"), "Workbook",
        class = "btn-ov2-blue bq2-run",
        icon = NULL,
        title = "Every table of the analysis in one Excel workbook"
      )
    })


    output$download_workbook <- shiny::downloadHandler(
      filename = function() {
        paste0("canhrqsort_bayesian_K", rv$bayesian$K, "_",
               format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        shiny::req(rv$bayesian, rv$qdata)
        shiny::withProgress(message = "Building workbook…", value = 0.4, {
          bq_write_workbook(file, rv$bayesian, rv$qdata)
          shiny::incProgress(0.6)
        })
      }
    )

    # Launch the job (control bar + hero button share one path) ----
    gather_settings <- function() {
      q <- as.numeric(input$fdr_q %||% "0.05")
      seed <- input$seed
      if (is.null(seed) || is.na(seed)) seed <- NULL else seed <- as.integer(seed)
      iterations <- as.integer(input$iterations %||% 12000)
      burn <- as.integer(input$burn %||% 2000)
      thin <- as.integer(input$thin %||% 5)
      max_iterations <- max(as.integer(input$max_iterations %||% 48000), iterations)
      # keep the draw grid phase-continuous: (iterations - burn) %% thin == 0
      excess <- (iterations - burn) %% thin
      if (excess != 0) iterations <- iterations - excess
      list(q = q, seed = seed, iterations = iterations, burn = burn,
           thin = thin, max_iterations = max_iterations,
           sigma_scale = as.numeric(input$sigma_scale %||% 1))
    }

    launch_job <- function(mode, K = 3L, k_min = 2L, k_max = 5L,
                           settings = NULL, merge_into_ladder = FALSE) {
      shiny::req(rv$data_loaded, rv$qdata)
      s <- settings %||% gather_settings()
      state$last_settings <- s
      state$merge_into_ladder <- isTRUE(merge_into_ladder)

      Y <- t(as.matrix(rv$qdata@sorts))          # bayesqm wants J x N
      # Compact statement ids keep every table and axis readable; the full
      # statement text is joined back positionally where it is displayed.
      rownames(Y) <- paste0("S", seq_len(nrow(Y)))
      distribution <- rv$qdata@distribution

      state$running <- TRUE

      if (has_async) {
        bayes_task$invoke(Y, distribution, mode, K, k_min, k_max, s$q,
                          s$iterations, s$burn, s$thin, s$max_iterations,
                          s$seed, s$sigma_scale, .libPaths(), dev_pkg_path)
      } else {
        shiny::withProgress(message = "Bayesian analysis (bayesqm)", value = 0.3, {
          result <- tryCatch(
            run_bayesqm_job(Y, distribution, mode, K, k_min, k_max, s$q,
                            s$iterations, s$burn, s$thin, s$max_iterations,
                            s$seed, s$sigma_scale),
            error = function(e) e
          )
          state$running <- FALSE
          deliver_result(result)
        })
      }
    }

    launch_run <- function() {
      if (mode_rv() == "auto") {
        k_max <- as.integer(input$k_max %||% 5)
        if (is.na(k_max) || k_max < 2) k_max <- 2
        launch_job("ladder", k_min = 2L, k_max = min(k_max, 8L))
      } else {
        launch_job("single", K = as.integer(input$k_single %||% 3))
      }
    }

    shiny::observeEvent(input$run_bayesian, launch_run())

    # Verdict follow-up actions: the app performs its recommendations ----

    # single_viewpoint: fit the one-factor solution and fold it in as a rung
    shiny::observeEvent(input$fit_k1, {
      launch_job("single", K = 1L, settings = state$last_settings,
                 merge_into_ladder = TRUE)
    })

    # no_adequate_rung: widen the search (kmax + 2, capped at 8)
    shiny::observeEvent(input$extend_search, {
      b <- rv$bayesian
      shiny::req(b, b$ladder)
      k_next <- min(max(b$ladder$K) + 2L, 8L)
      shiny::updateNumericInput(session, "k_max", value = k_next)
      launch_job("ladder", k_min = 2L, k_max = k_next,
                 settings = state$last_settings)
    })

    # adequate_but_unsupported: re-judge support at a lenient q, no refit
    shiny::observeEvent(input$rejudge_q, {
      b <- rv$bayesian
      shiny::req(b, b$ladder)
      shiny::withProgress(message = "Re-judging support at q = 0.10", value = 0.2, {
        out <- tryCatch(bq_rejudge(b, q2 = 0.10), error = function(e) e)
        shiny::incProgress(0.7)
        if (inherits(out, "error")) {
          session$sendCustomMessage("showToast", list(
            message = paste("Re-judge failed:", conditionMessage(out)),
            type = "error"))
        } else {
          rv$bayesian <- out
          rv$bayesian_results <- tryCatch(
            bayesqm_to_results(out, rv$qdata), error = function(e) NULL)
          session$sendCustomMessage("showToast", list(
            message = "Support re-judged at q = 0.10 from the stored draws (no refit).",
            type = "success"))
        }
      })
    })

    # tension: report either candidate with one click
    shiny::observeEvent(input$report_cand_a, {
      shiny::req(rv$bayesian, rv$bayesian$ladder)
      switch_rung(candidate_ks()$a)
    })
    shiny::observeEvent(input$report_cand_b, {
      shiny::req(rv$bayesian, rv$bayesian$ladder)
      switch_rung(candidate_ks()$b)
    })

    candidate_ks <- function() {
      tab <- rv$bayesian$ladder$selection$table
      a <- tab$K[which(tab$adequate)[1]]
      bK <- tab$K[which(tab$all_supported)[1]]
      list(a = a %||% tab$K[1], b = bK %||% tab$K[nrow(tab)])
    }

    # verdict navigation jumps
    shiny::observeEvent(input$goto_diagnostics, {
      bslib::nav_select("results_tabs", "Checks", session = session)
    })
    shiny::observeEvent(input$goto_claims, {
      bslib::nav_select("results_tabs", "Claims", session = session)
    })

    # Claim strictness lens: re-judge every claim, no refit ----
    apply_q <- function(q2) {
      b <- rv$bayesian
      shiny::req(b)
      if (isTRUE(all.equal(b$q, q2))) return()
      shiny::withProgress(message = sprintf("Re-judging claims at q = %.2f", q2),
                          value = 0.3, {
        out <- tryCatch({
          if (!is.null(b$ladder)) {
            bq_rejudge(b, q2 = q2)
          } else {
            tb <- b$tables
            tb$flags <- suppressMessages(bayesqm::compute_flags(b$fit, q = q2))
            tb$chars <- bayesqm::factor_characteristics(b$fit, q = q2)
            tb$qdc <- if (b$fit$brief$K >= 2) bayesqm::compute_qdc(b$fit, q = q2)
            tb$claims <- bayesqm::claims(b$fit, q = q2)
            b$tables <- tb
            b$q <- q2
            b
          }
        }, error = function(e) e)
        shiny::incProgress(0.6)
        if (inherits(out, "error")) {
          session$sendCustomMessage("showToast", list(
            message = paste("Re-judge failed:", conditionMessage(out)),
            type = "error"))
        } else {
          rv$bayesian <- out
          rv$bayesian_results <- tryCatch(
            bayesqm_to_results(out, rv$qdata), error = function(e) NULL)
          session$sendCustomMessage("showToast", list(
            message = sprintf(
              "Every claim re-judged at q = %.2f from the stored draws (no refit).",
              q2),
            type = "success"))
        }
      })
    }

    shiny::observeEvent(input$qlens_001, apply_q(0.01))
    shiny::observeEvent(input$qlens_005, apply_q(0.05))
    shiny::observeEvent(input$qlens_010, apply_q(0.10))

    # Continue sampling when the gate is not met ----
    shiny::observeEvent(input$continue_sampling, {
      b <- rv$bayesian
      shiny::req(b)
      s <- state$last_settings %||% gather_settings()
      s$max_iterations <- as.integer(s$max_iterations) * 2L
      s$iterations <- s$max_iterations
      shiny::updateNumericInput(session, "iterations", value = s$iterations)
      shiny::updateNumericInput(session, "max_iterations", value = s$max_iterations)
      if (!is.null(b$ladder)) {
        launch_job("ladder", k_min = max(2L, min(b$ladder$K)),
                   k_max = max(b$ladder$K), settings = s)
      } else {
        launch_job("single", K = b$K, settings = s)
      }
    })


    deliver_result <- function(result) {
      merge_k1 <- isTRUE(state$merge_into_ladder)
      state$merge_into_ladder <- FALSE
      arr_sel(NULL)

      if (inherits(result, "error") || inherits(result, "condition")) {
        session$sendCustomMessage("showToast", list(
          message = paste("Bayesian analysis failed:", conditionMessage(result)),
          type = "error"))
        return()
      }

      if (merge_k1 && !is.null(rv$bayesian$ladder) &&
          identical(result$mode, "single")) {
        # Fold the follow-up fit in as an extra reportable rung
        b <- rv$bayesian
        b$ladder$rungs[[as.character(result$K)]] <- list(
          K = result$K, fit = result$fit, tables = result$tables,
          converged = isTRUE(result$fit$gate$converged)
        )
        b$K <- result$K
        b$K_source <- "user"
        b$fit <- result$fit
        b$tables <- result$tables
        rv$bayesian <- b
      } else {
        rv$bayesian <- result
      }

      rv$bayesian_results <- tryCatch(
        bayesqm_to_results(rv$bayesian, rv$qdata),
        error = function(e) {
          message("[bayesqm bridge] ", e$message)
          NULL
        }
      )
      gate <- rv$bayesian$fit$gate
      session$sendCustomMessage("showToast", list(
        message = paste0(
          "Bayesian fit complete: K=", rv$bayesian$K,
          if (isTRUE(gate$converged)) " (convergence gate passed)"
          else " (gate not met; read cautiously)"),
        type = if (isTRUE(gate$converged)) "success" else "warning"
      ))
    }

    if (has_async) {
      shiny::observe({
        status <- bayes_task$status()
        if (status == "success" && isTRUE(state$running)) {
          state$running <- FALSE
          deliver_result(bayes_task$result())
        } else if (status == "error" && isTRUE(state$running)) {
          state$running <- FALSE
          err <- tryCatch(bayes_task$result(), error = function(e) e)
          deliver_result(err)
        }
      })
    }

    # Rung switching ----
    switch_rung <- function(k) {
      b <- rv$bayesian
      if (is.null(b) || is.null(b$ladder)) return()
      if (identical(k, b$K) || !as.character(k) %in% names(b$ladder$rungs)) return()
      arr_sel(NULL)
      rung <- b$ladder$rungs[[as.character(k)]]
      b$K <- k
      b$K_source <- if (!is.na(b$ladder$K_star) && k == b$ladder$K_star) "model" else "user"
      b$fit <- rung$fit
      b$tables <- rung$tables
      rv$bayesian <- b
      rv$bayesian_results <- tryCatch(
        bayesqm_to_results(b, rv$qdata),
        error = function(e) NULL
      )
    }

    lapply(1:8, function(k) {
      shiny::observeEvent(input[[paste0("rung_", k)]], switch_rung(k),
                          ignoreInit = TRUE)
    })

    # Main Content ----
    output$main_content <- shiny::renderUI({

      if (!rv$data_loaded || is.null(rv$qdata)) {
        return(empty_state(
          icon_name = "diagram-project",
          title = "No Data Loaded",
          message = "Load Q-sort data to run the Bayesian analysis",
          action_button = shiny::actionButton(
            ns("go_to_data_bayes"),
            htmltools::tagList(shiny::icon("arrow-right"), "Go to Upload"),
            class = "btn btn-primary"
          )
        ))
      }

      if (isTRUE(state$running)) {
        s <- state$last_settings
        run_desc <- if (identical(mode_rv(), "auto")) {
          paste0("Searching K = 2 to ", input$k_max %||% 5,
                 ", one chain per candidate")
        } else {
          paste0("Fitting K = ", input$k_single %||% 3)
        }
        return(htmltools::div(
          class = "bq2-running",
          htmltools::div(class = "loading-spinner"),
          htmltools::div(class = "bq2-running-title", "Drawing posterior samples"),
          htmltools::p(
            class = "bq2-running-note",
            paste0(run_desc,
                   if (!is.null(s)) paste0(", ", format(s$iterations, big.mark = ","),
                                           " iterations after ",
                                           format(s$burn, big.mark = ","), " burn-in"),
                   ". Sampling continues until the convergence checks pass or the ",
                   "iteration cap is reached. The job runs in the background, so ",
                   "the page can be left and revisited.")
          )
        ))
      }

      if (is.null(rv$bayesian)) {
        plan <- if (mode_rv() == "auto") {
          paste0("Fits K = 2 through ", input$k_max %||% 5,
                 ", checks fit and factor support at each K, and suggests the ",
                 "smallest K that passes both.")
        } else {
          paste0("Fits exactly K = ", input$k_single %||% 3,
                 " and reports it with credible intervals.")
        }
        return(htmltools::p(
          class = "bq2-plan",
          paste0(plan, " The false discovery level for claims is set on the ",
                 "results, where it can be changed without refitting.")
        ))
      }

      bayesian_results_ui()
    })

    shiny::observeEvent(input$go_to_data_bayes, {
      bslib::nav_select("main_nav", "home", session = parent_session)
    })

    # Results UI ----
    fac_ids_of <- function(b) dimnames(b$fit$draws$sigma)[[2]]

    k_phrase <- function(k) {
      words <- c("one factor", "two factors", "three factors", "four factors",
                 "five factors", "six factors", "seven factors", "eight factors")
      if (k >= 1 && k <= 8) words[k] else paste0(k, " factors")
    }

    bayesian_results_ui <- function() {
      b <- rv$bayesian

      panels <- list(
        bslib::nav_panel(
          "Loadings",
          htmltools::div(
            class = "bq2-tab-note",
            paste0("Loadings on a correlation scale with ",
                   round(100 * (b$fit$brief$prob %||% 0.95)),
                   "% credible intervals. Flags are chosen so that at most ",
                   round(100 * b$q),
                   "% of them are expected to be wrong (q = ", b$q, ").")
          ),
          DT::dataTableOutput(ns("loadings_table"))
        ),

        bslib::nav_panel(
          "Statements",
          shiny::uiOutput(ns("stmt_controls")),
          shiny::uiOutput(ns("array_html")),
          shiny::uiOutput(ns("array_pane")),
          DT::dataTableOutput(ns("statements_table"))
        ),

        bslib::nav_panel(
          "Claims",
          htmltools::div(
            class = "bq2-tab-note",
            paste0("Everything this report asserts passes one rule: within each ",
                   "family of claims, the expected share of false claims is held ",
                   "at q = ", b$q, ". Each block reports the expected number of ",
                   "false entries it may contain.")
          ),
          shiny::uiOutput(ns("claims_ui"))
        ),

        bslib::nav_panel(
          "Checks",
          shiny::uiOutput(ns("gate_cards")),
          shiny::plotOutput(ns("convergence_plot"), height = "440px"),
          htmltools::hr(),
          shiny::fluidRow(
            shiny::column(6, shiny::plotOutput(ns("ppc_plot"), height = "360px")),
            shiny::column(6, shiny::plotOutput(ns("person_plot"), height = "360px"))
          ),
          htmltools::hr(),
          htmltools::div(
            class = "bq2-tab-note",
            paste0("Person check: m is agreement with the model, w agreement ",
                   "with the rest of the panel. A sort marked as an uncovered ",
                   "viewpoint is one these factors cannot summarize.")
          ),
          DT::dataTableOutput(ns("persons_table"))
        )
      )

      htmltools::tagList(
        shiny::uiOutput(ns("verdict_block")),
        shiny::uiOutput(ns("evidence_block")),
        shiny::uiOutput(ns("composition_block")),
        do.call(bslib::navset_card_underline,
                c(list(id = ns("results_tabs")), panels))
      )
    }

    # The verdict: plain typography, gate fused into the status line ----
    output$verdict_block <- shiny::renderUI({
      b <- rv$bayesian
      shiny::req(b, b$fit)
      g <- b$fit$gate
      gate_ok <- isTRUE(g$converged)
      is_ladder <- !is.null(b$ladder)

      if (is_ladder) {
        sel <- b$ladder$selection
        verdict <- sel$verdict
        headline <- switch(verdict,
          selected = paste0("The search suggests ", k_phrase(sel$K)),
          single_viewpoint = "One shared viewpoint",
          tension = "Two defensible solutions",
          adequate_but_unsupported = "Fit without full support",
          no_adequate_rung = "No adequate K in this range",
          no_shared_structure = "No shared structure",
          paste0("Reporting ", k_phrase(b$K))
        )
        cand <- if (identical(verdict, "tension")) candidate_ks()
        explanation <- switch(verdict,
          selected = paste0("K = ", sel$K, " is the smallest solution showing no ",
                            "sign of a missing factor, where every factor has at ",
                            "least two flagged sorts and a distinguishing ",
                            "statement at q = ", b$q, "."),
          single_viewpoint = paste0("No multi-factor solution earns support. The ",
                                    "defensible summary is one shared viewpoint",
                                    if ("1" %in% names(b$ladder$rungs)) {
                                      ", reported below."
                                    } else "."),
          tension = paste0("Fit favors K = ", cand$a, " while factor support ",
                           "favors K = ", cand$b, ". Compare both before ",
                           "choosing; reporting both is legitimate. The ",
                           "Reporting control switches every table."),
          adequate_but_unsupported = paste0("Some K passes the fit check, but at ",
                                            "every such K at least one factor lacks either two flagged ",
                                            "sorts or a distinguishing statement at q = ", b$q,
                                            ". Reporting K = ", b$K, " for now."),
          no_adequate_rung = paste0("Every K from ", min(b$ladder$K), " to ",
                                    max(b$ladder$K), " shows signs of a missing ",
                                    "factor. The structure may need a wider search."),
          no_shared_structure = paste0("The sorts do not share viewpoints these ",
                                       "factors can summarize. Reporting K = ", b$K,
                                       " by default; the person check shows who ",
                                       "stands apart."),
          "This solution is on display for inspection."
        )
        action <- switch(verdict,
          single_viewpoint = if (!"1" %in% names(b$ladder$rungs)) {
            shiny::actionButton(ns("fit_k1"), "Fit the one-factor solution",
                                class = "btn-bq2-quiet")
          },
          adequate_but_unsupported = if (b$q < 0.10) {
            shiny::actionButton(ns("rejudge_q"), "Re-judge at q = 0.10",
                                class = "btn-bq2-quiet")
          },
          no_adequate_rung = if (max(b$ladder$K) < 8) {
            shiny::actionButton(
              ns("extend_search"),
              paste0("Extend the search to K = ", min(max(b$ladder$K) + 2L, 8L)),
              class = "btn-bq2-quiet")
          },
          no_shared_structure = shiny::actionButton(
            ns("goto_diagnostics"), "Review the person check",
            class = "btn-bq2-quiet"),
          NULL
        )
      } else {
        headline <- paste0("You chose ", k_phrase(b$K))
        explanation <- paste0("Fitted exactly K = ", b$K,
                              " and reported with credible intervals. The search ",
                              "mode can compare candidates when the choice is open.")
        action <- NULL
      }

      status_line <- htmltools::div(
        class = "bq2-verdict-status",
        htmltools::span(class = paste("bq2-gate-dot", if (!gate_ok) "warn")),
        htmltools::span(
          class = paste("bq2-gate-word", if (!gate_ok) "warn"),
          if (gate_ok) "Gate passed" else "Gate not met"
        ),
        htmltools::span(
          class = "bq2-status-rest",
          paste0("· R-hat ", sprintf("%.2f", g$rhat),
                 " · ESS ", sprintf("%.0f", min(g$ess_bulk, g$ess_tail)),
                 " · claims held at q = ", b$q)
        ),
        if (!gate_ok) {
          shiny::actionLink(ns("continue_sampling"), "Continue sampling",
                            class = "bq2-continue")
        }
      )

      reporting_seg <- if (is_ladder) {
        ks <- sort(as.integer(names(b$ladder$rungs)))
        htmltools::div(
          class = "bq2-group",
          htmltools::div(class = "bq2-label", "Reporting"),
          htmltools::div(
            class = "bq2-seg small",
            lapply(ks, function(k) {
              star <- !is.na(b$ladder$K_star) && k == b$ladder$K_star
              shiny::actionButton(
                ns(paste0("rung_", k)),
                htmltools::HTML(paste0("K = ", k, if (star) " ★" else "")),
                class = paste("btn bq2-seg-btn", if (identical(k, b$K)) "active")
              )
            })
          )
        )
      }

      q_seg <- htmltools::div(
        class = "bq2-group",
        htmltools::div(class = "bq2-label", "Claim strictness"),
        htmltools::div(
          class = "bq2-seg small",
          lapply(c(0.01, 0.05, 0.10), function(qv) {
            shiny::actionButton(
              ns(paste0("qlens_", gsub("\\.", "", sprintf("%.2f", qv)))),
              sprintf("%.2f", qv),
              class = paste("btn bq2-seg-btn",
                            if (isTRUE(all.equal(qv, b$q))) "active")
            )
          })
        ),
        htmltools::div(class = "bq2-subnote", "re-judged from stored draws, no refit")
      )

      htmltools::div(
        class = "bq2-verdict",
        htmltools::div(
          class = "bq2-verdict-main",
          htmltools::div(class = "bq2-verdict-headline", headline),
          status_line,
          htmltools::div(class = "bq2-verdict-body", explanation),
          htmltools::div(
            class = "bq2-verdict-actions",
            action,
            if (is_ladder) {
              open <- evidence_open() %||%
                (b$ladder$selection$verdict %in%
                   c("tension", "adequate_but_unsupported",
                     "no_adequate_rung", "no_shared_structure"))
              shiny::actionLink(
                ns("toggle_evidence"),
                if (open) "Hide the evidence" else "Show the evidence",
                class = "bq2-evidence-link"
              )
            }
          )
        ),
        htmltools::div(class = "bq2-verdict-controls", reporting_seg, q_seg)
      )
    })

    shiny::observeEvent(input$toggle_evidence, {
      b <- rv$bayesian
      current <- evidence_open() %||%
        (b$ladder$selection$verdict %in%
           c("tension", "adequate_but_unsupported",
             "no_adequate_rung", "no_shared_structure"))
      evidence_open(!current)
    })

    # The evidence: one ladder row per fitted K ----
    output$evidence_block <- shiny::renderUI({
      b <- rv$bayesian
      shiny::req(b, b$ladder)
      open <- evidence_open() %||%
        (b$ladder$selection$verdict %in%
           c("tension", "adequate_but_unsupported",
             "no_adequate_rung", "no_shared_structure"))
      if (!open) return(NULL)

      ks <- sort(as.integer(names(b$ladder$rungs)))
      rows <- lapply(ks, function(k) {
        r <- b$ladder$rungs[[as.character(k)]]
        pct <- tryCatch(r$tables$checks$extra_factor$percentile,
                        error = function(e) NA_real_)
        in_band <- !is.na(pct) && pct >= 0.05 && pct <= 0.95
        reported <- identical(k, b$K)
        star <- !is.na(b$ladder$K_star) && k == b$ladder$K_star

        fac_ids <- dimnames(r$fit$draws$sigma)[[2]]
        cl <- r$tables$claims
        supported <- vapply(fac_ids, function(f)
          sum(cl$flags$factor == f) >= 2 &&
            any(cl$distinguishing$factor == f), logical(1))

        support_ui <- if (k == 1) {
          htmltools::span(class = "bq2-rung-status",
                          "single viewpoint, no pairs to distinguish")
        } else {
          htmltools::div(
            class = "bq2-support",
            lapply(seq_along(fac_ids), function(i) {
              htmltools::span(
                class = "bq2-support-item",
                htmltools::span(class = paste("bq2-support-dot",
                                              if (!supported[i]) "hollow")),
                htmltools::span(
                  class = paste("bq2-support-name", if (!supported[i]) "muted"),
                  toupper(fac_ids[i])
                )
              )
            })
          )
        }

        status_txt <- if (reported && star) "Reported · model's pick"
          else if (reported) "Reported"
          else if (star) "model's pick"
          else if (k > 1 && !all(supported)) paste0("support incomplete at q = ", b$q)
          else if (!in_band) "outside the adequate band"
          else ""

        htmltools::div(
          class = paste("bq2-rung", if (reported) "active"),
          onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})",
                            ns("rung_click"), k),
          htmltools::div(
            class = "bq2-rung-k",
            paste0("K = ", k),
            if (!isTRUE(r$converged)) {
              htmltools::div(class = "bq2-rung-gate", "gate not met")
            }
          ),
          htmltools::div(
            class = "bq2-track",
            htmltools::div(class = "bq2-track-band"),
            if (!is.na(pct)) {
              htmltools::div(class = paste("bq2-track-dot", if (!in_band) "out"),
                             style = sprintf("left: %.0f%%;", 100 * pct))
            }
          ),
          htmltools::div(class = "bq2-rung-val",
                         if (!is.na(pct)) sprintf("%.2f", pct) else ""),
          support_ui,
          htmltools::div(
            class = paste("bq2-rung-status right",
                          if (reported || star) "blue"),
            status_txt
          )
        )
      })

      htmltools::div(
        class = "bq2-ladder",
        htmltools::div(
          class = "bq2-ladder-head",
          htmltools::div(),
          htmltools::div(
            class = "bq2-ladder-scale",
            htmltools::span("Extra-factor percentile"),
            htmltools::span(class = "bq2-subnote", "adequate band 0.05 to 0.95")
          ),
          htmltools::div(),
          htmltools::div(class = "bq2-ladder-scale-label", "Factor support"),
          htmltools::div()
        ),
        rows,
        htmltools::div(
          class = "bq2-ladder-caption",
          paste0("Dot inside the shaded band: adequate fit. Filled dot: the ",
                 "factor earns two flagged sorts and a distinguishing statement. ",
                 "Click a row to report that solution.")
        )
      )
    })

    shiny::observeEvent(input$rung_click, switch_rung(as.integer(input$rung_click)))

    # Per-Factor Summary Strip ----
    # Panel composition: one bar, one plain line per factor ----
    output$composition_block <- shiny::renderUI({
      b <- rv$bayesian
      shiny::req(b, b$tables$chars, b$tables$flags)

      chars <- b$tables$chars
      fl <- b$tables$flags
      colors <- get_theme_colors()$factor_colors
      n <- nrow(fl)
      n_unc <- sum(!fl$selected)

      counts <- vapply(chars$factor, function(f)
        sum(fl$selected & fl$factor == f), integer(1))
      pos <- vapply(chars$factor, function(f)
        sum(fl$selected & fl$factor == f & fl$sign > 0), integer(1))
      neg <- counts - pos

      ord <- order(-counts)
      summary_bits <- paste0(counts[ord], " flag ", toupper(chars$factor[ord]))
      summary_line <- paste0(n, " sorts: ", paste(summary_bits, collapse = ", "),
                             if (n_unc > 0) paste0(", ", n_unc, " unclassified"))

      bar <- htmltools::div(
        class = "bq2-comp-bar",
        lapply(ord, function(i) {
          htmltools::div(style = sprintf(
            "width: %.2f%%; background: %s;",
            100 * counts[i] / n, colors[(i - 1) %% length(colors) + 1]))
        }),
        if (n_unc > 0) {
          htmltools::div(style = sprintf(
            "width: %.2f%%; background: #e2e8f0;", 100 * n_unc / n))
        }
      )

      lines <- lapply(ord, function(i) {
        rel <- chars$reliability[i]
        small <- counts[i] < 5 || (!is.na(rel) && rel < 0.5)
        htmltools::div(
          class = "bq2-comp-line",
          htmltools::span(class = "bq2-comp-dot", style = paste0(
            "background:", colors[(i - 1) %% length(colors) + 1], ";")),
          htmltools::span(class = "bq2-comp-name",
                          paste("Factor", toupper(sub("^f", "", chars$factor[i])))),
          htmltools::span(
            class = "bq2-comp-facts",
            paste0(counts[i], " flagged · defining ", chars$defining_modal[i],
                   " [", chars$defining_lower[i], ", ", chars$defining_upper[i], "]",
                   if (!is.na(rel)) paste0(" · reliability ", round(rel, 2))
                   else " · reliability n/a",
                   if (pos[i] > 0 && neg[i] > 0) {
                     paste0(" · ", pos[i], " positive, ", neg[i], " negative")
                   },
                   if (small) " · small factor, interpret its array cautiously")
          )
        )
      })

      htmltools::div(
        class = "bq2-composition",
        htmltools::div(class = "bq2-comp-summary", summary_line),
        bar,
        htmltools::div(class = "bq2-comp-lines", lines)
      )
    })

    # Tables & plots ----

    fmt_num <- function(x) gsub("-", "−", sprintf("%.2f", x), fixed = TRUE)

    fmt_ci <- function(est, lo, hi) {
      paste0(fmt_num(est),
             ' <span class="bq2-ci">[', fmt_num(lo), ", ",
             fmt_num(hi), "]</span>")
    }

    fmt_p <- function(p) {
      ifelse(p > 0.99, "&gt;0.99",
             ifelse(p < 0.01, "&lt;0.01", sprintf("%.2f", p)))
    }

    output$loadings_table <- DT::renderDataTable({
      b <- rv$bayesian
      shiny::req(b)
      lo <- b$tables$loadings
      fl <- b$tables$flags
      fac_ids <- fac_ids_of(b)

      df <- data.frame(Participant = lo$participant, stringsAsFactors = FALSE)
      for (f in fac_ids) {
        df[[paste0(toupper(f), " loading")]] <- fmt_ci(
          lo[[paste0(f, "_loading")]],
          lo[[paste0(f, "_lower")]],
          lo[[paste0(f, "_upper")]]
        )
      }
      df$`Most likely factor` <- paste0(toupper(fl$factor),
                                        ifelse(fl$sign > 0, "+", "−"))
      df$`P(flag)` <- fmt_p(fl$flag_prob)
      df$Flag <- ifelse(
        fl$selected,
        paste0('<span class="bq2-flag">', toupper(fl$factor),
               ifelse(fl$sign > 0, "+", "−"), "</span>"),
        '<span class="bq2-flag-none">unclassified</span>'
      )

      DT::datatable(
        df, escape = FALSE, rownames = FALSE,
        options = list(pageLength = 15, dom = "ft", scrollX = TRUE),
        class = "compact stripe"
      )
    })

    # Statements: unified pyramids plus one filterable table ----
    output$stmt_controls <- shiny::renderUI({
      b <- rv$bayesian
      shiny::req(b)
      f <- stmt_filter()
      seg_btn <- function(id, label, value) {
        shiny::actionButton(
          ns(id), label,
          class = paste("btn bq2-seg-btn", if (identical(f, value)) "active")
        )
      }
      htmltools::div(
        class = "bq2-stmt-controls",
        htmltools::div(
          class = "bq2-seg small",
          seg_btn("stmt_all", "All statements", "all"),
          if (!is.null(b$tables$qdc)) seg_btn("stmt_dist", "Distinguishing", "dist"),
          if (!is.null(b$tables$qdc)) seg_btn("stmt_cons", "Consensus", "cons")
        )
      )
    })

    shiny::observeEvent(input$stmt_all, stmt_filter("all"))
    shiny::observeEvent(input$stmt_dist, stmt_filter("dist"))
    shiny::observeEvent(input$stmt_cons, stmt_filter("cons"))

    # Factor arrays drawn as the Overview pyramid component: same scaled
    # tiles, same rank-ramp colors, click a tile to read the statement.
    arr_sel <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$arr_sel, arr_sel(input$arr_sel))

    arr_tile_text_color <- function(col) {
      rgb_vals <- grDevices::col2rgb(col)
      lum <- (0.299 * rgb_vals[1] + 0.587 * rgb_vals[2] + 0.114 * rgb_vals[3]) / 255
      if (lum > 0.5) "#1e293b" else "#ffffff"
    }

    output$array_html <- shiny::renderUI({
      b <- rv$bayesian
      shiny::req(b)
      ar <- b$tables$array
      cert <- attr(ar, "certainty")
      fac_ids <- fac_ids_of(b)
      C <- length(b$fit$distribution)
      labels_grid <- bq_grid_labels(C)
      ramp <- grDevices::colorRampPalette(ov2_ramp_anchors)(C)
      stmts <- rv$qdata@statements
      fcolors <- get_theme_colors()$factor_colors
      fmt_lab <- function(x) sub("-", "−", as.character(x), fixed = TRUE)
      sel <- arr_sel()
      sel_i <- if (!is.null(sel) && !is.null(sel$s)) as.integer(sel$s) else NA_integer_

      pyramids <- lapply(seq_along(fac_ids), function(k) {
        f <- fac_ids[k]
        grid_col <- ar[[paste0(f, "_grid")]]
        cols_ui <- lapply(seq_len(C), function(col) {
          idx <- which(grid_col == col)
          tiles <- lapply(idx, function(i) {
            cval <- if (is.null(cert)) NA_real_ else cert[i, k]
            sid <- ar$statement[i]
            s_num <- suppressWarnings(as.integer(sub("^S", "", sid)))
            tip <- paste0(sid, " · column ", fmt_lab(labels_grid[col]),
                          if (!is.na(cval)) {
                            paste0(" · certainty ", sprintf("%.2f", cval))
                          },
                          if (!is.null(stmts) && !is.na(s_num) &&
                              s_num <= length(stmts)) {
                            paste0("\n", stmts[s_num])
                          })
            # The ring follows the statement across every pyramid, so one
            # click shows where the same statement sits in each factor.
            is_sel <- !is.na(sel_i) && sel_i == i
            htmltools::div(
              class = paste("ov2-qs-tile", if (is_sel) "sel"),
              style = paste0("background:", ramp[col], ";color:",
                             arr_tile_text_color(ramp[col]), ";"),
              title = tip,
              onclick = sprintf(
                "Shiny.setInputValue('%s', {s: %d, f: %d}, {priority: 'event'})",
                ns("arr_sel"), i, k),
              sid
            )
          })
          htmltools::div(
            class = "ov2-qs-col",
            htmltools::div(class = "ov2-qs-stack", tiles),
            htmltools::div(class = "ov2-qs-axis", fmt_lab(labels_grid[col]))
          )
        })
        mean_cert <- if (is.null(cert)) NA_real_ else mean(cert[, k])
        htmltools::div(
          class = "bq2-array",
          htmltools::div(
            class = "bq2-array-title",
            htmltools::span(class = "bq2-comp-dot", style = paste0(
              "background:", fcolors[(k - 1) %% length(fcolors) + 1], ";")),
            paste("Factor", toupper(sub("^f", "", f)))
          ),
          if (!is.na(mean_cert)) {
            htmltools::div(class = "bq2-array-sub",
                           paste0("mean certainty ", sprintf("%.2f", mean_cert)))
          },
          htmltools::div(
            class = "ov2-qs-wrap",
            style = sprintf("max-width: %dpx;", C * 82),
            htmltools::div(class = "ov2-qs-row", cols_ui),
            htmltools::div(
              class = "ov2-qs-ends",
              htmltools::div("Most disagree"),
              htmltools::div("Most agree")
            )
          )
        )
      })

      htmltools::div(class = "bq2-array-grid", pyramids)
    })

    output$array_pane <- shiny::renderUI({
      b <- rv$bayesian
      shiny::req(b)
      sel <- arr_sel()
      if (is.null(sel) || is.null(sel$s)) {
        return(htmltools::div(class = "ov2-qs-pane empty",
                              "Click a tile to read its statement."))
      }
      ar <- b$tables$array
      z <- b$tables$zscores
      cert <- attr(ar, "certainty")
      fac_ids <- fac_ids_of(b)
      i <- as.integer(sel$s)
      kf <- as.integer(sel$f)
      shiny::req(i >= 1, i <= nrow(ar), kf >= 1, kf <= length(fac_ids))
      C <- length(b$fit$distribution)
      labels_grid <- bq_grid_labels(C)
      ramp <- grDevices::colorRampPalette(ov2_ramp_anchors)(C)
      fmt_lab <- function(x) {
        x <- as.character(x)
        ifelse(as.numeric(x) > 0, paste0("+", x), sub("-", "−", x, fixed = TRUE))
      }

      sid <- ar$statement[i]
      s_num <- suppressWarnings(as.integer(sub("^S", "", sid)))
      stmts <- rv$qdata@statements
      txt <- if (!is.null(stmts) && !is.na(s_num) && s_num <= length(stmts)) {
        stmts[s_num]
      } else "No statement text available for this dataset."

      col_sel <- ar[[paste0(fac_ids[kf], "_grid")]][i]
      badge_bg <- ramp[col_sel]
      fcolors <- get_theme_colors()$factor_colors

      per_factor <- lapply(seq_along(fac_ids), function(k) {
        f <- fac_ids[k]
        colk <- ar[[paste0(f, "_grid")]][i]
        htmltools::div(
          class = "bq2-pane-fact",
          htmltools::span(class = "bq2-comp-dot", style = paste0(
            "background:", fcolors[(k - 1) %% length(fcolors) + 1], ";")),
          htmltools::span(class = "bq2-comp-name",
                          paste("Factor", toupper(sub("^f", "", f)))),
          htmltools::span(
            class = "bq2-comp-facts",
            paste0("column ", fmt_lab(labels_grid[colk]), " · z ",
                   fmt_num(z[[paste0(f, "_zsc")]][i]),
                   " [", fmt_num(z[[paste0(f, "_lower")]][i]), ", ",
                   fmt_num(z[[paste0(f, "_upper")]][i]), "]",
                   if (!is.null(cert)) {
                     paste0(" · certainty ", sprintf("%.2f", cert[i, k]))
                   })
          )
        )
      })

      cols_i <- vapply(fac_ids, function(f)
        as.numeric(ar[[paste0(f, "_grid")]][i]), numeric(1))
      move_line <- if (length(cols_i) >= 2) {
        pairs <- utils::combn(seq_along(cols_i), 2)
        gaps <- abs(cols_i[pairs[1, ]] - cols_i[pairs[2, ]])
        gmax <- max(gaps)
        fname <- function(x) paste("Factor", toupper(sub("^f", "", fac_ids[x])))
        if (gmax == 0) {
          if (length(cols_i) == 2) "Sits in the same column in both factors."
          else "Sits in the same column in every factor."
        } else {
          j <- which.max(gaps)
          if (length(cols_i) == 2) {
            paste0("Moves ", gmax, " column", if (gmax > 1) "s",
                   " between ", fname(1), " and ", fname(2), ".")
          } else {
            paste0("Widest movement: ", gmax, " column", if (gmax > 1) "s",
                   ", between ", fname(pairs[1, j]), " and ",
                   fname(pairs[2, j]), ".")
          }
        }
      }

      htmltools::div(
        class = "ov2-qs-pane",
        htmltools::div(
          class = "ov2-qs-pane-side",
          htmltools::span(class = "ov2-qs-pane-badge",
                          style = paste0("background:", badge_bg, ";color:",
                                         arr_tile_text_color(badge_bg), ";"),
                          sid)
        ),
        htmltools::div(
          class = "bq2-pane-body",
          htmltools::div(class = "ov2-qs-pane-text", txt),
          htmltools::div(class = "bq2-pane-facts", per_factor),
          if (!is.null(move_line)) {
            htmltools::div(class = "bq2-pane-move", move_line)
          }
        )
      )
    })

    output$statements_table <- DT::renderDataTable({
      b <- rv$bayesian
      shiny::req(b)
      z <- b$tables$zscores
      ar <- b$tables$array
      qdc <- b$tables$qdc
      fac_ids <- fac_ids_of(b)
      labels_grid <- bq_grid_labels(length(b$fit$distribution))
      fmt_lab <- function(x) {
        x <- as.character(x)
        ifelse(as.numeric(x) > 0, paste0("+", x), sub("-", "−", x, fixed = TRUE))
      }

      df <- data.frame(Statement = z$statement, stringsAsFactors = FALSE)
      txt <- bq_statement_text(z$statement, rv$qdata@statements, width = 70)
      if (!is.null(txt)) df$Text <- txt
      for (f in fac_ids) {
        df[[paste0(toupper(f), " column")]] <-
          fmt_lab(labels_grid[ar[[paste0(f, "_grid")]]])
        df[[paste0(toupper(f), " z score")]] <- fmt_ci(
          z[[paste0(f, "_zsc")]], z[[paste0(f, "_lower")]],
          z[[paste0(f, "_upper")]]
        )
      }

      if (!is.null(qdc)) {
        m <- match(df$Statement, qdc$statement)
        verdict <- qdc$verdict[m]
        is_dist <- grepl("^distinguishing", verdict)
        is_cons <- verdict == "consensus"
        df$Role <- ""
        df$Role[is_dist] <- paste0(
          '<span class="bq2-role-dist">distinguishes ',
          toupper(sub("^distinguishing ", "", verdict[is_dist])), "</span>")
        df$Role[is_cons] <- '<span class="bq2-role-cons">consensus</span>'

        flt <- stmt_filter()
        if (identical(flt, "dist")) df <- df[is_dist, , drop = FALSE]
        if (identical(flt, "cons")) df <- df[is_cons, , drop = FALSE]
      }

      DT::datatable(
        df, escape = FALSE, rownames = FALSE,
        options = list(pageLength = 15, dom = "ftp", scrollX = TRUE),
        class = "compact stripe"
      )
    })

    output$claims_ui <- shiny::renderUI({
      b <- rv$bayesian
      shiny::req(b)
      cl <- b$tables$claims
      ef <- cl$expected_false

      claim_card <- function(title, tab, ef_val, empty_msg) {
        qsort_card(
          title = paste0(title, " (expected false ", sprintf("%.2f", ef_val), ")"),
          mode = "standard",
          if (is.null(tab) || nrow(tab) == 0) {
            htmltools::p(class = "text-muted", empty_msg)
          } else {
            tab_disp <- tab
            num <- vapply(tab_disp, is.numeric, logical(1))
            tab_disp[num] <- lapply(tab_disp[num], round, 2)
            htmltools::tags$div(
              class = "table-responsive",
              htmltools::tags$table(
                class = "table table-sm characteristics-table",
                htmltools::tags$thead(htmltools::tags$tr(
                  lapply(names(tab_disp), htmltools::tags$th)
                )),
                htmltools::tags$tbody(
                  lapply(seq_len(nrow(tab_disp)), function(i) {
                    htmltools::tags$tr(lapply(unname(as.list(tab_disp[i, ])), function(v)
                      htmltools::tags$td(as.character(v))))
                  })
                )
              )
            )
          }
        )
      }

      htmltools::div(
        style = "display:grid; grid-template-columns:repeat(2, minmax(0,1fr)); gap:16px; align-items:start;",
        claim_card("Participant flags", cl$flags, ef["flags"],
                   "No participant flag clears the FDR rule."),
        claim_card("Distinguishing listings", cl$distinguishing, ef["distinguishing"],
                   "No distinguishing listing is selected."),
        claim_card("Consensus statements", cl$consensus, ef["consensus"],
                   "No consensus statement is selected."),
        claim_card("Pairwise stars", cl$stars, ef["stars"],
                   "No pairwise contrast is selected.")
      )
    })

    output$gate_cards <- shiny::renderUI({
      b <- rv$bayesian
      shiny::req(b)
      g <- b$fit$gate
      al <- b$fit$align

      htmltools::div(
        class = "metrics-strip",
        metric_item(sprintf("%.3f", g$rhat), "Max R-hat", "wave-square"),
        metric_item(sprintf("%.0f", min(g$ess_bulk, g$ess_tail)), "Min ESS", "chart-column"),
        metric_item(format(g$iterations, big.mark = ","),
                    paste0("Iterations", if (isTRUE(g$extended)) " (extended)" else ""),
                    "rotate"),
        metric_item(sprintf("%.2f", mean(al$congruence)), "Mean alignment congruence", "compress"),
        metric_item(if (isTRUE(g$converged)) "Passed" else "Not met", "Convergence gate",
                    if (isTRUE(g$converged)) "circle-check" else "triangle-exclamation")
      )
    })

    output$convergence_plot <- shiny::renderPlot({
      b <- rv$bayesian
      shiny::req(b)
      bayesqm::plot_convergence(b$fit)
    }, res = 96)

    output$ppc_plot <- shiny::renderPlot({
      b <- rv$bayesian
      shiny::req(b)
      bayesqm::plot_ppc(b$tables$checks)
    }, res = 96)

    output$person_plot <- shiny::renderPlot({
      b <- rv$bayesian
      shiny::req(b)
      bayesqm::plot_person_check(b$tables$persons)
    }, res = 96)

    output$persons_table <- DT::renderDataTable({
      b <- rv$bayesian
      shiny::req(b)
      pc <- b$tables$persons
      df <- data.frame(
        Participant = pc$participant,
        `Model agreement m` = round(pc$m, 2),
        `Person agreement w` = round(pc$w, 2),
        `Nearest partner` = pc$partner,
        Verdict = ifelse(pc$verdict == "fits",
                         '<span class="bq2-flag">fits</span>',
                         paste0('<span class="bq2-verdict-warn">',
                                gsub("unspanned", "viewpoint not covered",
                                     gsub("_", " ", pc$verdict)), '</span>')),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::datatable(
        df, escape = FALSE, rownames = FALSE,
        options = list(pageLength = 15, dom = "ft", scrollX = TRUE),
        class = "compact stripe"
      )
    })

  })
}
