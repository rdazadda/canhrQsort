#' @title Full Factor Analysis Workbook
#' @description One workbook carrying the complete factor analysis, sheet by
#' sheet, so a reader can audit every number behind the results.
#' @name export-workbook
#' @keywords internal
NULL

#' Export the Complete Factor Analysis Workbook
#'
#' @description
#' Write the full factor analysis to an Excel workbook: overview, statements,
#' raw sorts, correlations, unrotated loadings, cumulative communalities,
#' flagged loadings, per-sort descriptives, factor score ranks and
#' correlations, per-factor weights, defining-sort correlations and score
#' sheets, pairwise difference arrays, consensus versus disagreement, factor
#' characteristics, standard errors of differences, distinguishing and
#' consensus statements, and per-factor relative rankings.
#'
#' @param results A QsortResults object from [qsort_analyze()]
#' @param file Path of the .xlsx file to write
#'
#' @return Invisibly, the file path
#' @export
export_factor_workbook <- function(results, file) {

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    rlang::abort("The openxlsx package is required for the workbook export")
  }

  qdata <- results@data
  sorts <- qdata@sorts
  n <- nrow(sorts)
  J <- ncol(sorts)
  k <- results@n_factors
  loadings <- results@rotation$loadings
  flags <- results@flagging
  fs <- results@factor_scores
  md <- results@method_details
  dist_obj <- md$distinction
  chars <- results@factor_characteristics$characteristics
  sed <- results@factor_characteristics$sed_matrix
  fnames <- paste0("Factor ", seq_len(k))
  participants <- qdata@participants
  stmts <- data.frame(
    `Statement number` = seq_len(J),
    Statement = as.character(qdata@statements),
    check.names = FALSE
  )

  wb <- openxlsx::createWorkbook()
  header_style <- openxlsx::createStyle(textDecoration = "bold",
                                        border = "bottom")
  add <- function(name, df) {
    openxlsx::addWorksheet(wb, name)
    openxlsx::writeData(wb, name, df, headerStyle = header_style)
    openxlsx::freezePane(wb, name, firstActiveRow = 2)
    openxlsx::setColWidths(wb, name, cols = seq_along(df), widths = "auto")
  }

  # Overview
  add("Overview", data.frame(
    Item = c("Project", "Statements", "Sorts", "Factors", "Extraction",
             "Rotation", "Correlation", "Flagging", "Exported"),
    Value = c(qdata@source, J, n, k, md$extraction, md$rotation,
              md$cor_method,
              sprintf("auto at p < %.2f%s", md$flag_p_level %||% 0.05,
                      if (isTRUE(md$flag_majority %||% TRUE)) {
                        ", majority of common variance"
                      } else {
                        ""
                      }),
              format(Sys.time(), "%Y-%m-%d %H:%M")),
    stringsAsFactors = FALSE
  ))

  # Statements and raw sorts. Columns carry statement numbers, not the full
  # texts; the number-to-text mapping is the Statements sheet.
  add("Statements", stmts)
  sorts_out <- sorts
  colnames(sorts_out) <- paste0("S", seq_len(J))
  add("Q sorts", data.frame(Participant = participants, sorts_out,
                            check.names = FALSE, stringsAsFactors = FALSE))

  # Sort-by-sort correlations
  cm <- round(results@correlation, 4)
  add("Correlations",
      data.frame(Participant = rownames(cm), cm, check.names = FALSE))

  # Unrotated loadings and cumulative communalities
  uw <- results@extraction$unrotated_wide
  if (!is.null(uw)) {
    add("Unrotated loadings", data.frame(
      Participant = rownames(uw), round(uw, 4), check.names = FALSE
    ))
    cc <- results@extraction$cumulative_communalities
    add("Cumulative communalities", data.frame(
      Participant = rownames(cc), round(cc, 4), check.names = FALSE
    ))
  }

  # Rotated loadings with defining sorts marked
  fl <- data.frame(Participant = participants,
                   check.names = FALSE, stringsAsFactors = FALSE)
  for (f in seq_len(k)) {
    fl[[paste0("F", f)]] <- round(loadings[, f], 4)
    fl[[paste0("F", f, " defining")]] <- ifelse(flags[, f], "yes", "")
  }
  fl$h2 <- round(rowSums(loadings^2), 4)
  add("Loadings", fl)

  # The same table sorted into factor groups
  dominant <- max.col(abs(loadings), ties.method = "first")
  grp <- ifelse(rowSums(flags) > 0,
                paste0("F", max.col(flags + 0, ties.method = "first")),
                "None")
  ord <- order(match(grp, c(paste0("F", seq_len(k)), "None")),
               -abs(loadings[cbind(seq_len(n), dominant)]))
  add("Loadings by factor",
      cbind(data.frame(Group = grp[ord]), fl[ord, ]))

  # Per-sort descriptives
  fd <- sort_descriptives(qdata)
  add("Sort descriptives", data.frame(
    Participant = fd$participant,
    Mean = round(fd$mean, 3), SD = round(fd$sd, 3),
    check.names = FALSE
  ))

  # Factor scores with ranks
  fsr <- stmts
  for (f in seq_len(k)) {
    fsr[[paste0("F", f, " z")]] <- round(fs[[paste0("F", f, "_zscore")]], 3)
    fsr[[paste0("F", f, " q")]] <- fs[[paste0("F", f, "_score")]]
    fsr[[paste0("F", f, " rank")]] <- fs[[paste0("F", f, "_rank")]]
  }
  add("Score ranks", fsr)

  # Factor score correlations
  zmat <- as.matrix(fs[, paste0("F", seq_len(k), "_zscore"), drop = FALSE])
  fsc <- round(stats::cor(zmat, use = "pairwise.complete.obs"), 4)
  dimnames(fsc) <- list(fnames, fnames)
  add("Score correlations",
      data.frame(Factor = fnames, fsc, check.names = FALSE))

  # Per factor: weights, defining-sort correlations, full score sheet
  weights <- attr(fs, "factor_weights")
  def_corr <- defining_sort_correlations(results)
  for (f in seq_len(k)) {
    idx <- which(flags[, f])
    w <- weights[[paste0("F", f)]]
    if (!is.null(w) && length(idx) > 0) {
      add(sprintf("F%d weights", f), data.frame(
        Participant = participants[idx],
        Weight = round(unname(w), 4), check.names = FALSE
      ))
      sub <- round(def_corr[[f]]$correlations, 4)
      add(sprintf("F%d defining correlations", f), data.frame(
        Participant = rownames(sub), sub, check.names = FALSE
      ))
    }
    sheet_f <- stmts
    sheet_f[["z"]] <- round(fs[[paste0("F", f, "_zscore")]], 3)
    sheet_f[["q"]] <- fs[[paste0("F", f, "_score")]]
    for (i in idx) {
      sheet_f[[paste0("Sort ", participants[i])]] <- as.numeric(sorts[i, ])
    }
    ordf <- order(-sheet_f[["z"]])
    add(sprintf("F%d scores", f), sheet_f[ordf, ])
  }

  # Pairwise difference arrays, descending
  if (k >= 2) {
    for (f in seq_len(k - 1)) {
      for (g in (f + 1):k) {
        d <- stmts
        d[[paste0("F", f, " z")]] <- round(fs[[paste0("F", f, "_zscore")]], 3)
        d[[paste0("F", g, " z")]] <- round(fs[[paste0("F", g, "_zscore")]], 3)
        d$Difference <- round(d[[paste0("F", f, " z")]] -
                                d[[paste0("F", g, " z")]], 3)
        add(sprintf("Differences F%d-F%d", f, g),
            d[order(-d$Difference), ])
      }
    }
  }

  # Consensus versus disagreement, sorted by ranking variance
  if (!is.null(dist_obj)) {
    cd <- stmts
    for (f in seq_len(k)) {
      cd[[paste0("F", f, " q")]] <- fs[[paste0("F", f, "_score")]]
    }
    cd[["Ranking variance"]] <- round(unname(dist_obj$ranking_variance), 4)
    add("Consensus vs disagreement", cd[order(cd[["Ranking variance"]]), ])
  }

  # Factor characteristics
  fc <- data.frame(
    Item = c("Defining sorts", "Average reliability coefficient",
             "Composite reliability", "SE of factor z-scores"),
    stringsAsFactors = FALSE
  )
  for (f in seq_len(k)) {
    fc[[fnames[f]]] <- c(
      chars$n_flagged[f],
      md$av_rel_coef %||% 0.8,
      round(chars$composite_reliability[f], 3),
      round(chars$se_factor[f], 3)
    )
  }
  add("Factor characteristics", fc)

  # Standard errors of differences
  sed_r <- round(sed, 3)
  dimnames(sed_r) <- list(fnames, fnames)
  add("Standard errors of differences",
      data.frame(Factor = fnames, sed_r, check.names = FALSE))

  # Distinguishing statements per factor, consensus statements
  if (!is.null(dist_obj)) {
    for (f in seq_len(k)) {
      d_stmts <- dist_obj$distinguishing[[f]]
      stars <- dist_obj$distinguishing_significance[[f]]
      if (length(d_stmts) > 0) {
        rows_i <- match(d_stmts, fs$statement)
        dd <- data.frame(
          `Statement number` = rows_i,
          Statement = as.character(qdata@statements)[rows_i],
          Significance = stars,
          check.names = FALSE
        )
        for (g in seq_len(k)) {
          dd[[paste0("F", g, " z")]] <- round(
            fs[[paste0("F", g, "_zscore")]][rows_i], 3
          )
          dd[[paste0("F", g, " q")]] <- fs[[paste0("F", g, "_score")]][rows_i]
        }
      } else {
        dd <- data.frame(Note = "No distinguishing statements for this factor")
      }
      add(sprintf("Distinguishing F%d", f), dd)
    }

    lv <- dist_obj$consensus_level
    keep <- which(lv != "")
    cs <- if (length(keep) > 0) {
      data.frame(
        `Statement number` = keep,
        Statement = as.character(qdata@statements)[keep],
        Level = unname(lv[keep]),
        check.names = FALSE
      )
    } else {
      data.frame(Note = "No consensus statements")
    }
    add("Consensus statements", cs)
  }

  # Relative rankings (crib sheets) per factor
  for (f in seq_len(k)) {
    cs_f <- tryCatch(generate_crib_sheet(results, factor = f),
                     error = function(e) NULL)
    if (is.null(cs_f)) next
    sec <- function(df, label) {
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df$Section <- label
      df
    }
    parts <- list(
      sec(cs_f$most_agree, "Highest ranked"),
      sec(cs_f$higher_than_all, "Ranked higher than in other arrays"),
      sec(cs_f$lower_than_all, "Ranked lower than in other arrays"),
      sec(cs_f$most_disagree, "Lowest ranked")
    )
    parts <- parts[!vapply(parts, is.null, logical(1))]
    if (length(parts) > 0) {
      common <- Reduce(intersect, lapply(parts, names))
      rel <- do.call(rbind, lapply(parts, function(p) p[, common, drop = FALSE]))
      add(sprintf("Relative rankings F%d", f), rel)
    }
  }

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}