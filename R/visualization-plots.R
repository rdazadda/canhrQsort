#' Unified Visualization Plot Functions
#'
#' Base ggplot2 builders. The dashboard and the downloads call the same
#' functions, so they always match.
#'
#' @name visualization-plots
#' @keywords internal
NULL

#' Create Z-Score Comparison Plot
#'
#' Dotchart comparing z-scores across factors, ordered by disagreement,
#' with distinguishing statements highlighted.
#'
#' @param results QsortResults object
#' @param qdata QsortData object
#' @param theme_colors Optional theme colors
#' @return ggplot2 object
#' @export
create_plot_zscore <- function(results, qdata, theme_colors = NULL) {
  factor_colors <- get_theme_colors()$factor_colors

  factor_scores <- results@factor_scores
  if (is.null(factor_scores)) return(NULL)

  n_factors <- results@n_factors
  statements <- qdata@statements

  distinguishing <- results@distinguishing
  consensus <- results@consensus

  zscore_cols <- grep("_zscore$", names(factor_scores), value = TRUE)
  if (length(zscore_cols) == 0) return(NULL)

  plot_data <- data.frame(statement_num = seq_len(nrow(factor_scores)))
  for (i in seq_along(zscore_cols)) {
    plot_data[[paste0("F", i)]] <- factor_scores[[zscore_cols[i]]]
  }
  plot_data$statement_text <- if (length(statements) >= nrow(factor_scores)) {
    statements[plot_data$statement_num]
  } else {
    paste0("S", plot_data$statement_num)
  }

  # Disagreement score (range of z-scores across factors)
  if (length(zscore_cols) >= 2) {
    zmat <- as.matrix(factor_scores[, zscore_cols])
    plot_data$disagreement <- apply(zmat, 1, function(x) max(x) - min(x))
  } else {
    plot_data$disagreement <- abs(plot_data$F1)
  }

  plot_data$is_consensus <- plot_data$statement_num %in% which(statements %in% consensus)
  plot_data <- plot_data[order(plot_data$disagreement), ]
  plot_data$y <- seq_len(nrow(plot_data))

  # One shape; the factor colors carry the identity, filled = distinguishing
  shapes <- rep(21, 8)

  used_factors <- seq_len(min(n_factors, length(zscore_cols), length(shapes), length(factor_colors)))

  point_data <- do.call(rbind, lapply(used_factors, function(f) {
    col_name <- paste0("F", f)
    dist_idx <- if (f <= length(distinguishing)) which(statements %in% distinguishing[[f]]) else integer(0)
    is_dist <- plot_data$statement_num %in% dist_idx

    data.frame(
      factor_label = paste0("Factor ", f),
      x = plot_data[[col_name]],
      y = plot_data$y,
      is_dist = is_dist,
      is_consensus = plot_data$is_consensus,
      statement_num = plot_data$statement_num,
      statement_text = plot_data$statement_text,
      stringsAsFactors = FALSE
    )
  }))
  point_data$factor_label <- factor(point_data$factor_label, levels = paste0("Factor ", used_factors))

  # Y-axis labels with consensus marker
  y_labels <- paste0("S", plot_data$statement_num)
  y_labels[plot_data$is_consensus] <- paste0(y_labels[plot_data$is_consensus], " *")

  p <- ggplot2::ggplot(point_data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_vline(xintercept = 0, color = "#374151", linewidth = 1) +
    ggplot2::geom_hline(yintercept = plot_data$y, color = "#F3F4F6", linewidth = 0.4) +
    # Connecting lines between factors for each statement
    ggplot2::geom_line(
      ggplot2::aes(group = statement_num),
      color = "#D1D5DB", linewidth = 0.3, alpha = 0.6
    ) +
    # Distinguishing points (filled)
    ggplot2::geom_point(
      data = point_data[point_data$is_dist, ],
      ggplot2::aes(color = factor_label, fill = factor_label, shape = factor_label),
      size = 4.4, stroke = 1.4
    ) +
    # Non-distinguishing points (hollow)
    ggplot2::geom_point(
      data = point_data[!point_data$is_dist, ],
      ggplot2::aes(color = factor_label, shape = factor_label),
      size = 3.4, fill = "white", stroke = 1.3
    ) +
    ggplot2::scale_color_manual(
      values = stats::setNames(factor_colors[used_factors], paste0("Factor ", used_factors)),
      name = NULL
    ) +
    ggplot2::scale_fill_manual(
      values = stats::setNames(factor_colors[used_factors], paste0("Factor ", used_factors)),
      guide = "none"
    ) +
    ggplot2::scale_shape_manual(
      values = stats::setNames(shapes[used_factors], paste0("Factor ", used_factors)),
      name = NULL
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-3.5, 3.5),
      breaks = -3:3,
      labels = c("-3", "-2", "-1", "0", "+1", "+2", "+3")
    ) +
    ggplot2::scale_y_continuous(breaks = plot_data$y, labels = y_labels) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(color = "#E5E7EB", linewidth = 0.3),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 8.5, color = "#334155"),
      axis.text.x = ggplot2::element_text(size = 11, color = "#334155", face = "bold"),
      axis.title.x = ggplot2::element_text(size = 12, color = "#475569", margin = ggplot2::margin(t = 10)),
      legend.position = "top",
      legend.justification = "left",
      legend.text = ggplot2::element_text(size = 11),
      legend.key.size = ggplot2::unit(1.2, "lines"),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.title = ggplot2::element_text(hjust = 0, face = "bold", size = 17, color = "#00274C",
                                         margin = ggplot2::margin(b = 2)),
      plot.subtitle = ggplot2::element_text(hjust = 0, size = 11.5, color = "#64748b",
                                            margin = ggplot2::margin(b = 4)),
      plot.caption = ggplot2::element_text(hjust = 0, size = 10.5, color = "#64748b",
                                           margin = ggplot2::margin(t = 10)),
      plot.margin = ggplot2::margin(8, 10, 8, 8)
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(size = 4.4))
    ) +
    ggplot2::labs(
      title = "Statement Scores Across Factors",
      subtitle = paste0("ordered by disagreement between factors · ", nrow(plot_data), " statements"),
      caption = "filled = distinguishing (p < 0.05) · * = consensus statement",
      x = "z-score", y = NULL
    )

  p
}

#' Create Statement Ranking Plot
#'
#' @param results QsortResults object
#' @param qdata QsortData object
#' @param factor_num Factor number
#' @return ggplot2 object
#' @export
create_plot_ranking <- function(results, qdata, factor_num = 1) {
  factor_scores <- results@factor_scores
  if (is.null(factor_scores)) return(NULL)

  statements <- qdata@statements

  zscore_col <- paste0("F", factor_num, "_zscore")
  if (!zscore_col %in% names(factor_scores)) {
    zscore_cols <- grep("_zscore$", names(factor_scores), value = TRUE)
    zscore_col <- if (length(zscore_cols) >= factor_num) zscore_cols[factor_num] else zscore_cols[1]
  }
  scores <- factor_scores[[zscore_col]]

  df <- data.frame(
    stmt = seq_along(scores),
    score = scores,
    text = substr(statements, 1, 40)
  )
  df <- df[order(df$score, decreasing = TRUE), ]
  df$label <- factor(paste0("S", df$stmt), levels = rev(paste0("S", df$stmt)))
  # Create proper categorical variable for legend
  df$position <- factor(
    ifelse(df$score > 0, "Agree", ifelse(df$score < 0, "Disagree", "Neutral")),
    levels = c("Agree", "Neutral", "Disagree")
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(x = score, y = label)) +
    ggplot2::geom_col(ggplot2::aes(fill = position), width = 0.74) +
    ggplot2::scale_fill_manual(
      values = c("Agree" = "#236192", "Neutral" = "#cbd5e1",
                 "Disagree" = "#DF6A2E"),
      name = NULL
    ) +
    ggplot2::geom_vline(xintercept = 0, color = "#475569", linewidth = 0.5) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 8.5, color = "#334155"),
      axis.title = ggplot2::element_text(color = "#475569"),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.title = ggplot2::element_text(hjust = 0, face = "bold", size = 17,
                                         color = "#00274C",
                                         margin = ggplot2::margin(b = 2)),
      plot.subtitle = ggplot2::element_text(hjust = 0, size = 11.5,
                                            color = "#64748b",
                                            margin = ggplot2::margin(b = 12)),
      legend.position = "top",
      legend.justification = "left"
    ) +
    ggplot2::labs(
      title = paste0("Factor ", factor_num, " · Statement Rankings"),
      subtitle = sprintf("%d statements by z-score", nrow(df)),
      x = "z-score", y = NULL
    )

  p
}

#' Create Scree Plot
#'
#' @param results QsortResults object
#' @param qdata QsortData object
#' @return ggplot2 object
#' @export
create_plot_scree <- function(results, qdata) {
  # The same seeded permutation test the Frequentist page reports, so the
  # figure and the page never disagree about the suggested k
  pa_df <- tryCatch({
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      old_seed <- get(".Random.seed", envir = globalenv())
      on.exit(assign(".Random.seed", old_seed, envir = globalenv()),
              add = TRUE)
    }
    set.seed(42)
    parallel_analysis(qdata, n_iter = 100)
  }, error = function(e) NULL)
  if (is.null(pa_df) || nrow(pa_df) == 0) return(NULL)

  eigenvalues <- pmax(pa_df$eigenvalue, 0)
  n_comp <- length(eigenvalues)
  n_show <- min(12, n_comp)
  n_ext <- results@n_factors

  var_exp <- eigenvalues / sum(eigenvalues) * 100
  cum_var <- cumsum(var_exp)
  suggested <- max(1, sum(pa_df$eigenvalue > pa_df$threshold))

  df <- data.frame(
    comp = seq_len(n_show),
    eigen = eigenvalues[1:n_show],
    pa = pa_df$threshold[1:n_show],
    var = var_exp[1:n_show],
    extracted = seq_len(n_show) <= n_ext
  )

  p <- ggplot2::ggplot(df) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dotted",
                        color = "#94a3b8", linewidth = 0.5) +
    ggplot2::geom_line(ggplot2::aes(x = comp, y = pa), linetype = "dashed",
                       color = "#DF6A2E", linewidth = 0.9) +
    ggplot2::geom_point(ggplot2::aes(x = comp, y = pa), shape = 23,
                        fill = "#DF6A2E", color = "#DF6A2E", size = 3.2) +
    ggplot2::geom_line(ggplot2::aes(x = comp, y = eigen),
                       color = "#236192", linewidth = 1.3) +
    ggplot2::geom_point(ggplot2::aes(x = comp, y = eigen),
                        color = "#236192", size = 4.4) +
    ggplot2::geom_point(data = df[df$extracted, ], ggplot2::aes(x = comp, y = eigen),
                        shape = 21, fill = NA, color = "#FFB800",
                        size = 8, stroke = 2.4) +
    ggplot2::geom_text(ggplot2::aes(x = comp, y = eigen,
                                    label = paste0(round(var, 1), "%")),
                       vjust = -1.5, size = 3.4, color = "#475569") +
    ggplot2::scale_x_continuous(breaks = 1:n_show) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.04, 0.12))) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.title = ggplot2::element_text(hjust = 0, face = "bold", size = 17,
                                         color = "#00274C",
                                         margin = ggplot2::margin(b = 2)),
      plot.subtitle = ggplot2::element_text(hjust = 0, size = 11.5,
                                            color = "#64748b",
                                            margin = ggplot2::margin(b = 12)),
      axis.title = ggplot2::element_text(color = "#475569")
    ) +
    ggplot2::labs(
      title = "Scree · Parallel Analysis",
      subtitle = sprintf("blue observed · orange parallel threshold · gold rings mark the %d extracted (%s%% of variance) · parallel analysis suggests %d",
                         n_ext, round(cum_var[n_ext], 1), suggested),
      x = "Component", y = "Eigenvalue"
    )

  p
}

#' Draw a q-sort card grid on the deck ramp
#'
#' The shared renderer behind the frequentist and Bayesian factor arrays:
#' navy column headers, white cards with a ramp-colored band holding the
#' statement number (and an optional tag), black statement text capped at
#' three lines, and a black foot line per card.
#'
#' @param cards data.frame with col (1-based grid column), stmt_idx,
#'   stmt_text, foot (foot-line label), tag (band-right marker or NA),
#'   ord (stacking order within a column, larger on top)
#' @param distribution Deck distribution (cards per column)
#' @param title,subtitle,caption Figure text
#' @return ggplot2 object
#' @noRd
qsort_card_grid <- function(cards, distribution, title, subtitle, caption) {

  n_cols <- length(distribution)
  score_range <- bq_grid_labels(n_cols)
  max_height <- max(distribution)

  # Tiles wear the deck's own ramp, exactly like the app's Q-sort pyramids:
  # most disagree in deep navy through to most agree in orange. The ramp
  # lives in a band on each card; the text sits black on white beneath it.
  ramp <- grDevices::colorRampPalette(ov2_ramp_anchors)(n_cols)

  # Stack within each column, larger ord on top
  cards <- cards[order(cards$col, -cards$ord), ]
  cards$row <- stats::ave(seq_len(nrow(cards)), cards$col, FUN = seq_along)

  cards$band <- ramp[cards$col]
  cards$band_txt <- vapply(cards$band, contrast_text_color, character(1))

  card_w <- 0.96
  card_h <- 0.94
  band_h <- 0.24
  cards$x <- cards$col
  cards$y <- max_height - cards$row + 1
  cards$y_band <- cards$y + card_h / 2 - band_h / 2
  cards$y_text <- cards$y - band_h / 2 + 0.04

  header_data <- data.frame(
    x = seq_along(score_range),
    score = score_range,
    y = max_height + 0.78
  )
  tagged <- cards[!is.na(cards$tag), ]

  p <- ggplot2::ggplot() +
    # Column headers in navy
    ggplot2::geom_tile(
      data = header_data,
      ggplot2::aes(x = x, y = y),
      fill = "#00274C",
      width = card_w, height = 0.42
    ) +
    ggplot2::geom_text(
      data = header_data,
      ggplot2::aes(x = x, y = y, label = as.character(score)),
      color = "white", fontface = "bold", size = 5.4
    ) +
    # Card body: white, quiet border
    ggplot2::geom_tile(
      data = cards,
      ggplot2::aes(x = x, y = y),
      fill = "white", color = "#dbe3ea",
      width = card_w, height = card_h,
      linewidth = 0.6
    ) +
    # Deck-ramp band across the top of each card
    ggplot2::geom_tile(
      data = cards,
      ggplot2::aes(x = x, y = y_band),
      fill = cards$band,
      width = card_w, height = band_h
    ) +
    # Statement number on the band's left
    ggplot2::geom_text(
      data = cards,
      ggplot2::aes(x = x - card_w / 2 + 0.07, y = y_band,
                   label = paste0("S", stmt_idx)),
      color = cards$band_txt, fontface = "bold",
      size = 2.9, hjust = 0
    ) +
    # Tag on the band's right
    ggplot2::geom_text(
      data = tagged,
      ggplot2::aes(x = x + card_w / 2 - 0.07, y = y_band, label = tag),
      color = tagged$band_txt, fontface = "bold",
      size = 2.9, hjust = 1
    ) +
    # Statement text, black on white, capped at three lines
    ggplot2::geom_text(
      data = cards,
      ggplot2::aes(x = x, y = y_text,
                   label = vapply(stmt_text, function(t) {
                     w <- strwrap(t, width = 19)
                     if (length(w) > 3) {
                       w <- w[1:3]
                       w[3] <- paste0(substr(w[3], 1, 15), "...")
                     }
                     paste(w, collapse = "\n")
                   }, character(1))),
      color = "black", size = 2.75, lineheight = 0.9, vjust = 0.5
    ) +
    # Foot line at the card base
    ggplot2::geom_text(
      data = cards,
      ggplot2::aes(x = x, y = y - card_h / 2 + 0.10, label = foot),
      color = "black", size = 2.4, fontface = "bold"
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.008)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.02)) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.title = ggplot2::element_text(hjust = 0, face = "bold", size = 17,
                                         color = "#00274C",
                                         margin = ggplot2::margin(b = 2)),
      plot.subtitle = ggplot2::element_text(hjust = 0, size = 11.5,
                                            color = "#64748b",
                                            margin = ggplot2::margin(b = 10)),
      plot.caption = ggplot2::element_text(hjust = 0, size = 10.5,
                                           color = "#64748b",
                                           margin = ggplot2::margin(t = 12)),
      plot.margin = ggplot2::margin(8, 8, 8, 8)
    ) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption)

  p
}

#' Create Factor Array Plot (Model Q-Sort)
#'
#' The model Q-sort as statement cards on the study's own grid, drawn by
#' the shared card renderer.
#'
#' @param results QsortResults object
#' @param qdata QsortData object
#' @param factor_num Factor number
#' @return ggplot2 object
#' @export
create_plot_factor_array <- function(results, qdata, factor_num = 1) {
  factor_array <- generate_factor_array(results, factor_num = factor_num)
  if (is.null(factor_array) || is.null(factor_array$array)) return(NULL)

  distribution <- factor_array$distribution
  statements <- qdata@statements
  array_data <- factor_array$array

  n_cols <- length(distribution)
  score_range <- bq_grid_labels(n_cols)

  distinguishing <- results@distinguishing
  dist_idx <- if (factor_num <= length(distinguishing)) {
    which(statements %in% distinguishing[[factor_num]])
  } else {
    integer(0)
  }

  txt <- as.character(array_data$statement)
  long <- nchar(txt) > 52
  txt[long] <- paste0(substr(txt[long], 1, 49), "...")

  cards <- data.frame(
    col = match(array_data$score, score_range),
    stmt_idx = array_data$statement_num,
    stmt_text = txt,
    foot = sprintf("z = %.2f", array_data$zscore),
    tag = ifelse(array_data$statement_num %in% dist_idx, "D", NA_character_),
    ord = array_data$zscore,
    stringsAsFactors = FALSE
  )
  cards <- cards[!is.na(cards$col), ]
  if (nrow(cards) == 0) return(NULL)

  qsort_card_grid(
    cards, distribution,
    title = paste0("Factor ", factor_num, " · Model Q-Sort"),
    subtitle = sprintf("%d statements · most disagree to most agree",
                       nrow(cards)),
    caption = "D = distinguishing statement"
  )
}

#' Posterior factor array as the same card grid
#'
#' The Bayesian reported array drawn exactly like the frequentist Factor
#' Array: same cards, same ramp bands, with the posterior mean score at
#' each card's foot.
#'
#' @param fit bayesqm fit object
#' @param qdata QsortData object (statement texts)
#' @param factor_num Factor number
#' @return ggplot2 object
#' @noRd
create_plot_bq_factor_array <- function(fit, qdata = NULL, factor_num = 1) {
  fa <- tryCatch(bayesqm::compute_factor_array(fit), error = function(e) NULL)
  if (is.null(fa)) return(NULL)
  gcol <- fa[[paste0("f", factor_num, "_grid")]]
  if (is.null(gcol)) return(NULL)

  zs <- tryCatch(bayesqm::compute_zscores(fit), error = function(e) NULL)
  zm <- if (!is.null(zs)) zs[[paste0("f", factor_num, "_zsc")]] else rep(NA_real_, nrow(fa))

  distribution <- fit$brief$distribution
  if (is.null(distribution) && !is.null(qdata)) distribution <- qdata@distribution
  if (is.null(distribution)) return(NULL)

  stmt_idx <- suppressWarnings(as.integer(sub("^S", "", fa$statement)))
  if (anyNA(stmt_idx)) stmt_idx <- seq_len(nrow(fa))

  txt <- if (!is.null(qdata) && length(qdata@statements) >= max(stmt_idx)) {
    as.character(qdata@statements)[stmt_idx]
  } else {
    as.character(fa$statement)
  }
  long <- nchar(txt) > 52
  txt[long] <- paste0(substr(txt[long], 1, 49), "...")

  cards <- data.frame(
    col = gcol,
    stmt_idx = stmt_idx,
    stmt_text = txt,
    foot = ifelse(is.na(zm), "", sprintf("z = %.2f", zm)),
    tag = NA_character_,
    ord = ifelse(is.na(zm), 0, zm),
    stringsAsFactors = FALSE
  )

  qsort_card_grid(
    cards, distribution,
    title = paste0("Factor ", factor_num, " · Posterior Q-Sort"),
    subtitle = sprintf("%d statements · columns from the posterior reported array",
                       nrow(cards)),
    caption = "z = posterior mean score"
  )
}

# Bootstrap stability figures (dashboard only) ----

# Priorities figures (dashboard only) ----

#' Panel-wide priorities ranking as a bar chart
#'
#' Every statement's mean deck position across all sorts, ranked.
#'
#' @param qdata QsortData object
#' @return ggplot object
#' @noRd
create_plot_priorities_ranking <- function(qdata) {

  pr <- compute_priorities(qdata)
  tbl <- pr$table
  if (is.null(tbl) || nrow(tbl) == 0) return(NULL)

  tbl$stmt <- factor(tbl$stmt, levels = rev(tbl$stmt))
  tbl$side <- ifelse(tbl$mean >= 0, "Prioritized", "Deprioritized")

  ggplot2::ggplot(tbl, ggplot2::aes(x = mean, y = stmt, fill = side)) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::geom_vline(xintercept = 0, color = "#475569", linewidth = 0.4) +
    ggplot2::scale_fill_manual(
      values = c(Prioritized = "#236192", Deprioritized = "#DF6A2E"),
      guide = "none"
    ) +
    ggplot2::labs(
      title = "Panel Priorities",
      subtitle = sprintf("mean deck position across %d sorts", pr$meta$n),
      x = "Mean deck position", y = NULL
    ) +
    theme_canhrqsort() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 7.5),
      panel.grid.major.y = ggplot2::element_blank()
    )
}

#' Panel composition: one chart per demographic
#'
#' The Priorities page's participants figure as a reusable builder:
#' binned and ordinal attributes draw as pies, plain categoricals as
#' bars, raw numerics as histograms, arranged two per row.
#'
#' @param qdata QsortData object with participant attributes
#' @return a grob (draw with grid::grid.draw), or NULL without demographics
#' @noRd
create_plot_panel_composition <- function(qdata) {
    qd <- qdata
    if (is.null(qd) || !has_participant_attributes(qd)) return(NULL)
    groups <- attribute_groups(qd)
    if (length(groups) == 0) return(NULL)
    fcolors <- get_theme_colors()$factor_colors

    # Binned numeric and ordinal attributes (age groups) draw as pies, the
    # prototype's treatment; plain categorical attributes stay bars
    attrs <- participant_attributes(qd)
    spec_all <- qd@metadata$attribute_spec
    if (!is.list(spec_all)) spec_all <- list()
    detected <- detect_attribute_types(attrs)
    attr_spec <- function(a) {
      s <- spec_all[[a]]
      if (is.null(s)) s <- detected[[a]]
      s
    }

    plots <- lapply(names(groups), function(a) {
      v <- groups[[a]]
      s_a <- attr_spec(a)
      as_pie <- is.factor(v) && !is.null(s_a) &&
        (identical(s_a$type, "ordinal") ||
           (identical(s_a$type, "numeric") && !is.null(s_a$breaks)))

      if (as_pie) {
        cnt <- as.data.frame(table(v, useNA = "no"))
        names(cnt) <- c("level", "n")
        cnt <- cnt[cnt$n > 0, , drop = FALSE]
        cnt$pct <- 100 * cnt$n / sum(cnt$n)
        lv <- levels(v)[levels(v) %in% as.character(cnt$level)]
        fills_lv <- stats::setNames(
          fcolors[((seq_along(lv) - 1) %% length(fcolors)) + 1], lv)
        cnt$level <- factor(as.character(cnt$level), levels = lv)
        cnt <- cnt[order(cnt$level, decreasing = TRUE), , drop = FALSE]
        cnt$pos <- cumsum(cnt$n) - 0.5 * cnt$n

        ggplot2::ggplot(cnt, ggplot2::aes(x = "", y = n, fill = level)) +
          ggplot2::geom_col(width = 1, color = "white") +
          ggplot2::coord_polar("y", start = 0) +
          ggplot2::geom_text(
            ggplot2::aes(y = pos,
                         label = sprintf("%.0f%%\n(%s)", pct, level)),
            color = "white", size = 5.2, fontface = "bold", lineheight = 0.85
          ) +
          ggplot2::scale_fill_manual(values = fills_lv, guide = "none") +
          ggplot2::labs(title = tools::toTitleCase(pr_attr_label(a))) +
          ggplot2::theme_void(base_size = 13) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5, face = "bold",
                                               size = 15, color = "#00274C"),
            plot.margin = ggplot2::margin(6, 6, 6, 6),
            legend.position = "none"
          )
      } else if (is.factor(v)) {
        cnt <- as.data.frame(table(v, useNA = "no"))
        names(cnt) <- c("level", "n")
        cnt$pct <- 100 * cnt$n / sum(cnt$n)
        fills <- fcolors[((seq_len(nrow(cnt)) - 1) %% length(fcolors)) + 1]
        ggplot2::ggplot(cnt, ggplot2::aes(x = level, y = n, fill = level)) +
          ggplot2::geom_col(width = 0.6, color = "white") +
          ggplot2::geom_text(
            ggplot2::aes(label = sprintf("%d\n(%.0f%%)", n, pct)),
            vjust = -0.25, size = 4.4, fontface = "bold", lineheight = 0.9,
            color = "#1e293b"
          ) +
          ggplot2::scale_fill_manual(values = unname(fills), guide = "none") +
          ggplot2::scale_y_continuous(
            expand = ggplot2::expansion(mult = c(0, 0.3))
          ) +
          ggplot2::labs(title = tools::toTitleCase(pr_attr_label(a)), x = NULL, y = "Count") +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5, face = "bold",
                                               size = 15, color = "#00274C"),
            axis.text = ggplot2::element_text(face = "bold", color = "#334155"),
            axis.title.y = ggplot2::element_text(color = "#475569"),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank()
          )
      } else {
        df <- data.frame(v = as.numeric(v))
        df <- df[!is.na(df$v), , drop = FALSE]
        ggplot2::ggplot(df, ggplot2::aes(x = v)) +
          ggplot2::geom_histogram(bins = min(15, max(5, nrow(df) %/% 4)),
                                  fill = "#236192", color = "white") +
          ggplot2::labs(
            title = tools::toTitleCase(pr_attr_label(a)),
            subtitle = "numeric: group it below to compare and chart groups",
            x = NULL, y = "Count"
          ) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5, face = "bold",
                                               size = 15, color = "#00274C"),
            plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 11,
                                                  color = "#64748b"),
            axis.text = ggplot2::element_text(face = "bold", color = "#334155"),
            axis.title.y = ggplot2::element_text(color = "#475569"),
            panel.grid.minor = ggplot2::element_blank()
          )
      }
    })

    gridExtra::arrangeGrob(grobs = plots, ncol = min(2, length(plots)))
}

#' One statement across factors, plainly
#'
#' Replaces the density-overlay statement posterior with something a reader
#' takes in at a glance: one row per factor with its posterior mean, its
#' 95 percent credible interval, and the grid column the reported array
#' places the statement in.
#'
#' @param fit bayesqm fit object
#' @param qdata QsortData object (statement texts)
#' @param statement Statement index
#' @return ggplot2 object
#' @noRd
create_plot_bq_statement <- function(fit, qdata = NULL, statement = 1) {
  zs <- tryCatch(bayesqm::compute_zscores(fit), error = function(e) NULL)
  if (is.null(zs) || statement < 1 || statement > nrow(zs)) return(NULL)
  fa <- tryCatch(bayesqm::compute_factor_array(fit), error = function(e) NULL)

  K <- fit$brief$K
  distribution <- fit$brief$distribution
  labs <- if (!is.null(distribution)) bq_grid_labels(length(distribution)) else NULL
  fcolors <- get_theme_colors()$factor_colors
  fcol <- fcolors[((seq_len(K) - 1) %% length(fcolors)) + 1]

  rows <- lapply(seq_len(K), function(k) {
    gcol <- if (!is.null(fa)) fa[[paste0("f", k, "_grid")]][statement] else NA
    glab <- if (!is.null(labs) && !is.na(gcol)) labs[gcol] else NA
    data.frame(
      factor = paste("Factor", k),
      mean = zs[[paste0("f", k, "_zsc")]][statement],
      lower = zs[[paste0("f", k, "_lower")]][statement],
      upper = zs[[paste0("f", k, "_upper")]][statement],
      grid = glab,
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  if (all(is.na(df$mean))) return(NULL)
  df$factor <- factor(df$factor, levels = rev(paste("Factor", seq_len(K))))
  df$lab <- ifelse(
    is.na(df$grid),
    sprintf("z = %.2f", df$mean),
    sprintf("z = %.2f \u00b7 grid %s", df$mean,
            ifelse(df$grid > 0, paste0("+", df$grid), df$grid))
  )

  stmt_txt <- if (!is.null(qdata) &&
                  length(qdata@statements) >= statement) {
    as.character(qdata@statements)[statement]
  } else {
    ""
  }
  pad <- 0.12 * diff(range(c(df$lower, df$upper), na.rm = TRUE))

  ggplot2::ggplot(df, ggplot2::aes(y = factor)) +
    ggplot2::geom_vline(xintercept = 0, color = "#94a3b8",
                        linetype = "dashed", linewidth = 0.5) +
    ggplot2::geom_linerange(
      ggplot2::aes(xmin = lower, xmax = upper, color = factor),
      linewidth = 2.6, alpha = 0.9
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = mean, color = factor),
      size = 5, shape = 21, fill = "white", stroke = 2.2
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = upper + pad * 0.25, label = lab),
      color = "black", size = 3.6, fontface = "bold", hjust = 0
    ) +
    ggplot2::scale_color_manual(
      values = stats::setNames(fcol, paste("Factor", seq_len(K))),
      guide = "none"
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(add = c(pad * 0.6, pad * 2.6))
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 12.5, face = "bold",
                                          color = "#00274C"),
      axis.title.x = ggplot2::element_text(color = "#475569"),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.title = ggplot2::element_text(hjust = 0, face = "bold", size = 17,
                                         color = "#00274C",
                                         margin = ggplot2::margin(b = 2)),
      plot.subtitle = ggplot2::element_text(hjust = 0, size = 12,
                                            color = "#1e293b",
                                            margin = ggplot2::margin(b = 14)),
      plot.caption = ggplot2::element_text(hjust = 0, size = 10.5,
                                           color = "#64748b",
                                           margin = ggplot2::margin(t = 12))
    ) +
    ggplot2::labs(
      title = sprintf("Statement S%d", statement),
      subtitle = if (nzchar(stmt_txt)) {
        paste(strwrap(stmt_txt, width = 95), collapse = "\n")
      },
      caption = "posterior mean and 95% credible interval per factor \u00b7 grid = the reported array column",
      x = "score (z units)", y = NULL
    )
}
