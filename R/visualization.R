#' @title Visualization Functions for Q-Sort Analysis
#' @description ggplot2-based visualization functions for Q-sort results
#' @name visualization
#' @import ggplot2
NULL

#' Plot Q-Sort Analysis Results
#'
#' @description
#' Main plotting function for Q-sort analysis. Dispatches to specific
#' plot types based on the 'type' argument.
#'
#' @param x A QsortResults or QsortBootstrap object
#' @param type Plot type: "loadings", "scores", "correlation", "scree",
#'   "factor_arrays", "consensus", "distinguishing"
#' @param ... Additional arguments passed to specific plot functions
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' results <- qsort_analyze(qdata)
#' plot(results, type = "loadings")
#' plot(results, type = "scores", factor = 1)
#' plot(results, type = "correlation")
#' }
plot_qsort <- function(x, type = "loadings", ...) {
  type <- match.arg(type, c("loadings", "scores", "correlation", "scree",
                            "factor_arrays", "consensus", "distinguishing"))

  switch(type,
    loadings = plot_loadings(x, ...),
    scores = plot_scores(x, ...),
    correlation = plot_correlation(x, ...),
    scree = plot_scree(x, ...),
    factor_arrays = plot_factor_arrays(x, ...),
    consensus = plot_consensus(x, ...),
    distinguishing = plot_distinguishing(x, ...)
  )
}

#' Plot Factor Loadings
#'
#' @description
#' Create a visualization of factor loadings for Q-sort analysis.
#'
#' @param results A QsortResults or QsortBootstrap object
#' @param factors Which factors to plot (default: all)
#' @param show_flags Logical; highlight flagged Q-sorts (default TRUE)
#' @param show_ci Logical; show confidence intervals if available (default TRUE)
#' @param sort_by Factor number to sort by, or "none" (default: 1)
#' @param coord_flip Logical; flip coordinates for horizontal bars (default TRUE)
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_loadings(results)
#' plot_loadings(results, factors = c(1, 2), show_flags = TRUE)
#' }
plot_loadings <- function(results,
                          factors = NULL,
                          show_flags = TRUE,
                          show_ci = TRUE,
                          sort_by = 1,
                          coord_flip = TRUE) {

  # Extract data based on object type
  if (inherits(results, "QsortBootstrap")) {
    loadings_df <- results@loading_ci
    has_ci <- TRUE
    flags <- results@original@flagging
  } else if (inherits(results, "QsortResults")) {
    loadings <- results@rotation$loadings
    loadings <- if (inherits(loadings, "loadings")) unclass(loadings) else as.matrix(loadings)
    loadings_df <- tibble::as_tibble(loadings, rownames = "participant")
    loadings_df <- tidyr::pivot_longer(
      loadings_df,
      cols = -participant,
      names_to = "factor",
      values_to = "loading"
    )
    loadings_df$original <- loadings_df$loading
    has_ci <- FALSE
    flags <- results@flagging
  } else {
    rlang::abort("results must be QsortResults or QsortBootstrap")
  }

  # Filter factors if specified
  if (!is.null(factors)) {
    factor_names <- paste0("F", factors)
    loadings_df <- loadings_df[loadings_df$factor %in% factor_names, ]
  }

  if (show_flags && !is.null(flags)) {
    flag_df <- tibble::as_tibble(flags, rownames = "participant")
    flag_df <- tidyr::pivot_longer(
      flag_df,
      cols = -participant,
      names_to = "factor",
      values_to = "flagged"
    )
    loadings_df <- merge(loadings_df, flag_df, by = c("participant", "factor"), all.x = TRUE)
    loadings_df$flagged[is.na(loadings_df$flagged)] <- FALSE
  } else {
    loadings_df$flagged <- FALSE
  }

  # Sort participants
  if (sort_by != "none") {
    sort_factor <- paste0("F", sort_by)
    sort_order <- loadings_df[loadings_df$factor == sort_factor, ]

    if (has_ci && "boot_mean" %in% names(sort_order)) {
      sort_order <- sort_order[order(sort_order$boot_mean), ]
    } else if ("loading" %in% names(sort_order)) {
      sort_order <- sort_order[order(sort_order$loading), ]
    } else if ("original" %in% names(sort_order)) {
      sort_order <- sort_order[order(sort_order$original), ]
    }

    loadings_df$participant <- factor(
      loadings_df$participant,
      levels = sort_order$participant
    )
  }

  # Determine value column
  value_col <- if (has_ci && "boot_mean" %in% names(loadings_df)) "boot_mean" else
               if ("loading" %in% names(loadings_df)) "loading" else "original"

  p <- ggplot2::ggplot(loadings_df, ggplot2::aes(x = participant, y = .data[[value_col]])) +
    ggplot2::theme_minimal(base_size = 12)

  if (has_ci && show_ci && all(c("ci_lower", "ci_upper") %in% names(loadings_df))) {
    p <- p + ggplot2::geom_linerange(
      ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
      color = "gray60",
      linewidth = 0.5
    )
  }

  if (show_flags) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(fill = flagged, size = flagged),
      shape = 21,
      color = "black"
    ) +
    ggplot2::scale_fill_manual(
      values = c("FALSE" = "gray70", "TRUE" = "#E41A1C"),
      labels = c("Not flagged", "Flagged"),
      name = "Status"
    ) +
    ggplot2::scale_size_manual(
      values = c("FALSE" = 2, "TRUE" = 3),
      guide = "none"
    )
  } else {
    p <- p + ggplot2::geom_point(size = 2, color = "#377EB8")
  }

  # Reference line at zero
  p <- p + ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40")

  # Facet by factor
  n_factors <- length(unique(loadings_df$factor))
  if (n_factors > 1) {
    p <- p + ggplot2::facet_wrap(~ factor, scales = "free_y")
  }

  # Labels
  p <- p +
    ggplot2::labs(
      title = "Factor Loadings",
      subtitle = if (has_ci) "With 95% bootstrap confidence intervals" else NULL,
      x = "Participant",
      y = "Loading"
    )

  # Coordinate flip for better readability
  if (coord_flip) {
    p <- p + ggplot2::coord_flip()
  } else {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }

  return(p)
}

#' Plot Factor Scores
#'
#' @description
#' Create a visualization of factor scores (z-scores) for statements.
#'
#' @param results A QsortResults object
#' @param factor Factor number to plot (default: 1)
#' @param n_top Number of top/bottom statements to highlight (default: 5)
#' @param show_labels Logical; show statement labels (default TRUE)
#' @param distinguish Logical; highlight distinguishing statements (default TRUE)
#'
#' @return A ggplot object
#' @export
plot_scores <- function(results,
                        factor = 1,
                        n_top = 5,
                        show_labels = TRUE,
                        distinguish = TRUE) {

  if (!inherits(results, "QsortResults")) {
    rlang::abort("results must be a QsortResults object")
  }

  factor_name <- paste0("F", factor)
  zscore_col <- paste0(factor_name, "_zscore")

  if (!zscore_col %in% names(results@factor_scores)) {
    rlang::abort(glue::glue("Factor {factor} not found in results"))
  }

  scores_df <- results@factor_scores
  scores_df$zscore <- scores_df[[zscore_col]]

  # Sort by z-score
  scores_df <- scores_df[order(scores_df$zscore, decreasing = TRUE), ]
  scores_df$rank <- seq_len(nrow(scores_df))
  scores_df$statement_factor <- factor(scores_df$statement, levels = rev(scores_df$statement))

  # Identify distinguishing statements
  if (distinguish && length(results@distinguishing) >= factor) {
    dist_stmts <- results@distinguishing[[factor]]
    scores_df$distinguishing <- scores_df$statement %in% dist_stmts
  } else {
    scores_df$distinguishing <- FALSE
  }

  # Identify top/bottom
  n_stmt <- nrow(scores_df)
  scores_df$position <- ifelse(
    scores_df$rank <= n_top, "Top",
    ifelse(scores_df$rank > n_stmt - n_top, "Bottom", "Middle")
  )

  p <- ggplot2::ggplot(scores_df, ggplot2::aes(x = statement_factor, y = zscore)) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40")

  # Color by position and distinguish
  if (distinguish) {
    p <- p + ggplot2::geom_col(
      ggplot2::aes(fill = interaction(position, distinguishing)),
      width = 0.7
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Top.FALSE" = "#4DAF4A",
        "Top.TRUE" = "#006400",
        "Middle.FALSE" = "gray70",
        "Middle.TRUE" = "#984EA3",
        "Bottom.FALSE" = "#E41A1C",
        "Bottom.TRUE" = "#8B0000"
      ),
      labels = c("Top", "Top (Distinguishing)", "Middle",
                 "Middle (Distinguishing)", "Bottom", "Bottom (Distinguishing)"),
      name = "Position"
    )
  } else {
    p <- p + ggplot2::geom_col(
      ggplot2::aes(fill = position),
      width = 0.7
    ) +
    ggplot2::scale_fill_manual(
      values = c("Top" = "#4DAF4A", "Middle" = "gray70", "Bottom" = "#E41A1C"),
      name = "Position"
    )
  }

  if (show_labels) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = round(zscore, 2)),
      hjust = ifelse(scores_df$zscore >= 0, -0.1, 1.1),
      size = 3
    )
  }

  # Labels
  p <- p +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = glue::glue("Factor {factor} Statement Scores"),
      subtitle = "Z-scores (standardized factor scores)",
      x = NULL,
      y = "Z-score"
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.major.y = ggplot2::element_blank()
    )

  return(p)
}

#' Plot Correlation Matrix
#'
#' @description
#' Create a heatmap visualization of the Q-sort correlation matrix.
#'
#' @param results A QsortResults or QsortData object
#' @param show_values Logical; show correlation values (default FALSE)
#' @param cluster Logical; cluster similar Q-sorts (default TRUE)
#'
#' @return A ggplot object
#' @export
plot_correlation <- function(results,
                             show_values = FALSE,
                             cluster = TRUE) {

  if (inherits(results, "QsortResults")) {
    cor_mat <- results@correlation
    flags <- results@flagging
  } else if (inherits(results, "QsortData")) {
    cor_mat <- qsort_correlation(results)
    flags <- NULL
  } else {
    rlang::abort("results must be QsortResults or QsortData")
  }

  # Optionally cluster
  if (cluster) {
    hc <- hclust(as.dist(1 - cor_mat))
    order <- hc$order
    cor_mat <- cor_mat[order, order]
  }

  cor_df <- tibble::as_tibble(cor_mat, rownames = "participant_i")
  cor_df <- tidyr::pivot_longer(
    cor_df,
    cols = -participant_i,
    names_to = "participant_j",
    values_to = "correlation"
  )

  # Maintain order
  cor_df$participant_i <- factor(cor_df$participant_i, levels = rownames(cor_mat))
  cor_df$participant_j <- factor(cor_df$participant_j, levels = colnames(cor_mat))

  p <- ggplot2::ggplot(cor_df, ggplot2::aes(x = participant_j, y = participant_i, fill = correlation)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.1) +
    ggplot2::scale_fill_gradient2(
      low = "#2166AC",
      mid = "white",
      high = "#B2182B",
      midpoint = 0,
      limits = c(-1, 1),
      name = "r"
    ) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = ggplot2::element_text(size = 8),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = "Q-Sort Correlation Matrix",
      subtitle = if (cluster) "Clustered by similarity" else NULL,
      x = NULL,
      y = NULL
    )

  if (show_values) {
    # White text on strong correlations
    cor_df$text_color <- ifelse(abs(cor_df$correlation) > 0.5, "white", "black")
    p <- p + ggplot2::geom_text(
      data = cor_df,
      ggplot2::aes(label = sprintf("%.2f", correlation), color = text_color),
      size = 2
    ) +
    ggplot2::scale_color_identity()
  }

  return(p)
}

#' Plot Factor Arrays (Q-Sort Grids)
#'
#' @description
#' Visualize factor scores as Q-sort grid arrays showing the idealized
#' sort for each factor.
#'
#' @param results A QsortResults object
#' @param factor Factor number to plot (default: 1)
#' @param show_text Logical; show statement text (default TRUE, truncated)
#' @param text_size Text size for statements (default: 2.5)
#' @param distribution Optional distribution vector (uses data distribution if NULL)
#'
#' @return A ggplot object
#' @export
plot_factor_arrays <- function(results,
                               factor = 1,
                               show_text = TRUE,
                               text_size = 2.5,
                               distribution = NULL) {

  if (!inherits(results, "QsortResults")) {
    rlang::abort("results must be a QsortResults object")
  }

  factor_name <- paste0("F", factor)
  score_col <- paste0(factor_name, "_score")
  zscore_col <- paste0(factor_name, "_zscore")

  scores_df <- results@factor_scores

  if (is.null(distribution)) {
    distribution <- results@data@distribution
  }

  n_cols <- length(distribution)

  # Handle edge case of empty distribution
  if (n_cols == 0) {
    cli::cli_alert_warning("No distribution available for factor array plot")
    return(ggplot2::ggplot() + ggplot2::theme_void() +
           ggplot2::labs(title = "Factor array not available - no distribution data"))
  }

  # Calculate the range of column values based on distribution length

  # Standard Q-sort distributions are symmetric around 0
  # For 9 columns: range -4 to +4 (indices 1-9 map to values -4 to +4)
  # For 11 columns: range -5 to +5
  # For 7 columns: range -3 to +3
  # For even distributions (less common): center as best as possible
  if (n_cols %% 2 == 1) {
    # Odd number of columns - symmetric around 0
    half <- (n_cols - 1) / 2
    min_val <- -half
    max_val <- half
  } else {
    # Even number of columns - no true center, offset slightly
    half <- n_cols / 2
    min_val <- -half + 1
    max_val <- half
  }

  # Sort statements by z-score and assign to grid positions.
  # Lowest z-scores should appear in the most negative column; highest in the most positive.
  scores_df <- scores_df[order(scores_df[[zscore_col]], decreasing = FALSE), ]

  # Assign grid positions based on distribution
  grid_data <- data.frame()
  stmt_idx <- 1

  for (col in seq_len(n_cols)) {
    col_val <- min_val + col - 1
    n_in_col <- distribution[col]
    max_row <- max(distribution)

    # Center statements vertically in column
    start_row <- (max_row - n_in_col) / 2 + 1

    for (row in seq_len(n_in_col)) {
      if (stmt_idx <= nrow(scores_df)) {
        grid_data <- rbind(grid_data, data.frame(
          col_val = col_val,
          row_val = start_row + row - 1,
          statement = scores_df$statement[stmt_idx],
          zscore = scores_df[[zscore_col]][stmt_idx],
          statement_num = scores_df$statement_num[stmt_idx]
        ))
        stmt_idx <- stmt_idx + 1
      }
    }
  }

  # Truncate statement text for display
  if (show_text) {
    statements <- results@data@statements
    grid_data$text <- sapply(grid_data$statement_num, function(i) {
      if (i <= length(statements)) {
        txt <- statements[i]
        if (nchar(txt) > 40) paste0(substr(txt, 1, 37), "...") else txt
      } else {
        grid_data$statement[grid_data$statement_num == i]
      }
    })
  }

  p <- ggplot2::ggplot(grid_data, ggplot2::aes(x = col_val, y = row_val)) +
    ggplot2::geom_tile(ggplot2::aes(fill = zscore), color = "black", linewidth = 0.5) +
    ggplot2::scale_fill_gradient2(
      low = "#D73027",
      mid = "#FFFFBF",
      high = "#1A9850",
      midpoint = 0,
      name = "Z-score"
    ) +
    ggplot2::scale_x_continuous(
      breaks = min_val:max_val,
      labels = min_val:max_val
    ) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = glue::glue("Factor {factor} Array"),
      subtitle = "Idealized Q-sort",
      x = "Column Value",
      y = NULL
    )

  if (show_text) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = paste0(statement_num, ": ", stringr::str_wrap(text, 15))),
      size = text_size,
      lineheight = 0.8
    )
  } else {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = statement_num),
      size = text_size + 1,
      fontface = "bold"
    )
  }

  return(p)
}

#' Plot Distinguishing Statements
#'
#' @description
#' Create a visualization highlighting distinguishing statements for each factor.
#'
#' @param results A QsortResults object
#' @param factors Which factors to include (default: all)
#'
#' @return A ggplot object
#' @export
plot_distinguishing <- function(results, factors = NULL) {

  if (!inherits(results, "QsortResults")) {
    rlang::abort("results must be a QsortResults object")
  }

  if (is.null(factors)) {
    factors <- seq_len(results@n_factors)
  }

  # Build data frame of distinguishing statements
  dist_data <- data.frame()

  for (f in factors) {
    factor_name <- paste0("F", f)
    zscore_col <- paste0(factor_name, "_zscore")

    dist_stmts <- results@distinguishing[[f]]

    if (length(dist_stmts) > 0) {
      for (stmt in dist_stmts) {
        row_idx <- which(results@factor_scores$statement == stmt)
        if (length(row_idx) > 0) {
          dist_data <- rbind(dist_data, data.frame(
            factor = factor_name,
            statement = stmt,
            zscore = results@factor_scores[[zscore_col]][row_idx],
            statement_num = results@factor_scores$statement_num[row_idx]
          ))
        }
      }
    }
  }

  if (nrow(dist_data) == 0) {
    cli::cli_alert_warning("No distinguishing statements found")
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No distinguishing statements"))
  }

  # Sort within factors
  dist_data <- dist_data[order(dist_data$factor, -dist_data$zscore), ]
  dist_data$label <- paste0(dist_data$statement_num, ": ", dist_data$statement)
  dist_data$label <- factor(dist_data$label, levels = unique(dist_data$label))

  p <- ggplot2::ggplot(dist_data, ggplot2::aes(x = label, y = zscore, fill = factor)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~ factor, scales = "free_y", ncol = 1) +
    ggplot2::scale_fill_brewer(palette = "Set1", guide = "none") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::labs(
      title = "Distinguishing Statements by Factor",
      subtitle = "Statements that significantly differentiate each factor",
      x = NULL,
      y = "Z-score"
    )

  return(p)
}

#' Plot Consensus Statements
#'
#' @description
#' Create a visualization of consensus statements shared across all factors.
#'
#' @param results A QsortResults object
#'
#' @return A ggplot object
#' @export
plot_consensus <- function(results) {

  if (!inherits(results, "QsortResults")) {
    rlang::abort("results must be a QsortResults object")
  }

  consensus_stmts <- results@consensus

  if (length(consensus_stmts) == 0) {
    cli::cli_alert_warning("No consensus statements found")
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No consensus statements"))
  }

  # Get z-scores for consensus statements across all factors
  zscore_cols <- grep("_zscore$", names(results@factor_scores), value = TRUE)

  consensus_data <- results@factor_scores[results@factor_scores$statement %in% consensus_stmts, ]
  consensus_long <- tidyr::pivot_longer(
    consensus_data,
    cols = tidyr::all_of(zscore_cols),
    names_to = "factor",
    values_to = "zscore"
  )
  consensus_long$factor <- gsub("_zscore$", "", consensus_long$factor)

  avg_scores <- tapply(consensus_long$zscore, consensus_long$statement, mean)
  consensus_long$statement <- factor(
    consensus_long$statement,
    levels = names(sort(avg_scores))
  )

  p <- ggplot2::ggplot(consensus_long, ggplot2::aes(x = statement, y = zscore, fill = factor)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_brewer(palette = "Set1", name = "Factor") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = "Consensus Statements",
      subtitle = "Statements with similar scores across all factors",
      x = NULL,
      y = "Z-score"
    )

  return(p)
}

#' Scree Plot
#'
#' @description
#' Create a scree plot showing eigenvalues for factor selection.
#'
#' @param results A QsortResults object
#' @param max_factors Maximum factors to show (default: 10 or n_factors)
#' @param show_parallel Logical; show parallel analysis threshold (default TRUE)
#'
#' @return A ggplot object
#' @keywords internal
plot_scree <- function(results, max_factors = NULL, show_parallel = TRUE) {

  if (!inherits(results, "QsortResults")) {
    rlang::abort("results must be a QsortResults object")
  }

  eigenvalues <- results@extraction$eigenvalues
  n_available <- length(eigenvalues)

  if (is.null(max_factors)) {
    max_factors <- min(10, n_available)
  }
  # Ensure we don't exceed available eigenvalues
  max_factors <- min(max_factors, n_available)

  scree_data <- data.frame(
    factor = seq_len(max_factors),
    eigenvalue = eigenvalues[seq_len(max_factors)]
  )

  p <- ggplot2::ggplot(scree_data, ggplot2::aes(x = factor, y = eigenvalue)) +
    ggplot2::geom_line(linewidth = 1, color = "#377EB8") +
    ggplot2::geom_point(size = 3, color = "#377EB8") +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    ggplot2::scale_x_continuous(breaks = seq_len(max_factors)) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(
      title = "Scree Plot",
      subtitle = "Red line: Kaiser criterion (eigenvalue = 1)",
      x = "Factor Number",
      y = "Eigenvalue"
    )

  return(p)
}

# Crib sheet and factor comparison visualizations ----

#' Plot Factor Comparison Table
#'
#' @description
#' Comparison table of z-scores across all factors, showing how statements
#' rank differently, with distinguishing statements highlighted.
#'
#' @param results A QsortResults object
#' @param sort_by Factor number to sort by, or "variance" for most variable (default: 1)
#' @param n_show Number of statements to show (default: all)
#' @param highlight_threshold Z-score difference threshold for highlighting (default: 1.0)
#' @param text_width Maximum character width for statement text (default: 40)
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' results <- qsort_analyze(qdata, nfactors = 3)
#' plot_factor_comparison(results)
#' plot_factor_comparison(results, sort_by = "variance", n_show = 20)
#' }
plot_factor_comparison <- function(results,
                                    sort_by = 1,
                                    n_show = NULL,
                                    highlight_threshold = 1.0,
                                    text_width = 40) {

  if (!inherits(results, "QsortResults")) {
    rlang::abort("results must be a QsortResults object")
  }

  factor_scores <- results@factor_scores
  zscore_cols <- grep("_zscore$", names(factor_scores), value = TRUE)
  n_factors <- length(zscore_cols)

  comp_data <- data.frame(
    stmt_num = factor_scores$statement_num,
    statement = factor_scores$statement,
    stringsAsFactors = FALSE
  )

  zscores_mat <- as.matrix(factor_scores[, zscore_cols])
  colnames(zscores_mat) <- paste0("F", seq_len(n_factors))
  comp_data <- cbind(comp_data, zscores_mat)

  comp_data$variance <- apply(zscores_mat, 1, var, na.rm = TRUE)
  comp_data$range <- apply(zscores_mat, 1, function(x) max(x) - min(x))

  # Sort
  if (sort_by == "variance") {
    comp_data <- comp_data[order(comp_data$variance, decreasing = TRUE), ]
  } else {
    sort_col <- paste0("F", sort_by)
    if (sort_col %in% names(comp_data)) {
      comp_data <- comp_data[order(comp_data[[sort_col]], decreasing = TRUE), ]
    }
  }

  # Limit number shown
  if (!is.null(n_show) && n_show < nrow(comp_data)) {
    comp_data <- comp_data[seq_len(n_show), ]
  }

  # Truncate statement text
  comp_data$stmt_label <- sapply(seq_len(nrow(comp_data)), function(i) {
    txt <- comp_data$statement[i]
    num <- comp_data$stmt_num[i]
    if (nchar(txt) > text_width) {
      txt <- paste0(substr(txt, 1, text_width - 3), "...")
    }
    paste0(num, ": ", txt)
  })

  # Pivot to long format
  factor_cols <- paste0("F", seq_len(n_factors))
  comp_long <- tidyr::pivot_longer(
    comp_data,
    cols = tidyr::all_of(factor_cols),
    names_to = "factor",
    values_to = "zscore"
  )

  # Calculate if statement is distinguishing for each factor
  comp_long$is_distinguishing <- FALSE
  for (f in seq_len(n_factors)) {
    dist_stmts <- if (f <= length(results@distinguishing)) {
      results@distinguishing[[f]]
    } else {
      character(0)
    }
    mask <- comp_long$factor == paste0("F", f) & comp_long$statement %in% dist_stmts
    comp_long$is_distinguishing[mask] <- TRUE
  }

  consensus_stmts <- results@consensus
  comp_long$is_consensus <- comp_long$statement %in% consensus_stmts

  # Order statements
  comp_long$stmt_label <- factor(
    comp_long$stmt_label,
    levels = rev(unique(comp_data$stmt_label))
  )

  p <- ggplot2::ggplot(comp_long, ggplot2::aes(x = factor, y = stmt_label, fill = zscore))

  # Tiles
  p <- p + ggplot2::geom_tile(color = "white", linewidth = 0.5)

  # The deck's colors as a diverging scale: navy disagree, orange agree
  p <- p + ggplot2::scale_fill_gradient2(
    low = "#00274C",
    mid = "#f6f8fa",
    high = "#DF6A2E",
    midpoint = 0,
    limits = c(-3, 3),
    oob = scales::squish,
    name = "z-score"
  )

  # Cell values, white where the fill runs dark
  p <- p + ggplot2::geom_text(
    ggplot2::aes(
      label = sprintf("%+.1f", zscore),
      fontface = ifelse(is_distinguishing, "bold", "plain"),
      color = ifelse(abs(zscore) > 1.4, "white", "#1e293b")
    ),
    size = 3.4
  )
  p <- p + ggplot2::scale_color_identity()

  # Distinguishing markers
  if (any(comp_long$is_distinguishing)) {
    p <- p + ggplot2::geom_point(
      data = comp_long[comp_long$is_distinguishing, ],
      shape = 0,
      size = 9,
      color = "#1e293b",
      stroke = 1.1
    )
  }

  # Theme: the tiles stretch to the full canvas, no forced aspect
  p <- p +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 9, color = "#334155"),
      axis.text.x.top = ggplot2::element_text(size = 12.5, face = "bold",
                                              color = "#00274C"),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right",
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.title = ggplot2::element_text(hjust = 0, face = "bold", size = 17,
                                         color = "#00274C",
                                         margin = ggplot2::margin(b = 2)),
      plot.subtitle = ggplot2::element_text(hjust = 0, size = 11.5,
                                            color = "#64748b",
                                            margin = ggplot2::margin(b = 10)),
      plot.caption = ggplot2::element_text(hjust = 0, size = 10.5,
                                           color = "#64748b",
                                           margin = ggplot2::margin(t = 10))
    ) +
    ggplot2::labs(
      title = "Factor Comparison",
      subtitle = glue::glue(
        "z-scores across {n_factors} factors · sorted by ",
        "{if (sort_by == 'variance') 'variance' else paste('Factor', sort_by)}"
      ),
      x = NULL,
      y = NULL,
      caption = "bold + box = distinguishing (p < 0.05) · orange = agree · navy = disagree"
    )

  return(p)
}

# 10/10 visualization improvements ----

#' Multi-Panel Crib Sheet for All Factors
#'
#' @description
#' Multi-panel figure showing crib sheets for all factors side by side.
#'
#' @param results A QsortResults object
#' @param n_poles Number of pole statements to show at each extreme (default: 3)
#' @param text_width Maximum character width for statement text (default: 35)
#' @param ncol Number of columns in the panel layout (default: auto)
#'
#' @return A ggplot object with faceted crib sheets
#' @export
#'
#' @examples
#' \dontrun{
#' results <- qsort_analyze(qdata, nfactors = 3)
#' plot_crib_sheet_panel(results)
#' }
plot_crib_sheet_panel <- function(results,
                                   n_poles = 3,
                                   text_width = 35,
                                   ncol = NULL) {

  if (!inherits(results, "QsortResults")) {
    rlang::abort("results must be a QsortResults object")
  }

  n_factors <- results@n_factors
  factor_scores <- results@factor_scores
  zscore_cols <- grep("_zscore$", names(factor_scores), value = TRUE)
  n_statements <- nrow(factor_scores)

  # Build combined data for all factors
  all_crib_data <- data.frame()

  for (factor in seq_len(n_factors)) {
    focal_col <- paste0("F", factor, "_zscore")
    other_cols <- setdiff(zscore_cols, focal_col)

    crib_data <- data.frame(
      factor_num = factor,
      factor_label = paste("Factor", factor),
      stmt_num = factor_scores$statement_num,
      statement = factor_scores$statement,
      focal_zscore = factor_scores[[focal_col]],
      stringsAsFactors = FALSE
    )

    if (length(other_cols) > 0) {
      other_zscores <- as.matrix(factor_scores[, other_cols, drop = FALSE])
      crib_data$other_max <- apply(other_zscores, 1, max, na.rm = TRUE)
      crib_data$other_min <- apply(other_zscores, 1, min, na.rm = TRUE)
      crib_data$diff_from_max <- crib_data$focal_zscore - crib_data$other_max
      crib_data$diff_from_min <- crib_data$focal_zscore - crib_data$other_min
    } else {
      crib_data$diff_from_max <- crib_data$focal_zscore
      crib_data$diff_from_min <- crib_data$focal_zscore
    }

    # Identify distinguishing statements
    dist_stmts <- if (factor <= length(results@distinguishing)) {
      results@distinguishing[[factor]]
    } else character(0)
    crib_data$is_distinguishing <- crib_data$statement %in% dist_stmts

    # Rank and categorize
    crib_data <- crib_data[order(crib_data$focal_zscore, decreasing = TRUE), ]
    crib_data$rank <- seq_len(nrow(crib_data))

    crib_data$category <- "Middle"
    crib_data$category[crib_data$rank <= n_poles] <- "Positive Pole"
    crib_data$category[crib_data$rank > n_statements - n_poles] <- "Negative Pole"

    higher_idx <- which(crib_data$diff_from_max > 0.5 &
                         crib_data$category == "Middle" &
                         crib_data$focal_zscore > 0)
    crib_data$category[higher_idx] <- "Higher"

    lower_idx <- which(crib_data$diff_from_min < -0.5 &
                        crib_data$category == "Middle" &
                        crib_data$focal_zscore < 0)
    crib_data$category[lower_idx] <- "Lower"

    # Filter to key statements only
    crib_data <- crib_data[crib_data$category != "Middle" | crib_data$is_distinguishing, ]

    all_crib_data <- rbind(all_crib_data, crib_data)
  }

  # Truncate statement text
  all_crib_data$stmt_label <- sapply(seq_len(nrow(all_crib_data)), function(i) {
    txt <- all_crib_data$statement[i]
    num <- all_crib_data$stmt_num[i]
    if (nchar(txt) > text_width) {
      txt <- paste0(substr(txt, 1, text_width - 3), "...")
    }
    paste0(num, ": ", txt)
  })

  # Order within each factor
  category_order <- c("Positive Pole", "Higher", "Middle", "Lower", "Negative Pole")
  all_crib_data$category <- factor(all_crib_data$category, levels = category_order)

  # Create unique label with factor for proper ordering
  all_crib_data <- all_crib_data[order(all_crib_data$factor_num,
                                        all_crib_data$category,
                                        -all_crib_data$focal_zscore), ]

  # The deck's own colors, matching the app's Q-sort pyramids:
  # most agree in orange and gold, most disagree toward navy
  category_colors <- c(
    "Positive Pole" = "#DF6A2E",
    "Higher" = "#FFB800",
    "Middle" = "#cbd5e1",
    "Lower" = "#87D1E6",
    "Negative Pole" = "#00274C"
  )

  # Determine layout
  if (is.null(ncol)) {
    ncol <- min(n_factors, 3)
  }

  p <- ggplot2::ggplot(all_crib_data, ggplot2::aes(x = focal_zscore, y = reorder(stmt_label, focal_zscore)))

  p <- p +
    ggplot2::geom_col(ggplot2::aes(fill = category), width = 0.72) +
    ggplot2::geom_vline(xintercept = 0, color = "#475569", linewidth = 0.5) +
    ggplot2::scale_fill_manual(values = category_colors, name = NULL) +
    ggplot2::facet_wrap(~ factor_label, scales = "free_y", ncol = ncol) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 8.5, color = "#334155"),
      axis.title = ggplot2::element_text(color = "#475569"),
      strip.text = ggplot2::element_text(face = "bold", size = 12.5,
                                         color = "#00274C"),
      strip.background = ggplot2::element_rect(fill = "#eef2f6", color = NA),
      legend.position = "top",
      legend.justification = "left",
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.title = ggplot2::element_text(hjust = 0, face = "bold", size = 17,
                                         color = "#00274C",
                                         margin = ggplot2::margin(b = 2)),
      plot.subtitle = ggplot2::element_text(hjust = 0, size = 11.5,
                                            color = "#64748b",
                                            margin = ggplot2::margin(b = 8)),
      plot.caption = ggplot2::element_text(hjust = 0, size = 10.5,
                                           color = "#64748b",
                                           margin = ggplot2::margin(t = 10))
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1)) +
    ggplot2::labs(
      title = "Crib Sheets · All Factors",
      subtitle = paste0("pole statements (top and bottom ", n_poles,
                        ") and items ranked apart from the other factors"),
      x = "z-score",
      y = NULL,
      caption = "diamond = distinguishing statement"
    )

  dist_data <- all_crib_data[all_crib_data$is_distinguishing, ]
  if (nrow(dist_data) > 0) {
    p <- p + ggplot2::geom_point(
      data = dist_data,
      ggplot2::aes(x = focal_zscore, y = reorder(stmt_label, focal_zscore)),
      shape = 18, size = 2.4, color = "#1e293b"
    )
  }

  return(p)
}

#' Generic Plot Method for QsortResults
#'
#' @param x A QsortResults object
#' @param y Not used (included for S4 method signature compatibility)
#' @param ... Additional arguments passed to plot_qsort
#' @return A ggplot2 plot object
#' @exportMethod plot
#' @rdname plot-QsortResults-method
setMethod("plot", "QsortResults", function(x, y, ...) {
  plot_qsort(x, ...)
})
