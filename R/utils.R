#' @title Utility Functions
#' @description Helper functions and utilities for canhrQsort package
#' @name utils
NULL

#' Summary of Q-Sort Analysis
#'
#' @description
#' Summarize Q-sort analysis results.
#'
#' @param results A QsortResults object
#' @param verbose Logical; include detailed output (default TRUE)
#'
#' @return A list with summary components (invisibly)
#' @export
qsort_summary <- function(results, verbose = TRUE) {

  if (!inherits(results, "QsortResults")) {
    rlang::abort("results must be a QsortResults object")
  }

  summary_list <- list()

  # Data summary
  summary_list$data <- list(
    n_participants = nrow(results@data@sorts),
    n_statements = ncol(results@data@sorts),
    distribution = results@data@distribution,
    source = results@data@source
  )

  # Analysis summary
  summary_list$analysis <- list(
    n_factors = results@n_factors,
    extraction = results@method_details$extraction,
    rotation = results@method_details$rotation,
    flagging = results@method_details$flagging
  )

  # factor_characteristics is a list: 'characteristics' data.frame plus 'sed_matrix'
  factor_chars <- results@factor_characteristics
  if (is.list(factor_chars) && "characteristics" %in% names(factor_chars)) {
    summary_list$factors <- factor_chars$characteristics
  } else if (is.data.frame(factor_chars)) {
    summary_list$factors <- factor_chars
  } else {
    # Fallback: create basic summary from available data
    summary_list$factors <- data.frame(
      factor = paste0("F", seq_len(results@n_factors)),
      n_flagged = colSums(results@flagging),
      variance_pct = results@extraction$variance_explained * 100,
      composite_reliability = NA
    )
  }

  # Flagging summary
  flagged_per_factor <- colSums(results@flagging)
  unflagged <- sum(rowSums(results@flagging) == 0)
  confounded <- sum(rowSums(results@flagging) > 1)

  summary_list$flagging <- list(
    per_factor = flagged_per_factor,
    unflagged = unflagged,
    confounded = confounded
  )

  # Distinguishing/consensus
  summary_list$statements <- list(
    n_distinguishing = sum(sapply(results@distinguishing, length)),
    n_consensus = length(results@consensus)
  )

  if (verbose) {
    cat("\n")
    cli::cli_h1("Q-Sort Analysis Summary")

    cli::cli_h2("Data")
    cli::cli_text("{.strong Participants:} {summary_list$data$n_participants}")
    cli::cli_text("{.strong Statements:} {summary_list$data$n_statements}")
    cli::cli_text("{.strong Source:} {summary_list$data$source}")

    cli::cli_h2("Analysis Settings")
    cli::cli_text("{.strong Factors:} {summary_list$analysis$n_factors}")
    cli::cli_text("{.strong Extraction:} {summary_list$analysis$extraction}")
    cli::cli_text("{.strong Rotation:} {summary_list$analysis$rotation}")

    cli::cli_h2("Factor Statistics")
    factors_df <- summary_list$factors
    if (!is.null(factors_df) && is.data.frame(factors_df) && nrow(factors_df) > 0) {
      for (i in seq_len(nrow(factors_df))) {
        f <- factors_df[i, ]
        factor_name <- if ("factor" %in% names(f)) f$factor else paste0("F", i)
        n_flag <- if ("n_flagged" %in% names(f)) f$n_flagged else flagged_per_factor[i]
        var_pct <- if ("variance_pct" %in% names(f)) round(f$variance_pct, 1) else "N/A"
        rel <- if ("composite_reliability" %in% names(f) && !is.na(f$composite_reliability)) {
          round(f$composite_reliability, 3)
        } else {
          "N/A"
        }
        cli::cli_text("  {.strong {factor_name}}: {n_flag} Q-sorts, {var_pct}% variance, reliability = {rel}")
      }
    } else {
      cli::cli_text("  Factor statistics not available")
    }

    cli::cli_h2("Flagging")
    cli::cli_text("{.strong Unflagged:} {summary_list$flagging$unflagged}")
    cli::cli_text("{.strong Confounded:} {summary_list$flagging$confounded}")

    cli::cli_h2("Statements")
    cli::cli_text("{.strong Distinguishing:} {summary_list$statements$n_distinguishing}")
    cli::cli_text("{.strong Consensus:} {summary_list$statements$n_consensus}")
    cat("\n")
  }

  invisible(summary_list)
}


#' Parallel Analysis for Factor Selection
#'
#' @description
#' Perform parallel analysis to determine the optimal number of factors.
#'
#' @param data A QsortData object or correlation matrix
#' @param n_iter Number of random datasets to generate (default: 100)
#' @param percentile Percentile for threshold (default: 95)
#'
#' @return A data frame with eigenvalues and parallel analysis results
#' @export
parallel_analysis <- function(data, n_iter = 100, percentile = 95) {

  if (inherits(data, "QsortData")) {
    sorts <- data@sorts
    cor_mat <- qsort_correlation(data)
  } else if (is.matrix(data) && nrow(data) == ncol(data)) {
    cor_mat <- data
    sorts <- NULL
  } else {
    rlang::abort("data must be QsortData or correlation matrix")
  }

  n <- nrow(cor_mat)

  # Actual eigenvalues
  actual_eigen <- eigen(cor_mat, symmetric = TRUE)$values

  random_eigen <- matrix(0, nrow = n_iter, ncol = n)

  for (i in seq_len(n_iter)) {
    # Random data with same dimensions
    if (!is.null(sorts)) {
      random_sorts <- matrix(
        sample(as.vector(sorts), length(sorts), replace = TRUE),
        nrow = nrow(sorts), ncol = ncol(sorts)
      )
      random_cor <- cor(t(random_sorts), use = "pairwise.complete.obs")
    } else {
      random_data <- matrix(rnorm(n * n), nrow = n)
      random_cor <- cor(random_data)
    }
    random_eigen[i, ] <- eigen(random_cor, symmetric = TRUE)$values
  }

  threshold <- apply(random_eigen, 2, quantile, probs = percentile / 100)

  # Determine number of factors
  n_factors <- sum(actual_eigen > threshold)

  results <- data.frame(
    factor = seq_len(n),
    eigenvalue = actual_eigen,
    threshold = threshold,
    retain = actual_eigen > threshold
  )

  attr(results, "n_factors") <- n_factors
  class(results) <- c("parallel_analysis", "data.frame")

  return(results)
}

#' Choose a Readable Text Color for a Background
#'
#' @description
#' Pick black or white text for a given background color using the
#' perceived luminance formula (0.299 R + 0.587 G + 0.114 B). Backgrounds
#' brighter than the threshold get black text; darker ones get white.
#'
#' @param hex Character vector of colors (hex strings or R color names)
#' @param threshold Luminance cutoff in the 0 to 1 range (default 0.6)
#'
#' @return Character vector of "black" or "white", one per input color
#' @export
#'
#' @examples
#' contrast_text_color("#FFC72C")
#' contrast_text_color(c("#002554", "#FFFFFF"))
contrast_text_color <- function(hex, threshold = 0.6) {

  if (length(hex) == 0) {
    return(character(0))
  }

  rgb_vals <- grDevices::col2rgb(hex)
  lum <- (0.299 * rgb_vals[1, ] + 0.587 * rgb_vals[2, ] + 0.114 * rgb_vals[3, ]) / 255
  unname(ifelse(lum > threshold, "black", "white"))
}
