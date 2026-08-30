#' @title Bootstrap Methods for Q-Sort Analysis
#' @description
#' Bootstrap resampling methods for estimating confidence intervals and
#' stability of Q-sort analysis results. Based on Zabala & Pascual (2016).
#' @name bootstrap
#' @references
#' Zabala, A., & Pascual, U. (2016). Bootstrapping Q methodology to improve the
#' understanding of human perspectives. PloS one, 11(2), e0148087.
NULL

#' Bootstrap Q-Sort Analysis
#'
#' @description
#' Perform bootstrap resampling of Q-sort analysis to estimate confidence
#' intervals for factor loadings, factor scores, and other statistics.
#' This addresses a key limitation of standard Q analysis which provides
#' only a single SE value for all statements.
#'
#' @param data A QsortData object or results from qsort_analyze()
#' @param n_bootstrap Number of bootstrap iterations (default 1000)
#' @param nfactors Number of factors (uses original if data is QsortResults)
#' @param extraction Extraction method (default "pca")
#' @param rotation Rotation method (default "varimax")
#' @param ci_level Confidence interval level (default 0.95)
#' @param seed Random seed for reproducibility
#' @param parallel Logical; use parallel processing (default TRUE if available).
#'   Requires the 'future' and 'furrr' packages to be installed.
#' @param n_cores Number of cores for parallel processing (default: auto-detect,
#'   leaves one core free for system operations)
#' @param progress Logical; show progress bar (default TRUE). Uses 'progressr'
#'   package for parallel-compatible progress reporting.
#'
#' @return A QsortBootstrap object containing bootstrap results
#' @export
#'
#' @examples
#' \dontrun{
#' # Bootstrap from data
#' boot_results <- qsort_bootstrap(qdata, n_bootstrap = 1000, seed = 42)
#'
#' # Bootstrap from existing results
#' results <- qsort_analyze(qdata)
#' boot_results <- qsort_bootstrap(results, n_bootstrap = 2000)
#'
#' # View results
#' summary(boot_results)
#' plot(boot_results)
#' }
qsort_bootstrap <- function(data,
                            n_bootstrap = 1000,
                            nfactors = NULL,
                            extraction = "pca",
                            rotation = "varimax",
                            ci_level = 0.95,
                            seed = NULL,
                            parallel = TRUE,
                            n_cores = NULL,
                            progress = TRUE) {

  cli::cli_h1("Bootstrap Q-Sort Analysis")

  if (is.null(seed)) {
    seed <- sample.int(1e6, 1)
  }
  set.seed(seed)
  cli::cli_alert_info("Random seed: {seed}")

  # Extract original results or run initial analysis
  if (inherits(data, "QsortResults")) {
    original <- data
    qdata <- data@data
    if (is.null(nfactors)) nfactors <- data@n_factors
    extraction <- data@method_details$extraction
    rotation <- data@method_details$rotation
  } else if (inherits(data, "QsortData")) {
    qdata <- data
    if (is.null(nfactors)) nfactors <- 3
    original <- qsort_analyze(qdata, nfactors = nfactors,
                              extraction = extraction, rotation = rotation)
  } else {
    rlang::abort("data must be a QsortData or QsortResults object")
  }

  sorts <- qdata@sorts
  n_participants <- nrow(sorts)
  n_statements <- ncol(sorts)

  cli::cli_alert_info("Bootstrap iterations: {n_bootstrap}")
  cli::cli_alert_info("Participants: {n_participants}, Statements: {n_statements}")
  cli::cli_alert_info("Factors: {nfactors}")

  has_parallel <- parallel &&
                  requireNamespace("future", quietly = TRUE) &&
                  requireNamespace("furrr", quietly = TRUE)

  if (parallel && !has_parallel) {
    cli::cli_alert_warning("Parallel processing requested but 'future'/'furrr' not installed.")
    cli::cli_alert_info("Install with: install.packages(c('future', 'furrr'))")
    cli::cli_alert_info("Falling back to sequential processing.")
  }

  # Set number of workers for parallel processing
  if (is.null(n_cores)) {
    n_cores <- max(1, parallel::detectCores() - 1)
  }
  n_cores <- min(n_cores, n_bootstrap)  # Don't use more cores than iterations

  if (has_parallel) {
    cli::cli_alert_info("Using parallel processing with {n_cores} workers")
  }

  # Target loadings for alignment
  target_loadings <- original@rotation$loadings

  loading_samples <- array(
    NA,
    dim = c(n_participants, nfactors, n_bootstrap),
    dimnames = list(rownames(sorts), paste0("F", 1:nfactors), NULL)
  )

  score_samples <- array(
    NA,
    dim = c(n_statements, nfactors, n_bootstrap),
    dimnames = list(colnames(sorts), paste0("F", 1:nfactors), NULL)
  )

  flag_samples <- array(
    FALSE,
    dim = c(n_participants, nfactors, n_bootstrap),
    dimnames = list(rownames(sorts), paste0("F", 1:nfactors), NULL)
  )

  run_single_bootstrap <- function(b, sorts, n_participants, nfactors,
                                   extraction, rotation, target_loadings) {
    # Resample Q-sorts with replacement
    sample_indices <- sample(n_participants, replace = TRUE)
    boot_sorts <- sorts[sample_indices, , drop = FALSE]

    rownames(boot_sorts) <- paste0(rownames(sorts)[sample_indices], "_", seq_along(sample_indices))

    # Run analysis on bootstrap sample
    boot_result <- tryCatch({
      run_bootstrap_iteration(
        boot_sorts,
        nfactors = nfactors,
        extraction = extraction,
        rotation = rotation,
        target_loadings = target_loadings,
        original_rownames = rownames(sorts)
      )
    }, error = function(e) NULL)

    return(boot_result)
  }

  # Run bootstrap iterations (parallel or sequential)
  if (has_parallel) {
    old_plan <- future::plan(future::multisession, workers = n_cores)
    on.exit(future::plan(old_plan), add = TRUE)

    # Run parallel bootstrap with progress
    if (progress && requireNamespace("progressr", quietly = TRUE)) {
      progressr::handlers(global = TRUE)
      progressr::handlers("cli")

      boot_results <- progressr::with_progress({
        p <- progressr::progressor(steps = n_bootstrap)
        furrr::future_map(seq_len(n_bootstrap), function(b) {
          result <- run_single_bootstrap(b, sorts, n_participants, nfactors,
                                         extraction, rotation, target_loadings)
          p()
          result
        }, .options = furrr::furrr_options(seed = TRUE))
      })
    } else {
      boot_results <- furrr::future_map(seq_len(n_bootstrap), function(b) {
        run_single_bootstrap(b, sorts, n_participants, nfactors,
                             extraction, rotation, target_loadings)
      }, .options = furrr::furrr_options(seed = TRUE))
    }

    # Fill arrays from results
    for (b in seq_len(n_bootstrap)) {
      if (!is.null(boot_results[[b]])) {
        loading_samples[, , b] <- boot_results[[b]]$loadings
        score_samples[, , b] <- boot_results[[b]]$scores
        flag_samples[, , b] <- boot_results[[b]]$flags
      }
    }
  } else {
    # Sequential processing with progress bar
    if (progress) {
      cli::cli_progress_bar("Bootstrapping", total = n_bootstrap)
    }

    for (b in seq_len(n_bootstrap)) {
      boot_result <- run_single_bootstrap(b, sorts, n_participants, nfactors,
                                          extraction, rotation, target_loadings)

      if (!is.null(boot_result)) {
        loading_samples[, , b] <- boot_result$loadings
        score_samples[, , b] <- boot_result$scores
        flag_samples[, , b] <- boot_result$flags
      }

      if (progress) {
        cli::cli_progress_update()
      }
    }

    if (progress) {
      cli::cli_progress_done()
    }
  }

  # Count successful bootstrap iterations (those without all NAs in loadings)
  n_successful <- sum(apply(loading_samples, 3, function(x) !all(is.na(x))))
  success_rate <- n_successful / n_bootstrap

  # Warn if success rate is below threshold

  if (success_rate < 0.5) {
    rlang::abort(c(
      "Bootstrap analysis failed",
      "x" = sprintf("Only %d of %d iterations succeeded (%.1f%%)", n_successful, n_bootstrap, success_rate * 100),
      "i" = "This indicates serious problems with the data or model specification",
      "i" = "Consider checking for multicollinearity, zero-variance Q-sorts, or too few participants"
    ))
  } else if (success_rate < 0.9) {
    rlang::warn(c(
      "Low bootstrap success rate",
      "!" = sprintf("%d of %d iterations failed (%.1f%% success rate)", n_bootstrap - n_successful, n_bootstrap, success_rate * 100),
      "i" = "Results may be less reliable; consider investigating failed iterations"
    ))
  }

  cli::cli_alert_info("Bootstrap iterations: {n_successful}/{n_bootstrap} successful ({round(success_rate * 100, 1)}%)")

  alpha <- 1 - ci_level

  cli::cli_h2("Computing confidence intervals")

  # Loading CIs
  loading_ci <- compute_bootstrap_ci(
    loading_samples,
    target_loadings,
    alpha = alpha,
    type = "loadings",
    participant_ids = rownames(sorts)
  )

  # Score CIs
  score_ci <- compute_bootstrap_ci(
    score_samples,
    as.matrix(original@factor_scores[, grep("_zscore$", names(original@factor_scores))]),
    alpha = alpha,
    type = "scores",
    statement_ids = colnames(sorts)
  )

  # Flagging frequency: successful iterations as the denominator, not n_bootstrap
  flag_frequency <- apply(flag_samples, c(1, 2), sum, na.rm = TRUE) / n_successful
  rownames(flag_frequency) <- rownames(sorts)
  colnames(flag_frequency) <- paste0("F", 1:nfactors)

  # Stability metrics
  stability <- compute_stability_metrics(
    loading_samples,
    score_samples,
    flag_samples,
    original
  )

  cli::cli_alert_success("Bootstrap analysis complete")

  new(
    "QsortBootstrap",
    original = original,
    n_bootstrap = as.integer(n_bootstrap),
    loading_samples = loading_samples,
    score_samples = score_samples,
    loading_ci = loading_ci,
    score_ci = score_ci,
    flag_frequency = flag_frequency,
    stability_metrics = stability,
    seed = as.integer(seed)
  )
}

#' Run Single Bootstrap Iteration
#' @keywords internal
run_bootstrap_iteration <- function(boot_sorts, nfactors, extraction, rotation,
                                    target_loadings, original_rownames) {

  n_orig <- length(original_rownames)
  n_statements <- ncol(boot_sorts)

  # Correlation matrix
  cor_matrix <- cor(t(boot_sorts), use = "pairwise.complete.obs")

  if (any(is.na(cor_matrix))) {
    cor_matrix[is.na(cor_matrix)] <- 0
  }

  # Factor extraction
  extraction_result <- qsort_extract(cor_matrix, nfactors, method = extraction)

  # Rotation
  rotation_result <- qsort_rotate(extraction_result, method = rotation)

  # Align factors with target (handle axis reflection and reordering)
  aligned <- align_factors(rotation_result$loadings, target_loadings)

  # Map back to original participants
  # For each original participant, find their resampled versions and average
  mapped_loadings <- matrix(NA, nrow = n_orig, ncol = nfactors)
  rownames(mapped_loadings) <- original_rownames

  for (i in seq_len(n_orig)) {
    # Find rows corresponding to this original participant
    # Escape regex metacharacters in participant IDs
    escaped_name <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", original_rownames[i])
    pattern <- paste0("^", escaped_name, "_")
    matches <- grep(pattern, rownames(boot_sorts))

    if (length(matches) > 0) {
      mapped_loadings[i, ] <- colMeans(aligned[matches, , drop = FALSE])
    }
  }

  # Flagging
  flags <- qsort_flag(mapped_loadings, nstat = n_statements, method = "auto")

  # Factor scores
  scores <- matrix(NA, nrow = n_statements, ncol = nfactors)
  for (f in seq_len(nfactors)) {
    flagged_indices <- which(flags[, f])
    if (length(flagged_indices) > 0) {
      # Use original sorts for flagged participants
      # Build proper regex pattern for each flagged participant
      patterns <- paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1",
                                   original_rownames[flagged_indices]), "_")
      combined_pattern <- paste(patterns, collapse = "|")
      matching_rows <- grep(combined_pattern, rownames(boot_sorts))

      if (length(matching_rows) > 0) {
        orig_sorts <- boot_sorts[matching_rows, , drop = FALSE]

        # Use Brown (1980) weighted formula to match qsort_scores()
        # Get loadings for flagged participants and compute weights: w = f / (1 - f^2)
        flagged_loadings <- mapped_loadings[flagged_indices, f]
        safe_loadings <- sign(flagged_loadings) * pmax(pmin(abs(flagged_loadings), 0.9999), 0.0001)
        factor_weights <- safe_loadings / (1 - safe_loadings^2)

        # For bootstrap samples, we need to match weights to resampled rows
        # Each original participant may appear multiple times in orig_sorts
        # Map weights to the matching rows
        weights_expanded <- numeric(length(matching_rows))
        for (j in seq_along(flagged_indices)) {
          orig_name <- original_rownames[flagged_indices[j]]
          escaped <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", orig_name)
          pattern_j <- paste0("^", escaped, "_")
          matches_j <- grep(pattern_j, rownames(boot_sorts)[matching_rows])
          if (length(matches_j) > 0) {
            weights_expanded[matches_j] <- factor_weights[j]
          }
        }

        # Compute weighted scores (matching qsort_scores approach)
        weighted_raw <- sweep(orig_sorts, 1, weights_expanded, "*")
        weighted_sum <- colSums(weighted_raw)

        # Standardize to z-scores
        wsum_mean <- mean(weighted_sum)
        wsum_sd <- sd(weighted_sum)
        if (wsum_sd > 0) {
          scores[, f] <- (weighted_sum - wsum_mean) / wsum_sd
        } else {
          scores[, f] <- 0
        }
      }
    }
  }

  list(
    loadings = mapped_loadings,
    scores = scores,
    flags = flags
  )
}

#' Procrustes Rotation for Factor Alignment
#'
#' @description
#' Apply Procrustes rotation to align bootstrapped factor loadings with target
#' loadings. This is the standard method for handling axis reflection (sign
#' indeterminacy) and axis reordering in bootstrap Q-methodology analysis,
#' as recommended by Zabala & Pascual (2016).
#'
#' @param loadings Bootstrap factor loadings matrix
#' @param target Target factor loadings matrix (usually from original analysis)
#'
#' @return Aligned loadings matrix after Procrustes rotation
#' @export
#'
#' @details
#' Procrustes rotation finds the orthogonal transformation matrix T that
#' minimizes the sum of squared differences between the rotated loadings
#' and the target:
#'
#' \deqn{\min_T ||L \times T - Target||^2}
#'
#' subject to T'T = I (orthogonality constraint).
#'
#' The solution uses singular value decomposition (SVD):
#' \deqn{Target' \times L = U \times D \times V'}
#' \deqn{T = V \times U'}
#'
#' This handles both axis reflection and reordering automatically.
#'
#' @references
#' Zabala, A., & Pascual, U. (2016). Bootstrapping Q methodology to improve
#' the understanding of human perspectives. PLoS ONE, 11(2), e0148087.
#'
#' @examples
#' \dontrun{
#' # Align bootstrap loadings to original
#' aligned <- qsort_procrustes(boot_loadings, original_loadings)
#' }
qsort_procrustes <- function(loadings, target) {

  if (is.null(loadings) || is.null(target)) {
    return(loadings)
  }

  if (!is.matrix(loadings)) {
    loadings <- as.matrix(loadings)
  }
  if (!is.matrix(target)) {
    target <- as.matrix(target)
  }

  if (nrow(loadings) == 0 || ncol(loadings) == 0) {
    return(loadings)
  }
  if (nrow(target) == 0 || ncol(target) == 0) {
    return(loadings)
  }

  if (ncol(loadings) != ncol(target)) {
    warning("Number of factors differs between loadings (", ncol(loadings),
            ") and target (", ncol(target), "). Returning unaligned loadings.")
    return(loadings)
  }

  nfactors <- ncol(loadings)

  if (nfactors == 0) {
    return(loadings)
  }

  # Handle case where number of rows differs (bootstrap resampling)
  # In this case, we need to match rows by participant ID or use a subset
  if (nrow(loadings) != nrow(target)) {
    # Try to match by rownames
    common_rows <- intersect(rownames(loadings), rownames(target))
    if (length(common_rows) > nfactors) {
      loadings_sub <- loadings[common_rows, , drop = FALSE]
      target_sub <- target[common_rows, , drop = FALSE]
    } else {
      # Fall back to correlation-based alignment
      return(align_factors_correlation(loadings, target))
    }
  } else {
    loadings_sub <- loadings
    target_sub <- target
  }

  # Procrustes rotation using SVD
  # The goal is to find rotation matrix T such that loadings %*% T ~ target

  # Step 1: Compute cross-product matrix
  cross_product <- t(target_sub) %*% loadings_sub

  # Step 2: SVD of cross-product
  svd_result <- svd(cross_product)

  # Step 3: Rotation matrix T = V %*% U'
  rotation_matrix <- svd_result$v %*% t(svd_result$u)

  # Step 4: Apply rotation to full loadings matrix
  aligned <- loadings %*% rotation_matrix

  # Preserve row and column names
  rownames(aligned) <- rownames(loadings)
  colnames(aligned) <- colnames(loadings)

  return(aligned)
}

#' Align Bootstrapped Factors with Target (Correlation-based)
#'
#' @description
#' Align bootstrapped factor loadings with target loadings using correlation-based
#' matching. This is a fallback method when Procrustes rotation cannot be used
#' (e.g., when sample sizes differ significantly).
#'
#' @param loadings Bootstrap factor loadings
#' @param target Target factor loadings
#'
#' @return Aligned loadings matrix
#' @keywords internal
align_factors_correlation <- function(loadings, target) {

  nfactors <- ncol(loadings)

  if (is.null(loadings) || is.null(target)) {
    return(loadings)
  }

  if (nfactors == 0 || ncol(target) == 0) {
    return(loadings)
  }

  aligned <- loadings

  # Compute correlation between factor loading distributions
  # Compare using sorted values (distribution-based matching)
  cor_matrix <- matrix(0, nrow = nfactors, ncol = nfactors)
  for (i in seq_len(nfactors)) {
    for (j in seq_len(nfactors)) {
      boot_sorted <- sort(loadings[, i])
      target_sorted <- sort(target[, j])

      # Align lengths by interpolation if needed
      if (length(boot_sorted) != length(target_sorted)) {
        n <- min(length(boot_sorted), length(target_sorted))
        boot_interp <- approx(seq_along(boot_sorted), boot_sorted, n = n)$y
        target_interp <- approx(seq_along(target_sorted), target_sorted, n = n)$y
        cor_matrix[i, j] <- cor(boot_interp, target_interp, use = "complete.obs")
      } else {
        cor_matrix[i, j] <- cor(boot_sorted, target_sorted, use = "complete.obs")
      }
    }
  }

  cor_matrix[is.na(cor_matrix)] <- 0

  # Find best matching using greedy assignment
  used_target <- logical(nfactors)
  assignment <- integer(nfactors)
  sign_flip <- logical(nfactors)

  for (i in seq_len(nfactors)) {
    available <- which(!used_target)
    if (length(available) == 0) break

    abs_cors <- abs(cor_matrix[i, available])
    best_idx <- which.max(abs_cors)
    best_j <- available[best_idx]
    assignment[i] <- best_j
    used_target[best_j] <- TRUE
    sign_flip[i] <- cor_matrix[i, best_j] < 0
  }

  # Reorder columns and apply sign flips
  for (i in seq_len(nfactors)) {
    j <- assignment[i]
    if (j > 0 && j <= nfactors) {
      if (sign_flip[i]) {
        aligned[, i] <- -loadings[, j]
      } else {
        aligned[, i] <- loadings[, j]
      }
    }
  }

  return(aligned)
}

#' Align Bootstrapped Factors with Target
#'
#' @description
#' Align bootstrapped factor loadings with target (original) loadings
#' to handle axis reflection (sign indeterminacy) and axis reordering.
#' Uses Procrustes rotation when possible, falls back to correlation-based
#' alignment otherwise.
#'
#' @param loadings Bootstrap factor loadings
#' @param target Target factor loadings
#'
#' @return Aligned loadings matrix
#' @keywords internal
align_factors <- function(loadings, target) {
  # Use Procrustes rotation as primary method
  qsort_procrustes(loadings, target)
}

#' Compute Bootstrap Confidence Intervals
#' @keywords internal
compute_bootstrap_ci <- function(samples, original, alpha, type, ...) {

  dims <- dim(samples)
  n_items <- dims[1]
  n_factors <- dims[2]
  n_boot <- dims[3]

  args <- list(...)
  if (type == "loadings") {
    item_ids <- args$participant_ids
  } else {
    item_ids <- args$statement_ids
  }

  results <- list()

  for (f in seq_len(n_factors)) {
    factor_name <- paste0("F", f)

    ci_data <- data.frame(
      item = item_ids,
      factor = factor_name,
      original = original[, f],
      boot_mean = apply(samples[, f, ], 1, mean, na.rm = TRUE),
      boot_sd = apply(samples[, f, ], 1, sd, na.rm = TRUE),
      ci_lower = apply(samples[, f, ], 1, quantile, probs = alpha / 2, na.rm = TRUE),
      ci_upper = apply(samples[, f, ], 1, quantile, probs = 1 - alpha / 2, na.rm = TRUE),
      bias = NA
    )

    ci_data$bias <- ci_data$boot_mean - ci_data$original

    # Bias-corrected estimate
    ci_data$bc_estimate <- ci_data$original - ci_data$bias

    results[[factor_name]] <- ci_data
  }

  do.call(rbind, results)
}

#' Compute Stability Metrics
#' @keywords internal
compute_stability_metrics <- function(loading_samples, score_samples, flag_samples, original) {

  n_boot <- dim(loading_samples)[3]

  # Loading stability: correlation between bootstrap and original
  loading_stability <- apply(loading_samples, 3, function(boot_load) {
    tryCatch({
      cor(as.vector(boot_load), as.vector(original@rotation$loadings),
          use = "pairwise.complete.obs")
    }, error = function(e) NA)
  })

  # Score stability
  orig_scores <- as.matrix(original@factor_scores[, grep("_zscore$", names(original@factor_scores))])
  score_stability <- apply(score_samples, 3, function(boot_score) {
    tryCatch({
      cor(as.vector(boot_score), as.vector(orig_scores),
          use = "pairwise.complete.obs")
    }, error = function(e) NA)
  })

  # Flagging consistency: proportion of times each Q-sort is flagged same as original
  original_flags <- original@flagging
  flag_consistency <- apply(flag_samples, 3, function(boot_flag) {
    mean(boot_flag == original_flags, na.rm = TRUE)
  })

  list(
    loading_stability = loading_stability,
    score_stability = score_stability,
    flag_consistency = flag_consistency,
    summary = data.frame(
      metric = c("Loading Stability", "Score Stability", "Flag Consistency"),
      mean = c(mean(loading_stability, na.rm = TRUE),
               mean(score_stability, na.rm = TRUE),
               mean(flag_consistency, na.rm = TRUE)),
      sd = c(sd(loading_stability, na.rm = TRUE),
             sd(score_stability, na.rm = TRUE),
             sd(flag_consistency, na.rm = TRUE)),
      min = c(min(loading_stability, na.rm = TRUE),
              min(score_stability, na.rm = TRUE),
              min(flag_consistency, na.rm = TRUE)),
      max = c(max(loading_stability, na.rm = TRUE),
              max(score_stability, na.rm = TRUE),
              max(flag_consistency, na.rm = TRUE))
    )
  )
}

#' Summary Method for QsortBootstrap
#'
#' @param object A QsortBootstrap object
#' @return Invisibly returns the bootstrap object
#' @exportMethod summary
#' @rdname summary-QsortBootstrap-method
setMethod("summary", "QsortBootstrap", function(object) {

  cat("\n")
  cli::cli_h1("Bootstrap Q-Sort Analysis Summary")

  cli::cli_h2("Bootstrap Parameters")
  cli::cli_text("{.strong Iterations:} {object@n_bootstrap}")
  cli::cli_text("{.strong Random seed:} {object@seed}")

  cli::cli_h2("Stability Metrics")
  stability <- object@stability_metrics$summary
  for (i in seq_len(nrow(stability))) {
    cli::cli_text("{.strong {stability$metric[i]}:} {round(stability$mean[i], 3)} (SD = {round(stability$sd[i], 3)})")
  }

  cli::cli_h2("Flagging Consistency")
  flag_freq <- object@flag_frequency
  for (f in seq_len(ncol(flag_freq))) {
    original_flagged <- which(object@original@flagging[, f])
    if (length(original_flagged) > 0) {
      avg_freq <- mean(flag_freq[original_flagged, f])
      cli::cli_text("Factor {f}: {round(avg_freq * 100, 1)}% average consistency for originally flagged Q-sorts")
    }
  }

  cli::cli_h2("Confidence Interval Summary")
  loading_ci <- object@loading_ci
  avg_width <- mean(loading_ci$ci_upper - loading_ci$ci_lower, na.rm = TRUE)
  cli::cli_text("{.strong Average loading CI width:} {round(avg_width, 3)}")

  score_ci <- object@score_ci
  avg_score_width <- mean(score_ci$ci_upper - score_ci$ci_lower, na.rm = TRUE)
  cli::cli_text("{.strong Average score CI width:} {round(avg_score_width, 3)}")

  cat("\n")
  invisible(object)
})
