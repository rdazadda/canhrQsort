NULL

#' Complete Q-Sort Analysis
#'
#' @description
#' Perform a complete Q-sort analysis including correlation, factor extraction,
#' rotation, flagging, and computation of factor scores. This is the main
#' analysis function that wraps all individual steps.
#'
#' @param data A QsortData object or matrix of Q-sorts
#' @param nfactors Number of factors to extract (default: auto-determined)
#' @param extraction Extraction method: "pca" (default), "centroid", or "minres"
#' @param rotation Rotation method: "varimax" (default), "quartimax", "oblimin",
#'   "promax", or "none"
#' @param cor_method Correlation method: "pearson" (default), "spearman", or "kendall"
#' @param flagging Flagging method: "auto" (default), "manual", or "theoretical"
#' @param flag_threshold For theoretical flagging, the significance threshold
#' @param forced Logical; is the distribution forced? (default TRUE)
#' @param av_rel_coef Average reliability coefficient for individual Q-sorts
#'   (default 0.8). Used in computing composite reliability via the Spearman-Brown
#'   formula. Standard Q-methodology practice uses 0.8 (Brown, 1980).
#'
#' @return A QsortResults object containing all analysis results
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic analysis
#' results <- qsort_analyze(qdata)
#'
#' # Custom settings
#' results <- qsort_analyze(
#'   qdata,
#'   nfactors = 3,
#'   extraction = "centroid",
#'   rotation = "varimax",
#'   flagging = "auto"
#' )
#'
#' # View results
#' summary(results)
#' plot(results)
#' }
qsort_analyze <- function(data,
                          nfactors = NULL,
                          extraction = c("pca", "centroid", "minres"),
                          rotation = c("varimax", "quartimax", "oblimin", "promax", "none"),
                          cor_method = c("pearson", "spearman", "kendall"),
                          flagging = c("auto", "manual", "theoretical"),
                          flag_threshold = NULL,
                          forced = TRUE,
                          av_rel_coef = 0.8,
                          distinguish_criterion = c("all", "any"),
                          flag_p_level = 0.05,
                          flag_majority = TRUE) {

  call <- match.call()

  extraction <- match.arg(extraction)
  rotation <- match.arg(rotation)
  cor_method <- match.arg(cor_method)
  flagging <- match.arg(flagging)
  distinguish_criterion <- match.arg(distinguish_criterion)

  if (!inherits(data, "QsortData")) {
    data <- qsort_data(data, validate = TRUE)
  }

  n_participants <- nrow(data@sorts)
  n_statements <- ncol(data@sorts)

  if (n_participants < 2) stop("Need at least 2 participants for Q-sort analysis")
  if (n_statements < 3) stop("Need at least 3 statements for Q-sort analysis")

  if (!is.null(nfactors)) {
    if (!is.numeric(nfactors) || length(nfactors) != 1 || nfactors < 1)
      stop("nfactors must be a positive integer")
    nfactors <- as.integer(nfactors)
    if (nfactors > n_participants)
      stop("Cannot extract more factors than participants")
  }

  cor_matrix <- qsort_correlation(data, method = cor_method)

  if (is.null(nfactors)) {
    nfactors <- determine_nfactors(cor_matrix, data@sorts)
  }

  extraction_result <- qsort_extract(
    cor_matrix,
    nfactors = nfactors,
    method = extraction
  )

  rotation_result <- qsort_rotate(
    extraction_result,
    method = rotation
  )

  # Heywood cases (communalities > 1)
  communalities <- rowSums(rotation_result$loadings^2)
  heywood_idx <- which(communalities > 1)
  if (length(heywood_idx) > 0) {
    warning("Heywood case(s) detected: communality > 1 for Q-sort(s) ",
            paste(heywood_idx, collapse = ", "))
  }

  flagging_matrix <- qsort_flag(
    rotation_result$loadings,
    nstat = ncol(data@sorts),  # Number of statements for significance threshold
    method = flagging,
    threshold = flag_threshold,
    p_level = flag_p_level,
    majority = flag_majority
  )

  factor_scores <- qsort_scores(
    data@sorts,
    flagging_matrix,
    rotation_result$loadings,
    forced = forced,
    distribution = data@distribution
  )

  factor_chars_result <- compute_factor_characteristics(
    data@sorts,
    flagging_matrix,
    rotation_result$loadings,
    extraction_result,
    factor_scores = factor_scores,
    av_rel_coef = av_rel_coef
  )
  factor_chars <- factor_chars_result$characteristics
  sed_matrix <- factor_chars_result$sed_matrix

  distinction <- qsort_distinguish(
    factor_scores,
    statements = data@statements,
    sed_matrix = sed_matrix,
    se_factors = factor_chars$se_factor,
    criterion = distinguish_criterion
  )

  method_details <- list(
    extraction = extraction,
    rotation = rotation,
    cor_method = cor_method,
    flagging = flagging,
    flag_threshold = flag_threshold,
    flag_p_level = flag_p_level,
    flag_majority = flag_majority,
    forced = forced,
    av_rel_coef = av_rel_coef,
    sed_matrix = sed_matrix,
    se_factors = factor_chars$se_factor,
    distinguishing_significance = distinction$distinguishing_significance,
    comparison_details = distinction$comparison_df,
    distinction = distinction
  )

  results <- qsort_results(
    data = data,
    correlation = cor_matrix,
    extraction = extraction_result,
    rotation = rotation_result,
    flagging = flagging_matrix,
    factor_scores = factor_scores,
    distinguishing = distinction$distinguishing,
    consensus = distinction$consensus,
    factor_characteristics = factor_chars_result,  # Store full list with characteristics and sed_matrix
    n_factors = nfactors,
    method_details = method_details,
    call = call
  )

  results
}

#' Compute Q-Sort Correlation Matrix
#'
#' @description
#' Compute the correlation matrix between Q-sorts (by-person correlation).
#'
#' @param data A QsortData object or matrix
#' @param method Correlation method: "pearson", "spearman", or "kendall"
#'
#' @return A correlation matrix
#' @export
#'
#' @details
#' - **Pearson**: Standard linear correlation, most common for Q-sorts
#' - **Spearman**: Rank-based correlation, robust to outliers
#' - **Kendall**: Rank-based tau correlation, more robust but computationally slower
qsort_correlation <- function(data, method = c("pearson", "spearman", "kendall")) {

  method <- match.arg(method)

  if (inherits(data, "QsortData")) {
    sorts <- data@sorts
  } else {
    sorts <- as.matrix(data)
  }

  row_vars <- apply(sorts, 1, var, na.rm = TRUE)
  zero_var_idx <- which(row_vars == 0 | is.na(row_vars))
  if (length(zero_var_idx) > 0) {
    warning("Zero-variance Q-sorts detected at indices: ",
            paste(zero_var_idx, collapse = ", "))
  }

  # Transpose for by-person correlation (Q-sorts correlate with each other)
  cor_matrix <- cor(t(sorts), method = method, use = "pairwise.complete.obs")

  if (!is.null(rownames(sorts))) {
    rownames(cor_matrix) <- colnames(cor_matrix) <- rownames(sorts)
  }

  return(cor_matrix)
}

#' Determine Optimal Number of Factors
#'
#' @description
#' Determine the optimal number of factors using parallel analysis and
#' eigenvalue criteria. This implementation follows standard practices
#' for Q-methodology.
#'
#' @param cor_matrix Correlation matrix
#' @param data Original data matrix (for parallel analysis)
#' @param criteria Method: "parallel" (default), "kaiser", or "both"
#'
#' @return Integer number of factors
#' @keywords internal
#'
#' @details
#' Parallel analysis compares observed eigenvalues against eigenvalues from
#' random data with the same dimensions. The number of factors is determined
#' by how many observed eigenvalues exceed the 95th percentile of random
#' eigenvalues.
#'
#' For Q-methodology, the random data preserves the distribution structure
#' by permuting within columns (statements) rather than generating completely
#' random values.
determine_nfactors <- function(cor_matrix, data, criteria = "parallel") {

  n <- nrow(cor_matrix)
  n_vars <- ncol(data)
  n_obs <- nrow(data)

  # Eigenvalue decomposition
  eigen_result <- eigen(cor_matrix, symmetric = TRUE)
  eigenvalues <- eigen_result$values

  # Kaiser criterion (eigenvalues > 1)
  kaiser_n <- sum(eigenvalues > 1)

  # Parallel analysis
  if (criteria %in% c("parallel", "both")) {
    # Generate random data for parallel analysis
    # Use permutation within columns to preserve marginal distributions
    # This is more appropriate for Q-sort data than pure random generation
    n_iter <- 100

    random_eigenvalues <- matrix(0, nrow = n_iter, ncol = n)

    for (i in seq_len(n_iter)) {
      # Permute each column (statement) independently
      # This preserves the distribution of each statement but breaks correlations
      random_data <- apply(data, 2, function(col) sample(col, replace = FALSE))

      # Compute correlation matrix (by-person, like Q-methodology)
      random_cor <- cor(t(random_data), use = "pairwise.complete.obs")

      # Handle potential NA in correlation matrix
      if (any(is.na(random_cor))) {
        random_cor[is.na(random_cor)] <- 0
        diag(random_cor) <- 1
      }

      random_eigenvalues[i, ] <- eigen(random_cor, symmetric = TRUE)$values
    }

    # 95th percentile of random eigenvalues
    threshold <- apply(random_eigenvalues, 2, quantile, probs = 0.95)

    parallel_n <- sum(eigenvalues > threshold)
  } else {
    parallel_n <- kaiser_n
  }

  # Use parallel analysis as the primary criterion when available (Horn, 1965).
  # Kaiser is a fallback when parallel analysis was skipped.
  # Cap at 7 as recommended in Q-methodology (Brown, 1980); ensure at least 2.
  if (criteria %in% c("parallel", "both")) {
    nfactors <- max(2, min(parallel_n, 7))
  } else {
    nfactors <- max(2, min(kaiser_n, 7))
  }

  return(nfactors)
}

#' Factor Extraction
#'
#' @description
#' Extract factors from the correlation matrix using PCA, centroid,
#' or minimum residual methods.
#'
#' @param cor_matrix Correlation matrix
#' @param nfactors Number of factors to extract
#' @param method Extraction method: "pca", "centroid", or "minres"
#'
#' @return List with loadings, eigenvalues, and variance explained
#' @export
qsort_extract <- function(cor_matrix, nfactors, method = c("pca", "centroid", "minres")) {

  method <- match.arg(method)
  n <- nrow(cor_matrix)

  if (!is.matrix(cor_matrix) || nrow(cor_matrix) != ncol(cor_matrix))
    stop("cor_matrix must be a square matrix")

  if (any(is.na(cor_matrix))) {
    warning("Correlation matrix contains NA values; replacing with 0")
    cor_matrix[is.na(cor_matrix)] <- 0
  }

  det_cor <- det(cor_matrix)
  if (abs(det_cor) < .Machine$double.eps * n^2)
    warning("Correlation matrix is nearly singular")

  if (method == "pca") {
    # Principal Component Analysis
    eigen_result <- eigen(cor_matrix, symmetric = TRUE)
    eigenvalues <- eigen_result$values
    eigenvectors <- eigen_result$vectors

    if (any(eigenvalues[1:nfactors] < 0)) {
      warning("Negative eigenvalues detected; setting to 0")
      eigenvalues <- pmax(eigenvalues, 0)
    }

    loadings <- eigenvectors[, 1:nfactors, drop = FALSE] %*%
      diag(sqrt(eigenvalues[1:nfactors]), nrow = nfactors)

    rownames(loadings) <- rownames(cor_matrix)
    colnames(loadings) <- paste0("F", 1:nfactors)

    variance_explained <- eigenvalues[1:nfactors] / sum(eigenvalues)
    cumulative_var <- cumsum(variance_explained)

  } else if (method == "centroid") {
    # Centroid factor extraction
    loadings <- centroid_extraction(cor_matrix, nfactors)
    eigenvalues <- colSums(loadings^2)
    variance_explained <- eigenvalues / n
    cumulative_var <- cumsum(variance_explained)

  } else if (method == "minres") {
    # Minimum residual (using psych package)
    fa_result <- psych::fa(cor_matrix, nfactors = nfactors, fm = "minres", rotate = "none")
    loadings <- as.matrix(fa_result$loadings)
    eigenvalues <- fa_result$values[1:nfactors]
    variance_explained <- eigenvalues / n
    cumulative_var <- cumsum(variance_explained)

    rownames(loadings) <- rownames(cor_matrix)
    colnames(loadings) <- paste0("F", 1:nfactors)
  }

  # Wide unrotated view for the audit trail: up to min(8, n-1) factors
  # regardless of how many are retained, with the cumulative communalities
  # it implies
  wide_k <- min(8L, n - 1L)
  unrotated_wide <- if (wide_k <= nfactors) {
    loadings
  } else {
    tryCatch(suppressWarnings({
      if (method == "pca") {
        ev <- eigen(cor_matrix, symmetric = TRUE)
        lw <- ev$vectors[, 1:wide_k, drop = FALSE] %*%
          diag(sqrt(pmax(ev$values[1:wide_k], 0)), nrow = wide_k)
        lw
      } else if (method == "centroid") {
        centroid_extraction(cor_matrix, wide_k)
      } else {
        # A second minres fit just for the audit view is not worth its
        # cost; the retained components stand in
        loadings
      }
    }), error = function(e) loadings)
  }
  rownames(unrotated_wide) <- rownames(cor_matrix)
  colnames(unrotated_wide) <- paste0("F", seq_len(ncol(unrotated_wide)))
  sq <- unrotated_wide^2
  cumulative_communalities <- if (ncol(sq) > 1) t(apply(sq, 1, cumsum)) else sq
  colnames(cumulative_communalities) <- colnames(unrotated_wide)

  list(
    loadings = loadings,
    eigenvalues = eigenvalues[1:nfactors],
    variance_explained = variance_explained,
    cumulative_variance = cumulative_var,
    unrotated_wide = unrotated_wide,
    cumulative_communalities = cumulative_communalities,
    method = method
  )
}

#' Centroid Factor Extraction
#'
#' @description
#' Extract factors using the centroid method as described in Brown (1980,
#' pp. 208-224). This is the traditional factor extraction method preferred
#' in Q-methodology due to its theoretical indeterminacy, which aligns with
#' Q's focus on exploring subjective perspectives.
#'
#' @param cor_matrix Correlation matrix
#' @param nfactors Number of factors to extract
#' @param max_iter Maximum iterations for convergence (default 100)
#' @param tolerance Convergence tolerance (default 1e-5)
#'
#' @return Matrix of factor loadings
#' @keywords internal
#'
#' @details
#' The centroid method iteratively extracts factors by:
#' 1. Reflecting variables with negative column sums to maximize positive manifold
#' 2. Computing factor loadings as column sums divided by sqrt(total sum)
#' 3. Iterating until convergence for each factor
#' 4. Computing residual matrix and repeating for next factor
#'
#' This implementation follows Brown (1980) and matches the qmethod package.
#'
#' @references
#' Brown, S. R. (1980). Political subjectivity: Applications of Q methodology
#' in political science. Yale University Press. (pp. 208-224)
centroid_extraction <- function(cor_matrix, nfactors, max_iter = 100, tolerance = 1e-5) {

  n <- nrow(cor_matrix)
  loadings <- matrix(NA_real_, nrow = n, ncol = nfactors)
  tmat <- cor_matrix

  convergence_info <- list()

  for (f in seq_len(nfactors)) {
    diag(tmat) <- 0

    # Track which rows/cols were reflected to achieve a positive manifold
    refvec <- integer(0)
    n_reflections <- 0
    max_reflections <- n * n  # Prevent infinite loop in degenerate cases

    # Reflect until all column sums are positive (qmethod::centroid logic)
    # Use the row values to update BOTH the row and the column to preserve symmetry.
    while (!all(colSums(tmat) > 0) && n_reflections < max_reflections) {
      oo <- which.min(colSums(tmat))
      vec <- tmat[oo, ] * -1
      tmat[oo, ] <- vec
      tmat[, oo] <- vec
      refvec <- c(refvec, oo)
      n_reflections <- n_reflections + 1
    }

    if (n_reflections >= max_reflections)
      warning(sprintf("Reflection loop did not converge for factor %d", f))

    # Iterative centroid computation (qmethod::centroid logic)
    rmean <- colSums(tmat) / (n - 1)
    t1 <- rmean + colSums(tmat)

    denom1 <- sqrt(sum(t1))
    if (!is.finite(denom1) || denom1 <= 0) {
      convergence_info[[f]] <- list(converged = FALSE, iterations = 0, reason = "invalid_denominator")
      warning(sprintf("Centroid extraction failed for factor %d", f))
      break
    }
    f1 <- t1 / denom1

    iter <- 0
    converged <- FALSE
    while (!all(abs(rmean - f1^2) < tolerance) && iter < max_iter) {
      iter <- iter + 1
      rmean <- f1^2
      t2 <- colSums(tmat) + f1^2
      denom2 <- sqrt(sum(t2))
      if (!is.finite(denom2) || denom2 <= 0) {
        convergence_info[[f]] <- list(converged = FALSE, iterations = iter, reason = "invalid_denominator")
        break
      }
      f1 <- t2 / denom2
    }

    if (all(abs(rmean - f1^2) < tolerance)) {
      converged <- TRUE
    }
    convergence_info[[f]] <- list(converged = converged, iterations = iter,
                                   reason = if (converged) "converged" else if (iter >= max_iter) "max_iter" else "other")

    if (!converged && iter >= max_iter)
      warning(sprintf("Centroid extraction did not converge for factor %d after %d iterations", f, max_iter))

    # Apply the stored reflections to the factor loadings
    tfac <- f1
    if (length(refvec) > 0) {
      for (idx in refvec) {
        tfac[idx] <- tfac[idx] * -1
      }
    }

    loadings[, f] <- tfac

    # Residual matrix for next factor extraction (qmethod::centroid logic)
    residual_mat <- matrix(0, nrow = n, ncol = n)
    for (row in seq_len(n)) {
      residual_mat[row, ] <- tmat[row, ] - f1[row] * f1
    }

    # Reverse reflections on residuals
    if (length(refvec) > 0) {
      for (idx in rev(refvec)) {
        residual_mat[idx, ] <- -residual_mat[idx, ]
        residual_mat[, idx] <- -residual_mat[, idx]
      }
    }

    tmat <- residual_mat
  }

  rownames(loadings) <- rownames(cor_matrix)
  colnames(loadings) <- paste0("F", seq_len(nfactors))

  attr(loadings, "convergence") <- convergence_info

  loadings
}

#' Factor Rotation
#'
#' @description
#' Rotate factor loadings to improve interpretability.
#'
#' @param extraction_result Result from qsort_extract()
#' @param method Rotation method: "varimax", "quartimax", "oblimin", "promax", or "none"
#' @param ... Additional arguments passed to rotation functions
#'
#' @return List with rotated loadings and rotation matrix
#' @export
qsort_rotate <- function(extraction_result,
                         method = c("varimax", "quartimax", "oblimin", "promax", "none"),
                         ...) {

  method <- match.arg(method)
  loadings <- extraction_result$loadings

  if (method == "none" || ncol(loadings) == 1) {
    return(list(
      loadings = loadings,
      rotation_matrix = diag(ncol(loadings)),
      method = if (ncol(loadings) == 1) "none (K=1)" else method,
      phi = NULL
    ))
  }

  # Varimax goes through stats::varimax to match qmethod/psych exactly
  rotated <- switch(method,
    varimax = {
      vr <- stats::varimax(loadings, ...)
      list(loadings = vr$loadings, Th = vr$rotmat, Phi = NULL)
    },
    quartimax = GPArotation::quartimax(loadings, ...),
    oblimin = GPArotation::oblimin(loadings, ...),
    promax = {
      # Promax is oblique rotation based on varimax
      varimax_rot <- stats::varimax(loadings)
      promax_result <- stats::promax(varimax_rot$loadings, ...)
      # Compute factor correlation matrix (Phi) for promax oblique rotation
      # Phi = (T'^-1) * T^-1 where T is the rotation matrix
      promax_phi <- NULL
      if (!is.null(promax_result$rotmat)) {
        tryCatch({
          rotmat_inv <- solve(promax_result$rotmat)
          promax_phi <- t(rotmat_inv) %*% rotmat_inv
          colnames(promax_phi) <- rownames(promax_phi) <- colnames(loadings)
        }, error = function(e) {
          warning("Could not compute factor correlation matrix for promax rotation")
        })
      }
      list(
        loadings = promax_result$loadings,
        Th = promax_result$rotmat,
        Phi = promax_phi
      )
    }
  )

  # stats::varimax returns a `loadings` object which behaves like a matrix but
  # can break tibble/pivot_longer workflows; store plain numeric matrices.
  rotated_loadings <- if (inherits(rotated$loadings, "loadings")) {
    unclass(rotated$loadings)
  } else {
    as.matrix(rotated$loadings)
  }

  rownames(rotated_loadings) <- rownames(loadings)
  colnames(rotated_loadings) <- colnames(loadings)

  # Factor correlation matrix for oblique rotations
  phi <- if (method %in% c("oblimin", "promax") && !is.null(rotated$Phi)) {
    rotated$Phi
  } else {
    NULL
  }

  list(
    loadings = rotated_loadings,
    rotation_matrix = rotated$Th,
    method = method,
    phi = phi
  )
}

#' Q-Sort Flagging
#'
#' @description
#' Flag Q-sorts that significantly load on each factor using the standard
#' Q methodology criteria from Brown (1980) as implemented in qmethod.
#'
#' @param loadings Matrix of factor loadings
#' @param nstat Number of statements in the Q-sort (required for auto flagging)
#' @param method Flagging method: "auto", "manual", or "theoretical"
#' @param threshold Manual threshold for flagging (only used if method = "theoretical")
#' @param p_level Significance level for auto-flagging (default 0.05)
#'
#' @return A logical matrix indicating flagged Q-sorts
#' @export
#'
#' @details
#' The auto-flagging method uses two criteria (both must be met):
#' 1. The loading must be significant: |loading| > z_(1-p/2)/sqrt(nstat) where p = p_level
#'    (default p_level = 0.05 gives z = 1.96)
#' 2. The squared loading must be greater than the sum of squared loadings on all
#'    other factors (i.e., the Q-sort loads more heavily on this factor than all
#'    others combined)
#'
#' This implements the standard flagging algorithm from Brown (1980) and matches
#' the qmethod package implementation (Zabala, 2014).
#'
#' @references
#' Brown, S. R. (1980). Political subjectivity. Yale University Press.
#' Zabala, A. (2014). qmethod: A package to explore human perspectives using
#' Q methodology. The R Journal, 6(2), 163-173.
qsort_flag <- function(loadings,
                       nstat = NULL,
                       method = c("auto", "manual", "theoretical"),
                       threshold = NULL,
                       p_level = 0.05,
                       majority = TRUE) {

  method <- match.arg(method)
  n_qsorts <- nrow(loadings)
  nfactors <- ncol(loadings)

  if (!is.numeric(p_level) || length(p_level) != 1 || is.na(p_level) ||
      p_level <= 0 || p_level >= 1)
    stop("p_level must be a single number in (0, 1)")

  flags <- matrix(FALSE, nrow = n_qsorts, ncol = nfactors,
                  dimnames = list(rownames(loadings), colnames(loadings)))

  if (method == "auto") {
    # Automatic flagging using Brown (1980) / qmethod criteria
    # Both conditions must be met:
    # 1. |loading| > 1.96/sqrt(nstat), significant at p < 0.05
    # 2. loading^2 > sum of squared loadings on OTHER factors

    if (is.null(nstat)) {
      stop("nstat (number of statements) is required for auto flagging")
    }

    # Significance threshold based on number of STATEMENTS (not Q-sorts)
    # This is the standard error of a zero correlation for the Q-sort length
    zcrit <- stats::qnorm(1 - p_level / 2)
    sig_threshold <- zcrit / sqrt(nstat)

    loa_sq <- loadings^2

    for (i in seq_len(n_qsorts)) {
      for (f in seq_len(nfactors)) {
        # Condition 1: Loading is significant (p < 0.05, two-tailed)
        cond_significant <- abs(loadings[i, f]) > sig_threshold

        # Condition 2 (optional): squared loading > sum of OTHER squared
        # loadings, i.e. the factor explains more variance than all others
        # combined. With majority = FALSE, flagging rests on significance
        # alone.
        sum_other_sq <- sum(loa_sq[i, ]) - loa_sq[i, f]
        cond_dominant <- !majority || loa_sq[i, f] > sum_other_sq

        # Both conditions must be met
        flags[i, f] <- cond_significant && cond_dominant
      }
    }

  } else if (method == "theoretical") {
    # Simple threshold-based flagging
    if (is.null(threshold)) {
      if (is.null(nstat)) {
        # Fallback: use a common threshold
        threshold <- 0.38
        warning("No threshold or nstat provided. Using default threshold of 0.38")
      } else {
        # Calculate threshold for significance at p < 0.05
        zcrit <- stats::qnorm(1 - p_level / 2)
        threshold <- zcrit / sqrt(nstat)
      }
    }

    flags <- abs(loadings) > threshold

    # Remove confounded (flagged on multiple factors): keep the highest only
    confounded <- rowSums(flags) > 1
    if (any(confounded)) {
      confounded_names <- rownames(loadings)[which(confounded)]
      if (is.null(confounded_names)) confounded_names <- paste0("Q-sort ", which(confounded))
      message(sprintf("%d confounded Q-sort(s) resolved by assigning to highest loading", sum(confounded)))
      for (i in which(confounded)) {
        # Keep only the highest loading
        flags[i, ] <- FALSE
        flags[i, which.max(abs(loadings[i, ]))] <- TRUE
      }
    }

  } else if (method == "manual") {
    # Manual flagging: return empty flags for the user to set
  }

  flags
}

#' Compute Factor Scores (Z-scores)
#'
#' @description
#' Compute factor scores (z-scores) for statements based on flagged Q-sorts
#' using the standard Brown (1980) weighting formula. This implementation
#' matches the qmethod package (Zabala, 2014).
#'
#' @param sorts Matrix of Q-sorts (participants x statements)
#' @param flags Flagging matrix (participants x factors)
#' @param loadings Factor loadings matrix
#' @param forced Logical; is the distribution forced?
#' @param distribution The Q-sort distribution (for computing rounded scores)
#'
#' @return Data frame of factor scores with statement info
#' @export
#'
#' @details
#' Factor scores are computed using Brown's (1980) weighting formula:
#' \deqn{w = \frac{f}{1 - f^2}}
#' where \eqn{f} is the factor loading. This gives higher weight to Q-sorts
#' with higher loadings while accounting for the portion of variance not
#' explained by the factor.
#'
#' For each statement, the weighted score is computed as:
#' \deqn{WS_i = \sum_{j \in flagged} w_j \times s_{ij}}
#' where \eqn{s_{ij}} is the score given by participant j to statement i.
#'
#' These weighted scores are then standardized to z-scores with mean 0 and SD 1.
#'
#' Rounded scores are computed using rank-based assignment to match the forced
#' distribution, following the standard Q-methodology approach.
#'
#' @references
#' Brown, S. R. (1980). Political subjectivity. Yale University Press.
#' Zabala, A. (2014). qmethod: A package to explore human perspectives using
#' Q methodology. The R Journal, 6(2), 163-173.
qsort_scores <- function(sorts, flags, loadings, forced = TRUE, distribution = NULL) {

  n_statements <- ncol(sorts)
  nfactors <- ncol(flags)
  n_participants <- nrow(sorts)

  factor_scores <- data.frame(
    statement = colnames(sorts),
    statement_num = seq_len(n_statements)
  )
  factor_weights_out <- list()

  for (f in seq_len(nfactors)) {
    fname <- colnames(flags)[f]

    # Get flagged Q-sorts for this factor
    flagged_indices <- which(flags[, f])

    if (length(flagged_indices) == 0) {
      factor_scores[[paste0(fname, "_zscore")]] <- NA
      factor_scores[[paste0(fname, "_score")]] <- NA
      factor_scores[[paste0(fname, "_rank")]] <- NA_integer_
      next
    }

    flagged_loadings <- loadings[flagged_indices, f]

    # Brown (1980) weights: w = f / (1 - f^2). Keep the sign, since a
    # negative loading is the opposite perspective and subtracts from the
    # factor. Cap |loading| at 0.9999 so the weight cannot blow up.
    safe_loadings <- sign(flagged_loadings) * pmax(pmin(abs(flagged_loadings), 0.9999), 0.0001)
    factor_weights <- safe_loadings / (1 - safe_loadings^2)
    factor_weights_out[[fname]] <- stats::setNames(
      factor_weights, rownames(sorts)[flagged_indices]
    )

    # Compute weighted scores for each statement (matching qmethod approach)
    flagged_sorts <- sorts[flagged_indices, , drop = FALSE]

    # Weight matrix: multiply each Q-sort's values by its weight
    # This is equivalent to: t(t(dataset) * weights)
    weighted_raw <- sweep(flagged_sorts, 1, factor_weights, "*")

    # Sum weighted scores for each statement
    weighted_sum <- colSums(weighted_raw)

    # Compute mean and SD for standardization (per qmethod: across statements)
    wsum_mean <- mean(weighted_sum)
    wsum_sd <- sd(weighted_sum)

    # Standardize to z-scores (mean = 0, SD = 1)
    if (wsum_sd > 0) {
      zscore <- (weighted_sum - wsum_mean) / wsum_sd
    } else {
      zscore <- rep(0, n_statements)
    }

    factor_scores[[paste0(fname, "_zscore")]] <- zscore
    factor_scores[[paste0(fname, "_rank")]] <- as.integer(
      rank(-zscore, ties.method = "min")
    )

    # Compute rounded scores using rank-based distribution assignment
    # This is the standard Q-methodology approach
    if (forced && !is.null(distribution) && length(distribution) > 0) {
      rounded_scores <- compute_rounded_scores(zscore, distribution)
      factor_scores[[paste0(fname, "_score")]] <- as.integer(rounded_scores)
    } else if (forced) {
      # Fallback: infer distribution from data if not provided
      # Use mode across ALL participants (not just first) for robustness
      # Compute distribution for each participant and take the most common
      all_dists <- lapply(seq_len(nrow(sorts)), function(i) {
        as.numeric(table(sorts[i, ]))
      })
      # Find the most common distribution pattern
      dist_strings <- sapply(all_dists, paste, collapse = ",")
      mode_dist_string <- names(sort(table(dist_strings), decreasing = TRUE))[1]
      inferred_dist <- as.numeric(strsplit(mode_dist_string, ",")[[1]])
      if (length(inferred_dist) > 0) {
        rounded_scores <- compute_rounded_scores(zscore, inferred_dist)
        factor_scores[[paste0(fname, "_score")]] <- as.integer(rounded_scores)
      }
    }
  }

  attr(factor_scores, "factor_weights") <- factor_weights_out
  return(factor_scores)
}

#' Compute Rounded Scores from Z-scores
#'
#' @description
#' Convert z-scores to rounded scores by ranking statements and assigning
#' them to positions in the forced distribution. This is the standard
#' Q-methodology approach used by PQMethod and qmethod.
#'
#' @param zscores Vector of z-scores for statements
#' @param distribution Vector specifying number of statements at each position
#'
#' @return Vector of rounded scores matching the distribution
#' @keywords internal
#'
#' @details
#' Statements are ranked by their z-scores (highest = most positive position)
#' and assigned to distribution positions. For example, with distribution
#' c(2, 3, 4, 5, 4, 3, 2), the 2 statements with highest z-scores get +3,
#' the next 3 get +2, etc.
compute_rounded_scores <- function(zscores, distribution) {

  n_statements <- length(zscores)
  n_positions <- length(distribution)

  # Validate distribution sums to number of statements
  if (sum(distribution) != n_statements) {
    # Adjust distribution to match statement count if needed
    warning("Distribution sum (", sum(distribution), ") doesn't match statement count (",
            n_statements, "). Adjusting distribution.")
    # Simple adjustment: scale proportionally
    adjusted <- round(distribution * n_statements / sum(distribution))

    # Ensure no zeros: redistribute if needed
    adjusted <- pmax(adjusted, 0)

    # Fix any rounding errors
    diff_n <- n_statements - sum(adjusted)
    if (diff_n != 0) {
      # Add/remove from middle position
      mid_pos <- ceiling(n_positions / 2)
      adjusted[mid_pos] <- adjusted[mid_pos] + diff_n
    }

    # Final check: all positive
    if (any(adjusted < 0)) {
      # Fallback: equal distribution
      adjusted <- rep(n_statements %/% n_positions, n_positions)
      remainder <- n_statements %% n_positions
      if (remainder > 0) {
        # Add remainder to middle positions
        middle_indices <- seq(ceiling((n_positions - remainder + 1) / 2),
                              length.out = remainder)
        adjusted[middle_indices] <- adjusted[middle_indices] + 1
      }
    }
    distribution <- adjusted
  }

  # Create position values (e.g., for 7 positions: -3, -2, -1, 0, 1, 2, 3)
  if (n_positions %% 2 == 1) {
    # Odd number of positions: symmetric around 0
    half <- (n_positions - 1) / 2
    position_values <- seq(-half, half)
  } else {
    # Even number of positions
    half <- n_positions / 2
    position_values <- c(seq(-half, -1), seq(1, half))
  }

  # Expand distribution to a score vector (qmethod approach).
  # Example: positions -3:3 with distribution c(2,3,4,5,4,3,2) becomes
  # -3, -3, -2, -2, -2, ..., +3, +3 (length == n_statements).
  qscores <- rep(position_values, times = distribution)

  # Rank statements by z-score (lowest z-score gets the most negative position).
  # This matches qmethod::qzscores() behavior.
  ranks <- rank(zscores)
  rounded_scores <- qscores[ranks]

  # Handle ties like qmethod: identical z-scores receive the minimum score
  # among the tied ranks (can slightly deviate from the forced distribution).
  if (length(unique(zscores)) != length(zscores)) {
    tied_idx <- which(round(ranks) != ranks)
    if (length(tied_idx) > 0) {
      tied_vals <- unique(zscores[tied_idx])
      for (g in tied_vals) {
        idx <- which(zscores == g)
        rounded_scores[idx] <- min(rounded_scores[idx])
      }
    }
  }

  return(rounded_scores)
}

#' Identify Distinguishing and Consensus Statements
#'
#' @description
#' Identify statements that distinguish between factors (distinguishing)
#' and statements that are similar across all factors (consensus) using
#' the Standard Error of Differences (SED) method. This implementation
#' matches the qmethod package classification system.
#'
#' @param factor_scores Data frame of factor scores with z-score columns
#' @param statements Character vector of statement texts (optional)
#' @param sed_matrix Standard Error of Differences matrix between factors
#' @param sig_level Significance level for distinguishing (default 0.05)
#' @param se_factors Vector of standard errors for each factor (optional)
#'
#' @return List with distinguishing statements per factor, consensus statements,
#'         category classifications, and a detailed comparison matrix
#' @export
#'
#' @details
#' Statements are classified into categories following qmethod (Zabala, 2014):
#'
#' \itemize{
#'   \item \strong{"Distinguishes all"}: The statement differs significantly
#'         (p < 0.05) from ALL other factors in ALL pairwise comparisons.
#'   \item \strong{"Distinguishes f*"}: The statement differs significantly
#'         from all other factors for factor f, BUT comparisons among the
#'         other factors are NOT significant. This indicates the statement
#'         is uniquely characteristic of factor f.
#'   \item \strong{"Consensus"}: No significant differences across any factor
#'         pairs. All factors view this statement similarly.
#'   \item \strong{""} (empty): Statement distinguishes some but not all
#'         factor pairs in a pattern not fitting the above categories.
#' }
#'
#' The significance test uses: |z_f - z_g| > SED_fg * 1.96 for p < 0.05
#'
#' @references
#' Brown, S. R. (1980). Political subjectivity. Yale University Press.
#' Zabala, A. (2014). qmethod: A package to explore human perspectives
#' using Q methodology. The R Journal, 6(2), 163-173.
qsort_distinguish <- function(factor_scores, statements = NULL,
                              sed_matrix = NULL, sig_level = 0.05,
                              se_factors = NULL,
                              criterion = c("all", "any")) {

  criterion <- match.arg(criterion)

  zscore_cols <- grep("_zscore$", names(factor_scores), value = TRUE)
  nfactors <- length(zscore_cols)
  n_statements <- nrow(factor_scores)

  if (nfactors < 2) {
    return(list(
      distinguishing = list(),
      consensus = factor_scores$statement,
      categories = character(0),
      comparison_matrix = NULL
    ))
  }

  zscores <- as.matrix(factor_scores[, zscore_cols])

  # If SED matrix not provided, compute from SE factors or use approximation
  if (is.null(sed_matrix)) {
    if (!is.null(se_factors) && length(se_factors) == nfactors) {
      # Compute SED from individual factor SEs
      sed_matrix <- matrix(NA, nrow = nfactors, ncol = nfactors)
      for (f in seq_len(nfactors)) {
        for (g in seq_len(nfactors)) {
          if (f != g) {
            sed_matrix[f, g] <- sqrt(se_factors[f]^2 + se_factors[g]^2)
          }
        }
      }
    } else {
      # Fallback: use approximate SED based on typical values
      se_approx <- 0.25  # Typical SE for factor scores
      sed_approx <- sqrt(2) * se_approx
      sed_matrix <- matrix(sed_approx, nrow = nfactors, ncol = nfactors)
      diag(sed_matrix) <- NA
      warning("SED matrix not provided. Using approximate SED = ", round(sed_approx, 3))
    }
  }

  factor_names <- gsub("_zscore$", "", zscore_cols)
  distinguishing <- vector("list", nfactors)
  names(distinguishing) <- factor_names

  # Track significance levels (* p<0.05, ** p<0.01, *** p<0.001)
  distinguishing_sig <- vector("list", nfactors)
  names(distinguishing_sig) <- factor_names

  # Create detailed comparison data frame (like qmethod's qdc output)
  # Columns: statement, zscores for each factor, differences, significance stars, category
  comparison_df <- data.frame(
    statement = factor_scores$statement,
    statement_num = factor_scores$statement_num
  )

  for (f in seq_len(nfactors)) {
    comparison_df[[paste0("zsc_", factor_names[f])]] <- zscores[, f]
  }

  # Create significance matrix for all factor pairs
  # and track differences with significance markers
  pair_names <- c()
  for (f in 1:(nfactors - 1)) {
    for (g in (f + 1):nfactors) {
      pair_names <- c(pair_names, paste0(factor_names[f], "-", factor_names[g]))
    }
  }

  comparison_df$category <- character(n_statements)

  # For each statement, perform all pairwise comparisons
  for (i in seq_len(n_statements)) {
    stmt_zscores <- zscores[i, ]

    # Matrix to store significance of all pairwise comparisons
    # TRUE = significant difference, FALSE = not significant
    sig_matrix <- matrix(FALSE, nrow = nfactors, ncol = nfactors)
    sig_level_matrix <- matrix(0, nrow = nfactors, ncol = nfactors)  # 0, 1, 2, 3 for significance levels

    for (f in seq_len(nfactors)) {
      for (g in seq_len(nfactors)) {
        if (f != g && !is.na(sed_matrix[f, g])) {
          z_diff <- abs(stmt_zscores[f] - stmt_zscores[g])
          sed_fg <- sed_matrix[f, g]

          if (z_diff > sed_fg * 3.29) {  # p < 0.001
            sig_matrix[f, g] <- TRUE
            sig_level_matrix[f, g] <- 3
          } else if (z_diff > sed_fg * 2.58) {  # p < 0.01
            sig_matrix[f, g] <- TRUE
            sig_level_matrix[f, g] <- 2
          } else if (z_diff > sed_fg * 1.96) {  # p < 0.05
            sig_matrix[f, g] <- TRUE
            sig_level_matrix[f, g] <- 1
          }
        }
      }
    }

    # Determine category following qmethod logic
    category <- ""

    # Check if ALL pairwise comparisons are significant -> "Distinguishes all"
    all_pairs_sig <- TRUE
    for (f in 1:(nfactors - 1)) {
      for (g in (f + 1):nfactors) {
        if (!sig_matrix[f, g]) {
          all_pairs_sig <- FALSE
          break
        }
      }
      if (!all_pairs_sig) break
    }

    if (all_pairs_sig) {
      category <- "Distinguishes all"
    } else {
      # Factor f differs from all others, but others don't differ among themselves
      for (f in seq_len(nfactors)) {
        # Check if factor f differs from all other factors
        f_differs_from_all <- TRUE
        for (g in seq_len(nfactors)) {
          if (f != g && !sig_matrix[f, g]) {
            f_differs_from_all <- FALSE
            break
          }
        }

        if (f_differs_from_all && nfactors > 2) {
          # Check if OTHER factors don't differ among themselves
          others_same <- TRUE
          other_factors <- setdiff(seq_len(nfactors), f)
          for (j in seq_along(other_factors)) {
            for (k in seq_along(other_factors)) {
              if (j < k) {
                fj <- other_factors[j]
                fk <- other_factors[k]
                if (sig_matrix[fj, fk]) {
                  others_same <- FALSE
                  break
                }
              }
            }
            if (!others_same) break
          }

          if (others_same) {
            category <- paste0("Distinguishes ", factor_names[f], "*")
            break
          }
        } else if (f_differs_from_all && nfactors == 2) {
          # With only 2 factors, if they differ, it distinguishes both
          category <- paste0("Distinguishes ", factor_names[f], "*")
          break
        }
      }

      # Check for consensus: NO significant differences
      any_sig <- any(sig_matrix[upper.tri(sig_matrix)])
      if (!any_sig) {
        category <- "Consensus"
      }
    }

    comparison_df$category[i] <- category

    # Add to distinguishing lists for each factor. Under "all" (the
    # standard Q-methodology convention) a statement distinguishes factor f only
    # when it differs significantly from EVERY other factor, and its star
    # is the weakest of those comparisons; under "any" one significant
    # pair suffices and the star is the strongest.
    for (f in seq_len(nfactors)) {
      others <- setdiff(seq_len(nfactors), f)
      hit <- if (criterion == "all") {
        all(sig_matrix[f, others])
      } else {
        any(sig_matrix[f, others])
      }
      if (hit) {
        distinguishing[[f]] <- c(distinguishing[[f]], factor_scores$statement[i])
        lvl <- if (criterion == "all") {
          min(sig_level_matrix[f, others])
        } else {
          max(sig_level_matrix[f, others])
        }
        sig_star <- switch(as.character(lvl),
                          "3" = "***",
                          "2" = "**",
                          "1" = "*",
                          "")
        distinguishing_sig[[f]] <- c(distinguishing_sig[[f]], sig_star)
      }
    }
  }

  # Identify consensus statements
  consensus <- comparison_df$statement[comparison_df$category == "Consensus"]

  # Build detailed pairwise difference columns with significance markers
  diff_df <- data.frame(statement = factor_scores$statement)
  for (f in 1:(nfactors - 1)) {
    for (g in (f + 1):nfactors) {
      col_name <- paste0("diff_", factor_names[f], "_", factor_names[g])
      diffs <- zscores[, f] - zscores[, g]

      sig_markers <- sapply(seq_len(n_statements), function(i) {
        z_diff <- abs(diffs[i])
        sed_fg <- sed_matrix[f, g]
        if (is.na(sed_fg)) return("")
        if (z_diff > sed_fg * 3.29) return("***")
        if (z_diff > sed_fg * 2.58) return("**")
        if (z_diff > sed_fg * 1.96) return("*")
        return("")
      })

      diff_df[[col_name]] <- round(diffs, 3)
      diff_df[[paste0(col_name, "_sig")]] <- sig_markers
    }
  }

  # Exact two-sided p per statement and pair, BH-adjusted across the whole
  # family; dual-level consensus; ranking variance and z ranges
  n_pairs <- length(pair_names)
  p_raw_mat <- matrix(NA_real_, nrow = n_statements, ncol = n_pairs,
                      dimnames = list(factor_scores$statement, pair_names))
  max_level <- integer(n_statements)
  pj <- 0
  for (f in 1:(nfactors - 1)) {
    for (g in (f + 1):nfactors) {
      pj <- pj + 1
      sed_fg <- sed_matrix[f, g]
      if (!is.na(sed_fg) && sed_fg > 0) {
        zdiff <- abs(zscores[, f] - zscores[, g])
        p_raw_mat[, pj] <- 2 * (1 - stats::pnorm(zdiff / sed_fg))
        lvl <- ifelse(zdiff > sed_fg * 3.29, 3L,
               ifelse(zdiff > sed_fg * 2.58, 2L,
               ifelse(zdiff > sed_fg * 1.96, 1L, 0L)))
        max_level <- pmax(max_level, lvl)
      }
    }
  }
  p_adj_mat <- p_raw_mat
  p_adj_mat[] <- stats::p.adjust(as.vector(p_raw_mat), method = "BH")

  consensus_level <- stats::setNames(
    ifelse(max_level == 0L, "strict",
           ifelse(max_level == 1L, "broad", "")),
    factor_scores$statement
  )
  ranking_variance <- stats::setNames(
    apply(zscores, 1, stats::var), factor_scores$statement
  )
  z_range <- cbind(min = apply(zscores, 1, min),
                   max = apply(zscores, 1, max))
  rownames(z_range) <- factor_scores$statement

  list(
    distinguishing = distinguishing,
    distinguishing_significance = distinguishing_sig,
    consensus = consensus,
    consensus_level = consensus_level,
    categories = comparison_df[, c("statement", "statement_num", "category")],
    comparison_df = comparison_df,
    pairwise_differences = diff_df,
    pairwise_p = p_raw_mat,
    pairwise_p_adj = p_adj_mat,
    ranking_variance = ranking_variance,
    z_range = z_range,
    criterion = criterion,
    sed_matrix = sed_matrix
  )
}

#' Compute Factor Characteristics
#'
#' @description
#' Compute factor characteristics including composite reliability, standard
#' errors, and standard error of differences (SED) between factors.
#'
#' @param sorts Original Q-sort matrix
#' @param flags Flagging matrix
#' @param loadings Factor loadings
#' @param extraction Extraction results
#' @param factor_scores Data frame of factor z-scores (optional, for SE calculation)
#' @param av_rel_coef Average reliability coefficient for individual Q-sorts.
#'   Default is 0.8, the standard assumption in Q methodology (Brown, 1980).
#'   This value is used in the Spearman-Brown formula for composite reliability.
#'
#' @return List with factor characteristics data frame and SED matrix
#' @keywords internal
#'
#' @details
#' Composite reliability is computed using the Spearman-Brown formula:
#' \deqn{r_{xx} = \frac{av\_rel\_coef \times p}{1 + (p-1) \times av\_rel\_coef}}
#' where p is the number of flagged Q-sorts and av_rel_coef is the average
#' reliability coefficient assumed for individual Q-sorts (default 0.8).
#'
#' Standard error of factor scores:
#' \deqn{SE_f = SD(z_f) \times \sqrt{1 - r_{xx}}}
#'
#' Standard error of differences (SED) between factors:
#' \deqn{SED_{fg} = \sqrt{SE_f^2 + SE_g^2}}
#'
#' @references
#' Brown, S. R. (1980). Political subjectivity. Yale University Press.
#' Zabala, A. (2014). qmethod: A package to explore human perspectives using
#' Q methodology. The R Journal, 6(2), 163-173.
compute_factor_characteristics <- function(sorts, flags, loadings, extraction,
                                           factor_scores = NULL,
                                           av_rel_coef = 0.8) {

  nfactors <- ncol(flags)
  n_participants <- nrow(sorts)

  if (!is.numeric(av_rel_coef) || length(av_rel_coef) != 1 ||
      av_rel_coef <= 0 || av_rel_coef >= 1) {
    warning("av_rel_coef must be between 0 and 1; using default 0.8")
    av_rel_coef <- 0.8
  }

  characteristics <- data.frame(
    factor = colnames(flags),
    n_flagged = colSums(flags),
    eigenvalue = extraction$eigenvalues,
    variance_pct = extraction$variance_explained * 100,
    cumulative_var_pct = extraction$cumulative_variance * 100
  )

  # Composite reliability using Spearman-Brown formula
  # r_xx = (av_rel_coef * p) / (1 + (p - 1) * av_rel_coef)
  # where p = number of flagged Q-sorts
  characteristics$composite_reliability <- sapply(seq_len(nfactors), function(f) {
    n_flagged <- sum(flags[, f])
    if (n_flagged > 0) {
      (av_rel_coef * n_flagged) / (1 + (n_flagged - 1) * av_rel_coef)
    } else {
      NA
    }
  })

  # Average loading for flagged Q-sorts
  characteristics$avg_loading <- sapply(seq_len(nfactors), function(f) {
    flagged <- which(flags[, f])
    if (length(flagged) > 0) mean(abs(loadings[flagged, f])) else NA
  })

  # Standard error of factor scores
  # SE_f = SD(z_f) * sqrt(1 - reliability)
  if (!is.null(factor_scores)) {
    zscore_cols <- grep("_zscore$", names(factor_scores), value = TRUE)
    if (length(zscore_cols) == nfactors) {
      zscores <- as.matrix(factor_scores[, zscore_cols])

      characteristics$se_factor <- sapply(seq_len(nfactors), function(f) {
        rel <- characteristics$composite_reliability[f]
        if (!is.na(rel) && rel < 1) {
          sd(zscores[, f], na.rm = TRUE) * sqrt(1 - rel)
        } else {
          NA
        }
      })
    } else {
      # Fallback: approximate SE based on number of flagged
      characteristics$se_factor <- sapply(seq_len(nfactors), function(f) {
        rel <- characteristics$composite_reliability[f]
        if (!is.na(rel) && rel < 1) {
          1 * sqrt(1 - rel)  # Assume SD(z) = 1
        } else {
          NA
        }
      })
    }
  } else {
    # Approximate SE when z-scores not available
    characteristics$se_factor <- sapply(seq_len(nfactors), function(f) {
      rel <- characteristics$composite_reliability[f]
      if (!is.na(rel) && rel < 1) {
        1 * sqrt(1 - rel)
      } else {
        NA
      }
    })
  }

  # Standard Error of Differences (SED) matrix between factors
  # SED_fg = sqrt(SE_f^2 + SE_g^2)
  sed_matrix <- matrix(NA, nrow = nfactors, ncol = nfactors,
                       dimnames = list(colnames(flags), colnames(flags)))

  for (f in seq_len(nfactors)) {
    for (g in seq_len(nfactors)) {
      if (f != g) {
        se_f <- characteristics$se_factor[f]
        se_g <- characteristics$se_factor[g]
        if (!is.na(se_f) && !is.na(se_g)) {
          sed_matrix[f, g] <- sqrt(se_f^2 + se_g^2)
        }
      }
    }
  }

  return(list(
    characteristics = characteristics,
    sed_matrix = sed_matrix
  ))
}

# JUDGMENTAL (MANUAL) ROTATION

#' Apply Judgmental (Manual) Rotation
#'
#' @description
#' Apply a manual rotation to factor loadings by a specified angle,
#' so factors can be rotated on theoretical grounds rather than purely
#' mathematical criteria.
#'
#' @param loadings A matrix of factor loadings
#' @param factors Vector of two factor indices to rotate (e.g., c(1, 2))
#' @param angle Rotation angle in degrees (positive = counterclockwise)
#'
#' @return A matrix of rotated factor loadings
#' @export
#'
#' @examples
#' \dontrun{
#' # Rotate factors 1 and 2 by 15 degrees
#' new_loadings <- qsort_rotate_manual(loadings, c(1, 2), 15)
#' }
qsort_rotate_manual <- function(loadings, factors = c(1, 2), angle = 0) {

  if (length(factors) != 2) {
    stop("Must specify exactly 2 factors to rotate")
  }

  if (max(factors) > ncol(loadings)) {
    stop("Factor indices exceed number of available factors")
  }

  theta <- angle * pi / 180

  cos_t <- cos(theta)
  sin_t <- sin(theta)

  rotation_matrix <- matrix(c(cos_t, sin_t, -sin_t, cos_t), nrow = 2, ncol = 2)

  # Extract the two factors to rotate
  f1 <- factors[1]
  f2 <- factors[2]

  rotated <- loadings
  two_factors <- loadings[, c(f1, f2)]
  rotated_two <- two_factors %*% rotation_matrix

  rotated[, f1] <- rotated_two[, 1]
  rotated[, f2] <- rotated_two[, 2]

  return(rotated)
}

#' Re-analyze with Manual Rotation
#'
#' @description
#' Take existing results and apply manual rotation, then recompute
#' flagging, scores, and other derived statistics.
#'
#' @param results A QsortResults object
#' @param factors Vector of two factor indices to rotate
#' @param angle Rotation angle in degrees
#'
#' @return A new QsortResults object with rotated solution
#' @export
qsort_reanalyze_rotated <- function(results, factors = c(1, 2), angle = 0) {

  if (!inherits(results, "QsortResults")) {
    stop("results must be a QsortResults object")
  }
  old_loadings <- results@rotation$loadings
  new_loadings <- qsort_rotate_manual(old_loadings, factors, angle)

  new_rotation <- results@rotation
  new_rotation$loadings <- new_loadings
  new_rotation$method <- paste0(results@method_details$rotation, "+manual(", angle, "deg)")

  # Re-flag with new loadings, honoring the run's flagging settings
  nstat <- ncol(results@data@sorts)
  new_flagging <- qsort_flag(
    new_loadings, nstat = nstat, method = "auto",
    p_level = results@method_details$flag_p_level %||% 0.05,
    majority = results@method_details$flag_majority %||% TRUE
  )

  # Recompute factor scores with new flagging
  sorts <- results@data@sorts
  distribution <- results@data@distribution
  forced <- results@method_details$forced %||% TRUE
  new_scores <- qsort_scores(sorts, new_flagging, new_loadings, forced = forced,
                             distribution = distribution)

  # Recompute factor characteristics
  new_characteristics <- compute_factor_characteristics(
    sorts, new_flagging, new_loadings, results@extraction
  )

  # Recompute distinguishing and consensus statements
  statements <- results@data@statements
  if (is.null(statements) || length(statements) == 0) {
    statements <- colnames(sorts)
  }
  se_factors <- new_characteristics$characteristics$se_factor
  sed_matrix <- new_characteristics$sed_matrix

  distinguish_result <- qsort_distinguish(
    factor_scores = new_scores,
    statements = statements,
    sed_matrix = sed_matrix,
    se_factors = se_factors,
    criterion = results@method_details$distinction$criterion %||% "all"
  )
  new_distinguishing <- distinguish_result$distinguishing
  new_consensus <- distinguish_result$consensus

  new_method_details <- results@method_details
  new_method_details$rotation <- new_rotation$method
  new_method_details$sed_matrix <- sed_matrix
  new_method_details$se_factors <- se_factors
  new_method_details$distinguishing_significance <-
    distinguish_result$distinguishing_significance
  new_method_details$comparison_details <- distinguish_result$comparison_df
  new_method_details$distinction <- distinguish_result

  # Create new results object with fully recomputed values
 new(
    "QsortResults",
    data = results@data,
    correlation = results@correlation,
    extraction = results@extraction,
    rotation = new_rotation,
    flagging = new_flagging,
    factor_scores = new_scores,
    factor_characteristics = new_characteristics,
    distinguishing = new_distinguishing,
    consensus = new_consensus,
    n_factors = results@n_factors,
    method_details = new_method_details,
    call = results@call
  )
}

# Bipolar Factor Detection

#' Detect Bipolar Factors
#'
#' @description
#' Analyze factor loadings to detect potential bipolar factors - factors
#' where some participants load significantly positive and others load
#' significantly negative, suggesting two opposing viewpoints within
#' a single factor.
#'
#' @param results A QsortResults object or a loadings matrix
#' @param threshold Minimum absolute loading to be considered significant
#'   (default: 0.3 or calculated from nstat)
#' @param min_negative Minimum number of significant negative loaders
#'   to flag as bipolar (default: 2)
#'
#' @return A data frame with bipolar analysis for each factor
#' @export
#'
#' @examples
#' \dontrun{
#' bipolar_info <- detect_bipolar(results)
#' print(bipolar_info)
#' }
detect_bipolar <- function(results, threshold = NULL, min_negative = 2) {

  if (inherits(results, "QsortResults")) {
    loadings <- results@rotation$loadings
    nstat <- ncol(results@data@sorts)
  } else if (is.matrix(results)) {
    loadings <- results
    nstat <- NULL
  } else {
    stop("results must be QsortResults or a loadings matrix")
  }

  if (is.null(threshold)) {
    if (!is.null(nstat)) {
      threshold <- 1.96 / sqrt(nstat)
    } else {
      threshold <- 0.3
    }
  }

  nfactors <- ncol(loadings)
  bipolar_info <- data.frame(
    factor = paste0("F", 1:nfactors),
    n_positive = integer(nfactors),
    n_negative = integer(nfactors),
    is_bipolar = logical(nfactors),
    positive_loaders = character(nfactors),
    negative_loaders = character(nfactors),
    bipolar_ratio = numeric(nfactors),
    recommendation = character(nfactors),
    stringsAsFactors = FALSE
  )

  for (f in seq_len(nfactors)) {
    pos_mask <- loadings[, f] > threshold
    neg_mask <- loadings[, f] < -threshold

    n_pos <- sum(pos_mask)
    n_neg <- sum(neg_mask)

    pos_names <- rownames(loadings)[pos_mask]
    neg_names <- rownames(loadings)[neg_mask]

    # Is bipolar if significant negative loaders exist
    is_bipolar <- n_neg >= min_negative

    # Ratio of negative to positive (higher = more bipolar)
    ratio <- if (n_pos > 0) n_neg / n_pos else if (n_neg > 0) Inf else 0

    # Recommendation
    if (is_bipolar) {
      if (ratio > 0.5) {
        rec <- "Consider splitting into two factors"
      } else {
        rec <- "Minor bipolar tendency - review negative loaders"
      }
    } else {
      rec <- "Unipolar factor"
    }

    bipolar_info[f, ] <- list(
      factor = paste0("F", f),
      n_positive = n_pos,
      n_negative = n_neg,
      is_bipolar = is_bipolar,
      positive_loaders = paste(pos_names, collapse = ", "),
      negative_loaders = paste(neg_names, collapse = ", "),
      bipolar_ratio = round(ratio, 3),
      recommendation = rec
    )
  }

  class(bipolar_info) <- c("bipolar_analysis", "data.frame")
  return(bipolar_info)
}

# Crib Sheet Generation

#' Generate Factor Crib Sheet
#'
#' @description
#' Generate an interpretive crib sheet for each factor following the
#' structured approach recommended in Q methodology. The crib sheet
#' highlights the most characteristic statements for each factor to
#' aid in qualitative interpretation.
#'
#' @param results A QsortResults object
#' @param factor Factor number to generate crib sheet for (default: all)
#' @param factor_num Alternative parameter for factor number (deprecated, use factor)
#' @param n_extreme Number of extreme statements to include (default: 5)
#' @param threshold Z-score difference threshold for comparative analysis (default: 0.5)
#'
#' @return A list of crib sheets, one per factor
#' @export
#'
#' @examples
#' \dontrun{
#' crib <- generate_crib_sheet(results)
#' print(crib$F1)
#' }
generate_crib_sheet <- function(results, factor = NULL, factor_num = NULL, n_extreme = 5, threshold = 0.5) {

  if (!inherits(results, "QsortResults"))
    stop("results must be a QsortResults object")

  if (!is.null(factor_num) && is.null(factor)) {
    factor <- factor_num
  }

  scores <- results@factor_scores
  statements <- results@data@statements
  dist_list <- results@distinguishing
  consensus_list <- results@consensus
  n_factors <- results@n_factors

  # Get all z-score columns for cross-factor comparisons
  all_zscore_cols <- grep("_zscore$", names(scores), value = TRUE)

  # Determine which factors to process
  single_factor_requested <- !is.null(factor)
  if (is.null(factor)) {
    factors <- seq_len(n_factors)
  } else {
    factors <- factor
  }

  crib_sheets <- list()

  for (f in factors) {
    factor_name <- paste0("F", f)
    zscore_col <- paste0(factor_name, "_zscore")
    score_col <- paste0(factor_name, "_score")

    if (!zscore_col %in% names(scores)) next

    if (!"statement_num" %in% names(scores)) {
      scores$statement_num <- seq_len(nrow(scores))
    }

    # Sort by z-score
    sorted_scores <- scores[order(-scores[[zscore_col]]), ]

    # Get extreme statements (use actual count available)
    n_stmt <- nrow(sorted_scores)
    actual_n_extreme <- min(n_extreme, n_stmt)
    top_n <- head(sorted_scores, actual_n_extreme)
    bottom_n <- tail(sorted_scores, actual_n_extreme)

    dist_stmts <- if (f <= length(dist_list)) dist_list[[f]] else character(0)

    # Get target z-scores and scores for comparative analysis
    target_zscores <- scores[[zscore_col]]
    target_scores <- if (score_col %in% names(scores)) {
      scores[[score_col]]
    } else {
      round(target_zscores)
    }

    # Watts & Stenner (2012): compare INTEGER factor array scores, not z-scores.
    # "Ranked higher than all other factors" = this factor's integer rank is
    # strictly greater than every other factor's integer rank for that statement.
    all_score_cols <- grep("_score$", names(scores), value = TRUE)
    other_score_cols <- setdiff(all_score_cols, score_col)
    other_zscore_cols <- setdiff(all_zscore_cols, zscore_col)

    higher_than_all <- rep(TRUE, n_stmt)
    for (other_col in other_score_cols) {
      other_scores <- scores[[other_col]]
      higher_than_all <- higher_than_all & (target_scores > other_scores)
    }

    lower_than_all <- rep(TRUE, n_stmt)
    for (other_col in other_score_cols) {
      other_scores <- scores[[other_col]]
      lower_than_all <- lower_than_all & (target_scores < other_scores)
    }

    # Helper: get full statement text (no truncation)
    get_stmt <- function(i) {
      if (i >= 1 && i <= length(statements)) statements[i] else paste0("Statement ", i)
    }

    # Helper: determine D/C marker for a statement
    get_marker <- function(stmt_num) {
      stmt_text <- get_stmt(stmt_num)
      is_dist <- stmt_text %in% dist_stmts
      is_cons <- stmt_text %in% consensus_list
      if (is_dist) "D" else if (is_cons) "C" else ""
    }

    # Helper: compute differential (gap in integer ranks to next-closest factor)
    get_diff <- function(stmt_idx, direction = "higher") {
      my_score <- target_scores[stmt_idx]
      other_ss <- sapply(other_score_cols, function(col) scores[[col]][stmt_idx])
      if (direction == "higher") {
        best_other <- max(other_ss, na.rm = TRUE)
        as.integer(my_score - best_other)
      } else {
        best_other <- min(other_ss, na.rm = TRUE)
        as.integer(best_other - my_score)
      }
    }

    # Helper: build enriched data.frame for a set of statement indices
    build_section_df <- function(indices, include_diff = FALSE, diff_direction = "higher") {
      if (length(indices) == 0) {
        return(data.frame(
          statement_num = integer(0), statement = character(0),
          zscore = numeric(0), score = integer(0), marker = character(0),
          distinguishing = logical(0),
          stringsAsFactors = FALSE
        ))
      }
      df <- data.frame(
        statement_num = indices,
        statement = sapply(indices, get_stmt),
        zscore = round(target_zscores[indices], 2),
        score = target_scores[indices],
        marker = sapply(indices, get_marker),
        distinguishing = sapply(indices, function(i) get_stmt(i) %in% dist_stmts),
        stringsAsFactors = FALSE
      )
      if (include_diff && length(other_zscore_cols) > 0) {
        df$diff <- sapply(indices, get_diff, direction = diff_direction)
      }
      for (of in seq_len(n_factors)) {
        if (of == f) next
        of_score_col <- paste0("F", of, "_score")
        if (of_score_col %in% names(scores)) {
          df[[paste0("F", of, "_score")]] <- scores[[of_score_col]][indices]
        }
      }
      df
    }

    top_idx <- top_n$statement_num
    bottom_idx <- bottom_n$statement_num
    higher_idx <- which(higher_than_all)
    lower_idx <- which(lower_than_all)

    crib <- list(
      factor = factor_name,
      n_flagged = results@factor_characteristics$characteristics$n_flagged[f],
      variance = round(results@factor_characteristics$characteristics$variance_pct[f], 2),
      reliability = round(results@factor_characteristics$characteristics$composite_reliability[f], 3),

      # Section 1: Highest ranked
      most_agree = build_section_df(top_idx),

      # Section 2: Ranked higher than all other factors
      higher_than_all = {
        df <- build_section_df(higher_idx, include_diff = TRUE, diff_direction = "higher")
        if (nrow(df) > 0) df[order(-df$zscore), ] else df
      },

      # Section 3: Ranked lower than all other factors
      lower_than_all = {
        df <- build_section_df(lower_idx, include_diff = TRUE, diff_direction = "lower")
        if (nrow(df) > 0) df[order(df$zscore), ] else df
      },

      # Section 4: Lowest ranked
      most_disagree = build_section_df(bottom_idx),

      # Legacy: distinguishing list
      distinguishing = if (length(dist_stmts) > 0) {
        dist_idx <- which(statements %in% dist_stmts)
        dist_data <- sorted_scores[sorted_scores$statement_num %in% dist_idx, ]
        if (nrow(dist_data) > 0) {
          build_section_df(dist_data$statement_num)
        } else NULL
      } else NULL,

      # Legacy: consensus list
      consensus = if (length(consensus_list) > 0) {
        cons_idx <- which(statements %in% consensus_list)
        cons_data <- sorted_scores[sorted_scores$statement_num %in% cons_idx, ]
        if (nrow(cons_data) > 0) {
          build_section_df(cons_data$statement_num)
        } else NULL
      } else NULL,

      # Narrative template
      narrative_template = paste0(
        "Factor ", f, " (", results@factor_characteristics$characteristics$n_flagged[f],
        " participants, ", round(results@factor_characteristics$characteristics$variance_pct[f], 1),
        "% variance) is characterized by...\n\n",
        "People loading on this factor MOST AGREE that:\n",
        paste(paste0("  - ", sapply(top_n$statement_num, get_stmt)), collapse = "\n"),
        "\n\nPeople loading on this factor MOST DISAGREE that:\n",
        paste(paste0("  - ", sapply(bottom_n$statement_num, get_stmt)), collapse = "\n")
      )
    )

    class(crib) <- c("qsort_crib_sheet", "list")
    crib_sheets[[factor_name]] <- crib
  }

  # A single requested factor returns its crib sheet directly
  if (single_factor_requested && length(crib_sheets) == 1) {
    return(crib_sheets[[1]])
  }

  class(crib_sheets) <- c("qsort_crib_sheets", "list")
  return(crib_sheets)
}

# Second-Order Factor Analysis

# Manual Rotation

# Recalculation after Manual Flag Changes

#' Recalculate Results After Manual Flagging
#'
#' @description
#' Recalculates factor scores, standard errors, distinguishing statements,
#' and consensus statements after manual flag changes. This is essential
#' for proper Q-methodology workflow where researchers may adjust automatic
#' flagging based on theoretical considerations.
#'
#' @param results A QsortResults object with updated flagging
#' @param qdata The original QsortData object
#'
#' @return Updated QsortResults object
#' @export
recalculate_after_flagging <- function(results, qdata) {

  loadings <- results@rotation$loadings
  flags <- results@flagging
  sorts <- as.matrix(qdata@sorts)
  n_factors <- ncol(loadings)
  n_statements <- ncol(sorts)
  n_participants <- nrow(sorts)

  # Recalculate factor scores through qsort_scores, which carries the
  # statement column, rank columns, and the factor_weights attribute the
  # rest of the app depends on
  factor_scores <- qsort_scores(
    sorts, flags, loadings,
    forced = results@method_details$forced %||% TRUE,
    distribution = qdata@distribution
  )

  # Recalculate standard errors of factor scores
  se_scores <- compute_factor_score_se(sorts, loadings, flags)

  # Combine scores and SE, dropping the duplicate statement_num column and
  # keeping the weights attribute cbind would discard
  fw <- attr(factor_scores, "factor_weights")
  se_extra <- se_scores[, setdiff(names(se_scores), names(factor_scores)),
                        drop = FALSE]
  results@factor_scores <- cbind(factor_scores, se_extra)
  attr(results@factor_scores, "factor_weights") <- fw

  # Recalculate factor characteristics
  results@factor_characteristics <- compute_factor_characteristics(
    sorts, flags, loadings, results@extraction, factor_scores = factor_scores
  )

  # Recalculate distinguishing and consensus statements with the full
  # engine, keeping the stored distinction current for downstream readers
  dist_full <- qsort_distinguish(
    results@factor_scores,
    statements = qdata@statements,
    sed_matrix = results@factor_characteristics$sed_matrix,
    se_factors = results@factor_characteristics$characteristics$se_factor,
    criterion = results@method_details$distinction$criterion %||% "all"
  )
  results@distinguishing <- dist_full$distinguishing
  results@consensus <- dist_full$consensus
  results@method_details$sed_matrix <- results@factor_characteristics$sed_matrix
  results@method_details$distinguishing_significance <-
    dist_full$distinguishing_significance
  results@method_details$comparison_details <- dist_full$comparison_df
  results@method_details$distinction <- dist_full

  return(results)
}

#' Compute Standard Errors of Factor Scores
#'
#' @description
#' Calculate standard errors for factor scores following Brown (1980) and
#' the PQMethod methodology. The SE depends on:
#' 1. Number of defining sorts

#' 2. Factor loadings of defining sorts
#' 3. Composite reliability of the factor
#'
#' Formula: SE = sd_zscore * sqrt(1 - reliability)
#'
#' @param sorts Matrix of Q-sort data
#' @param loadings Factor loading matrix
#' @param flags Flagging matrix
#'
#' @return Data frame with SE for each factor's scores
#' @keywords internal
compute_factor_score_se <- function(sorts, loadings, flags) {

  n_factors <- ncol(loadings)
  n_statements <- ncol(sorts)
  results <- data.frame(statement_num = seq_len(n_statements))

  for (f in seq_len(n_factors)) {
    flagged <- which(flags[, f])

    if (length(flagged) < 2) {
      # Not enough flagged participants for SE calculation
      results[[paste0("F", f, "_se")]] <- rep(NA_real_, n_statements)
      next
    }

    # Compute composite reliability for this factor
    # rxx = sum(loadings^2) / (sum(loadings^2) + sum(1-loadings^2)/n)
    f_loadings <- loadings[flagged, f]
    sum_h2 <- sum(f_loadings^2)
    sum_error <- sum(1 - f_loadings^2)
    reliability <- sum_h2 / (sum_h2 + sum_error / length(flagged))

    # Standard error of z-scores
    # SE = 1 * sqrt(1 - reliability) for standardized scores
    se_factor <- sqrt(1 - reliability)

    # SE is the same for all statements within a factor
    # (this is standard Q-method practice)
    results[[paste0("F", f, "_se")]] <- rep(se_factor, n_statements)
  }

  return(results)
}

#' Split Bipolar Factor
#'
#' @description
#' Split a bipolar factor into two unipolar factors by reflecting
#' the negative loadings and recomputing.
#'
#' @param results A QsortResults object
#' @param factor_num Which factor to split
#'
#' @return Updated QsortResults with additional factor
#' @export
split_bipolar_factor <- function(results, factor_num) {
  loadings <- results@rotation$loadings
  nstat <- ncol(results@data@sorts)
  sig_threshold <- 1.96 / sqrt(nstat)

  bipolar_loadings <- loadings[, factor_num]

  # Identify negative loaders
  negative_loaders <- bipolar_loadings < -sig_threshold

  if (sum(negative_loaders) < 2) return(results)

  new_loadings <- cbind(loadings, 0)
  colnames(new_loadings) <- c(colnames(loadings), paste0("F", ncol(new_loadings)))
  new_loadings[negative_loaders, factor_num] <- 0
  new_loadings[negative_loaders, ncol(new_loadings)] <- abs(bipolar_loadings[negative_loaders])
  new_flagging <- qsort_flag(new_loadings, nstat = nstat, method = "auto")

  # Recompute scores
  new_scores <- qsort_scores(
    results@data@sorts,
    new_flagging,
    new_loadings,
    forced = results@method_details$forced %||% TRUE,
    distribution = results@data@distribution
  )

  new_rotation <- results@rotation
  new_rotation$loadings <- new_loadings
  new_n_factors <- ncol(new_loadings)

  new_results <- results
  new_results@rotation <- new_rotation
  new_results@flagging <- new_flagging
  new_results@factor_scores <- new_scores
  new_results@n_factors <- new_n_factors

  return(new_results)
}

# Factor Array Visualization

#' Generate Factor Array
#'
#' @description
#' Generate the factor array (model Q-sort) for a factor. This shows
#' how the "ideal" participant loading purely on this factor would
#' sort the statements.
#'
#' @param results A QsortResults object
#' @param factor_num Which factor to generate array for
#'
#' @return Data frame with statement positions in the factor array
#' @export
#'
#' @examples
#' \dontrun{
#' array <- generate_factor_array(results, factor_num = 1)
#' print(array)
#' }
generate_factor_array <- function(results, factor_num = 1) {
  if (factor_num > results@n_factors)
    stop("Factor number exceeds number of extracted factors")

  distribution <- results@data@distribution
  if (is.null(distribution) || length(distribution) == 0) {
    # Default quasi-normal distribution
    n_statements <- nrow(results@factor_scores)
    distribution <- qsort_default_distribution(n_statements)
  }

  scores <- results@factor_scores
  zscore_col <- paste0("F", factor_num, "_zscore")
  score_col <- paste0("F", factor_num, "_score")

  zscores <- scores[[zscore_col]]
  rounded_scores <- if (score_col %in% names(scores)) {
    scores[[score_col]]
  } else {
    compute_rounded_scores(zscores, distribution)
  }

  statement_nums <- if ("statement_num" %in% names(scores)) {
    scores$statement_num
  } else {
    seq_len(length(zscores))
  }

  statements <- if (!is.null(results@data@statements)) {
    results@data@statements
  } else if ("statement" %in% names(scores)) {
    scores$statement
  } else {
    paste("Statement", seq_len(length(zscores)))
  }

  # Use 'score' column name for compatibility with visualize page
  array_df <- data.frame(
    statement_num = statement_nums,
    statement = statements,
    zscore = round(zscores, 2),
    score = as.integer(rounded_scores),
    stringsAsFactors = FALSE
  )

  # Sort by score (descending) then by z-score within score
  array_df <- array_df[order(-array_df$score, -array_df$zscore), ]

  # Return as list with array and distribution for compatibility with visualize page
  result <- list(
    array = array_df,
    distribution = distribution,
    factor = factor_num,
    n_statements = nrow(array_df)
  )

  class(result) <- c("QsortFactorArray", "list")
  return(result)
}

#' Default Q-Sort Distribution
#'
#' @description
#' Generate a default quasi-normal distribution for a given number of statements.
#'
#' @param n_statements Number of statements
#'
#' @return Vector of counts per position (from most negative to most positive).
#'   For example, c(1, 2, 3, 2, 1) means 1 statement at position -2,
#'   2 at position -1, 3 at position 0, 2 at position 1, 1 at position 2.
#' @keywords internal
qsort_default_distribution <- function(n_statements) {
  # Return counts-per-position format for compute_rounded_scores()
  # Common quasi-normal distributions by statement count

  if (n_statements <= 9) {
    # 9 statements: positions -2 to +2, distribution 1-2-3-2-1
    dist <- c(1, 2, 3, 2, 1)
  } else if (n_statements <= 13) {
    # 11-13 statements: positions -3 to +3, distribution 1-2-2-3-2-2-1
    dist <- c(1, 2, 2, 3, 2, 2, 1)
  } else if (n_statements <= 18) {
    # 16-18 statements: positions -3 to +3, distribution 1-2-3-4-3-2-1
    dist <- c(1, 2, 3, 4, 3, 2, 1)
  } else if (n_statements <= 25) {
    # 23-25 statements: positions -4 to +4, distribution 1-2-3-3-5-3-3-2-1
    dist <- c(1, 2, 3, 3, 5, 3, 3, 2, 1)
  } else if (n_statements <= 36) {
    # 33-36 statements: positions -4 to +4, distribution 2-3-4-5-6-5-4-3-2
    dist <- c(2, 3, 4, 5, 6, 5, 4, 3, 2)
  } else if (n_statements <= 48) {
    # ~48 statements: positions -5 to +5, distribution 2-3-4-5-6-8-6-5-4-3-2
    dist <- c(2, 3, 4, 5, 6, 8, 6, 5, 4, 3, 2)
  } else {
    # For larger sets, create symmetric quasi-normal counts
    # Target ~9-11 positions with quasi-normal shape
    n_positions <- min(11, max(7, ceiling(sqrt(n_statements))))
    if (n_positions %% 2 == 0) n_positions <- n_positions + 1  # Ensure odd

    # Create quasi-normal shape
    half <- (n_positions - 1) / 2
    # Build from center out with decreasing counts
    center <- n_statements %/% n_positions + 2
    dist <- numeric(n_positions)
    dist[half + 1] <- center  # Center position

    for (i in seq_len(half)) {
      count <- max(1, center - i)
      dist[half + 1 - i] <- count
      dist[half + 1 + i] <- count
    }

    # Adjust to match n_statements
    diff_n <- n_statements - sum(dist)
    if (diff_n > 0) {
      # Add to middle positions
      dist[half + 1] <- dist[half + 1] + diff_n
    } else if (diff_n < 0) {
      # Remove from edges
      for (i in seq_len(abs(diff_n))) {
        edge_idx <- if (i %% 2 == 1) 1 else n_positions
        if (dist[edge_idx] > 1) dist[edge_idx] <- dist[edge_idx] - 1
      }
    }

    # Final adjustment if still off
    diff_n <- n_statements - sum(dist)
    if (diff_n != 0) {
      dist[half + 1] <- dist[half + 1] + diff_n
    }
  }

  # Adjust distribution to exactly match n_statements
  current_sum <- sum(dist)
  if (current_sum != n_statements) {
    diff_n <- n_statements - current_sum
    mid_pos <- ceiling(length(dist) / 2)
    dist[mid_pos] <- dist[mid_pos] + diff_n
  }

  dist <- pmax(dist, 0)

  return(dist)
}

# Report generator

#' Retention Evidence for Choosing the Number of Factors
#'
#' @description
#' One row per candidate number of factors, judged by eigenvalue, variance,
#' parallel analysis, Kaiser, Humphrey's rule, and the two-loadings rule.
#'
#' @param qdata A QsortData object
#' @param max_k Largest number of factors to evaluate (default 8, capped at
#'   one less than the number of sorts)
#' @param n_iter Permutations for parallel analysis (default 100)
#' @param percentile Parallel analysis percentile (default 95)
#' @param extraction Extraction method for the unrotated loadings
#' @param cor_method Correlation method
#'
#' @return A data.frame with columns k, eigenvalue, variance, cumulative,
#'   parallel_95, kaiser, humphrey, two_loadings, plus attributes
#'   `k_parallel`, `k_kaiser`, `k_two`, and `flag_threshold`.
#' @export
retention_evidence <- function(qdata, max_k = 8, n_iter = 100,
                               percentile = 95,
                               extraction = c("pca", "centroid", "minres"),
                               cor_method = c("pearson", "spearman", "kendall"),
                               pa = NULL) {

  extraction <- match.arg(extraction)
  cor_method <- match.arg(cor_method)
  if (!inherits(qdata, "QsortData")) {
    rlang::abort("qdata must be a QsortData object")
  }

  sorts <- qdata@sorts
  n <- nrow(sorts)
  J <- ncol(sorts)
  cm <- qsort_correlation(qdata, method = cor_method)
  if (is.null(pa)) {
    pa <- parallel_analysis(qdata, n_iter = n_iter, percentile = percentile)
  }
  kmax <- max(1L, min(as.integer(max_k), n - 1L))
  ext <- suppressWarnings(qsort_extract(cm, nfactors = kmax, method = extraction))
  lw <- ext$loadings

  thr <- 1.96 / sqrt(J)
  hum_thr <- 2 / sqrt(J)
  ks <- seq_len(kmax)
  eig <- pa$eigenvalue[ks]

  humphrey <- vapply(ks, function(k) {
    a <- sort(abs(lw[, k]), decreasing = TRUE)
    length(a) >= 2 && (a[1] * a[2]) > hum_thr
  }, logical(1))
  two_loadings <- vapply(ks, function(k) {
    sum(abs(lw[, k]) > thr) >= 2
  }, logical(1))

  out <- data.frame(
    k = ks,
    eigenvalue = eig,
    variance = eig / n,
    cumulative = cumsum(eig / n),
    parallel_95 = pa$threshold[ks],
    kaiser = eig > 1,
    humphrey = humphrey,
    two_loadings = two_loadings
  )
  attr(out, "k_parallel") <- attr(pa, "n_factors")
  attr(out, "k_kaiser") <- sum(pa$eigenvalue > 1)
  attr(out, "k_two") <- match(FALSE, two_loadings, nomatch = kmax + 1L) - 1L
  attr(out, "flag_threshold") <- thr
  class(out) <- c("retention_evidence", "data.frame")
  out
}

#' Tucker Congruence Coefficients Between Two Loading Matrices
#'
#' @description
#' phi = sum(xy) / sqrt(sum(x^2) sum(y^2)) for every column pair. Values
#' above 0.95 are conventionally read as equal factors, 0.85 to 0.94 as
#' fairly similar.
#'
#' @param a,b Numeric matrices with the same number of rows
#' @return A ncol(a) x ncol(b) matrix of congruence coefficients
#' @export
tucker_congruence <- function(a, b) {
  a <- as.matrix(a)
  b <- as.matrix(b)
  if (nrow(a) != nrow(b)) {
    rlang::abort("a and b must have the same number of rows")
  }
  num <- crossprod(a, b)
  den <- sqrt(outer(colSums(a^2), colSums(b^2)))
  phi <- num / den
  phi[!is.finite(phi)] <- NA_real_
  phi
}

#' Correlations Among Each Factor's Defining Sorts
#'
#' @description
#' For every factor, the correlation submatrix of its flagged sorts and the
#' mean off-diagonal correlation.
#'
#' @param results A QsortResults object
#' @return A list, one element per factor, each with `factor`, `n`,
#'   `correlations`, and `mean_r`
#' @export
defining_sort_correlations <- function(results) {
  if (!isS4(results)) {
    rlang::abort("results must be a QsortResults object")
  }
  flags <- results@flagging
  cm <- results@correlation
  lapply(seq_len(ncol(flags)), function(f) {
    idx <- which(flags[, f])
    sub <- cm[idx, idx, drop = FALSE]
    list(
      factor = f,
      n = length(idx),
      correlations = sub,
      mean_r = if (length(idx) >= 2) mean(sub[upper.tri(sub)]) else NA_real_
    )
  })
}

#' Per-Sort Mean and Standard Deviation
#'
#' @description
#' The free-distribution descriptives: each sort's mean placement and
#' standard deviation.
#'
#' @param qdata A QsortData object
#' @return A data.frame with participant, mean, and sd
#' @export
sort_descriptives <- function(qdata) {
  if (!inherits(qdata, "QsortData")) {
    rlang::abort("qdata must be a QsortData object")
  }
  s <- qdata@sorts
  data.frame(
    participant = rownames(s),
    mean = rowMeans(s, na.rm = TRUE),
    sd = apply(s, 1, stats::sd, na.rm = TRUE),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}
