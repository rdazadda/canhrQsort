#' @title Data Validation Functions
#' @description Functions to validate Q-sort data quality and detect issues
#' @name validation
NULL

#' Validate Q-Sort Data
#'
#' @description
#' Validate Q-sort data:
#' - Distribution conformity
#' - Missing values
#' - Invalid values
#' - Duplicate responses
#' - Response pattern anomalies
#'
#' @param data A QsortData object or matrix of Q-sorts
#' @param distribution Expected distribution (optional)
#' @param strict Logical; use strict validation rules (default FALSE)
#'
#' @return A list with validation results including validity status and issues found
#' @export
#'
#' @examples
#' \dontrun{
#' validation <- validate_qsort(qdata)
#' if (!validation$valid) {
#'   print(validation$issues)
#' }
#' }
validate_qsort <- function(data, distribution = NULL, strict = FALSE) {

  if (inherits(data, "QsortData")) {
    sorts <- data@sorts
    if (is.null(distribution)) {
      distribution <- data@distribution
    }
  } else {
    sorts <- as.matrix(data)
  }

  issues <- character()
  warnings <- character()

  n_participants <- nrow(sorts)
  n_statements <- ncol(sorts)

  # Basic checks
  if (n_participants < 2) {
    issues <- c(issues, "At least 2 participants required for Q analysis")
  }

  if (n_statements < 5) {
    issues <- c(issues, "At least 5 statements required for meaningful analysis")
  }

  # Check for missing values
  n_missing <- sum(is.na(sorts))
  if (n_missing > 0) {
    pct_missing <- round(n_missing / length(sorts) * 100, 1)
    if (pct_missing > 10) {
      issues <- c(issues, glue::glue("High missing value rate: {pct_missing}% ({n_missing} cells)"))
    } else if (pct_missing > 0) {
      warnings <- c(warnings, glue::glue("Missing values detected: {pct_missing}% ({n_missing} cells)"))
    }
  }

  # Check distribution conformity
  if (!is.null(distribution) && length(distribution) > 0) {
    dist_check <- check_distribution(sorts, distribution)
    if (!dist_check$conforms) {
      issues <- c(issues, dist_check$message)
    }
    if (length(dist_check$non_conforming) > 0) {
      n_bad <- length(dist_check$non_conforming)
      warnings <- c(warnings, glue::glue("{n_bad} Q-sorts don't match expected distribution"))
    }
  }

  # Check value range
  value_range <- range(sorts, na.rm = TRUE)
  expected_range <- if (!is.null(distribution)) {
    c(-floor(length(distribution) / 2), floor(length(distribution) / 2))
  } else {
    c(-6, 6)  # Default reasonable range
  }

  if (value_range[1] < expected_range[1] - 1 || value_range[2] > expected_range[2] + 1) {
    warnings <- c(warnings, glue::glue(
      "Unusual value range [{value_range[1]}, {value_range[2]}] detected"
    ))
  }

  # Check for duplicate Q-sorts
  duplicates <- find_duplicate_qsorts(sorts)
  if (length(duplicates) > 0) {
    warnings <- c(warnings, glue::glue(
      "{length(duplicates)} potential duplicate Q-sort pairs detected"
    ))
  }

  # Check for careless responses
  careless <- detect_careless_response(sorts)
  if (length(careless$flagged) > 0) {
    warnings <- c(warnings, glue::glue(
      "{length(careless$flagged)} potentially careless responses detected"
    ))
  }

  # Check correlation matrix properties
  cor_matrix <- cor(t(sorts), use = "pairwise.complete.obs")
  if (any(is.na(cor_matrix))) {
    issues <- c(issues, "Correlation matrix contains NA values (insufficient data)")
  }

  # Determine overall validity
  valid <- length(issues) == 0

  list(
    valid = valid,
    issues = issues,
    warnings = warnings,
    summary = list(
      n_participants = n_participants,
      n_statements = n_statements,
      n_missing = n_missing,
      value_range = value_range,
      n_duplicates = length(duplicates),
      n_careless = length(careless$flagged)
    ),
    details = list(
      duplicates = duplicates,
      careless = careless
    )
  )
}


#' Check Distribution Conformity
#'
#' @description
#' Check if Q-sorts conform to the expected forced distribution.
#'
#' @param sorts Matrix of Q-sorts
#' @param distribution Expected distribution (count per column)
#'
#' @return List with conformity status and non-conforming Q-sorts
#' @export
#'
#' @examples
#' \dontrun{
#' check <- check_distribution(qdata@sorts, c(2, 3, 4, 5, 6, 5, 4, 3, 2))
#' }
check_distribution <- function(sorts, distribution) {

  if (is.null(distribution) || length(distribution) == 0) {
    return(list(
      conforms = TRUE,
      message = "No distribution specified",
      non_conforming = integer()
    ))
  }

  n_cols <- length(distribution)
  expected_sum <- sum(distribution)

  # Infer value range from distribution
  center <- ceiling(n_cols / 2)
  min_val <- 1 - center
  max_val <- n_cols - center

  non_conforming <- integer()

  for (i in seq_len(nrow(sorts))) {
    qsort <- sorts[i, ]
    qsort <- qsort[!is.na(qsort)]

    # Check total items
    if (length(qsort) != expected_sum) {
      non_conforming <- c(non_conforming, i)
      next
    }

    # Check distribution shape
    observed_dist <- as.numeric(table(factor(qsort, levels = min_val:max_val)))

    if (!all(observed_dist == distribution)) {
      non_conforming <- c(non_conforming, i)
    }
  }

  n_conform <- nrow(sorts) - length(non_conforming)
  pct_conform <- round(n_conform / nrow(sorts) * 100, 1)

  list(
    conforms = length(non_conforming) == 0,
    message = if (length(non_conforming) == 0) {
      "All Q-sorts conform to expected distribution"
    } else {
      glue::glue("{pct_conform}% of Q-sorts conform to distribution")
    },
    non_conforming = non_conforming,
    n_conforming = n_conform
  )
}


#' Detect Careless Responses
#'
#' @description
#' Detect potentially careless or random responses using multiple indicators:
#' - Low inter-item variance
#' - Extreme response patterns
#' - Unusual correlation with other Q-sorts
#' - Response time (if available)
#'
#' @param data A QsortData object or matrix of Q-sorts
#' @param methods Character vector of detection methods to use
#' @param threshold Sensitivity threshold (default 0.05)
#'
#' @return List with flagged participants and detection details
#' @export
#'
#' @examples
#' \dontrun{
#' careless <- detect_careless_response(qdata)
#' print(careless$flagged)
#' }
detect_careless_response <- function(data,
                                     methods = c("variance", "correlation", "pattern"),
                                     threshold = 0.05) {

  if (inherits(data, "QsortData")) {
    sorts <- data@sorts
  } else {
    sorts <- as.matrix(data)
  }

  n <- nrow(sorts)
  flags <- matrix(FALSE, nrow = n, ncol = length(methods),
                  dimnames = list(rownames(sorts), methods))
  scores <- matrix(0, nrow = n, ncol = length(methods),
                   dimnames = list(rownames(sorts), methods))

  # Method 1: Low variance (monotonous responding)
  if ("variance" %in% methods) {
    row_vars <- apply(sorts, 1, var, na.rm = TRUE)
    # Expected variance should be based on actual Q-sort value range, not column indices
    # For a typical Q-sort with values from min to max (e.g., -4 to +4)
    value_range <- range(sorts, na.rm = TRUE)
    expected_values <- seq(value_range[1], value_range[2])
    expected_var <- var(rep(expected_values, length.out = ncol(sorts)))
    low_var_threshold <- expected_var * 0.3  # Less than 30% of expected

    scores[, "variance"] <- row_vars / expected_var
    flags[, "variance"] <- row_vars < low_var_threshold
  }

  # Method 2: Unusual correlation pattern
  if ("correlation" %in% methods) {
    cor_matrix <- cor(t(sorts), use = "pairwise.complete.obs")
    avg_correlations <- rowMeans(cor_matrix, na.rm = TRUE)

    # Flag those with very low average correlation (outliers)
    cor_mad <- mad(avg_correlations, na.rm = TRUE)
    cor_median <- median(avg_correlations, na.rm = TRUE)

    scores[, "correlation"] <- avg_correlations

    # Handle edge case where MAD is 0 (all correlations identical)
    # In this case, use standard deviation or don't flag based on correlation
    if (cor_mad > 0) {
      flags[, "correlation"] <- avg_correlations < (cor_median - 3 * cor_mad)
    } else {
      # Fallback: use SD-based threshold if MAD is 0
      cor_sd <- sd(avg_correlations, na.rm = TRUE)
      if (cor_sd > 0) {
        flags[, "correlation"] <- avg_correlations < (cor_median - 3 * cor_sd)
      } else {
        # All correlations identical - no outliers possible
        flags[, "correlation"] <- FALSE
      }
    }
  }

  # Method 3: Response pattern (e.g., straight-lining, zigzag)
  if ("pattern" %in% methods) {
    pattern_scores <- sapply(seq_len(n), function(i) {
      qsort <- sorts[i, ]

      if (length(qsort) < 2 || all(is.na(qsort))) {
        return(0.5)  # Neutral score for invalid data
      }

      qsort_clean <- qsort[!is.na(qsort)]
      if (length(qsort_clean) < 2) {
        return(0.5)
      }

      # Check for runs of identical values (straight-lining)
      runs <- rle(qsort_clean)
      max_run <- max(runs$lengths)
      # Normalize: 1 run of all same values = 1.0 (most suspicious)
      run_score <- max_run / length(qsort_clean)

      # Check for alternating pattern (zigzag) - also suspicious
      diffs <- diff(qsort_clean)
      if (length(diffs) > 1) {
        # Count sign changes: if every consecutive pair changes sign, it's zigzag
        sign_changes <- sum(diffs[-1] * diffs[-length(diffs)] < 0)
        max_possible_changes <- length(diffs) - 1
        alternating <- if (max_possible_changes > 0) sign_changes / max_possible_changes else 0
      } else {
        alternating <- 0
      }

      # Combined suspicion score (0 = perfect, 1 = highly suspicious)
      # Weight straight-lining more heavily as it's more clearly careless
      suspicious_score <- run_score * 0.6 + alternating * 0.4

      # Return quality score: higher = better quality
      # 1.0 = no suspicious patterns, 0.0 = highly suspicious
      quality_score <- 1 - suspicious_score

      # Clamp to valid range
      max(0, min(1, quality_score))
    })

    scores[, "pattern"] <- pattern_scores
    flags[, "pattern"] <- pattern_scores < threshold
  }

  # Combine flags - flag if multiple indicators triggered
  combined_flags <- rowSums(flags) >= 2
  flagged <- which(combined_flags)

  list(
    flagged = flagged,
    flagged_ids = rownames(sorts)[flagged],
    method_flags = flags,
    method_scores = scores,
    summary = data.frame(
      method = methods,
      n_flagged = colSums(flags)
    )
  )
}


#' Find Duplicate Q-Sorts
#'
#' @description
#' Identify potential duplicate Q-sorts based on high correlation
#' or identical responses.
#'
#' @param sorts Matrix of Q-sorts
#' @param threshold Correlation threshold for duplicate detection (default 0.95)
#'
#' @return Data frame of potential duplicate pairs
#' @keywords internal
find_duplicate_qsorts <- function(sorts, threshold = 0.95) {

  n <- nrow(sorts)
  if (n < 2) return(data.frame())

  cor_matrix <- cor(t(sorts), use = "pairwise.complete.obs")

  # Find pairs above threshold (excluding diagonal)
  duplicates <- data.frame()

  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      if (!is.na(cor_matrix[i, j]) && cor_matrix[i, j] > threshold) {
        duplicates <- rbind(duplicates, data.frame(
          participant1 = rownames(sorts)[i],
          participant2 = rownames(sorts)[j],
          correlation = round(cor_matrix[i, j], 4),
          identical = all(sorts[i, ] == sorts[j, ], na.rm = TRUE)
        ))
      }
    }
  }

  return(duplicates)
}


#' Compute Data Quality Metrics
#'
#' @description
#' Compute data quality metrics for Q-sort data.
#'
#' @param data A QsortData object
#'
#' @return Data frame of quality metrics
#' @export
compute_quality_metrics <- function(data) {

  if (inherits(data, "QsortData")) {
    sorts <- data@sorts
  } else {
    sorts <- as.matrix(data)
  }

  n <- nrow(sorts)
  p <- ncol(sorts)

  cor_matrix <- cor(t(sorts), use = "pairwise.complete.obs")

  # KMO measure (Kaiser-Meyer-Olkin)
  kmo <- tryCatch({
    psych::KMO(cor_matrix)$MSA
  }, error = function(e) NA)

  # Bartlett's test (n = number of observations/participants, not variables)
  bartlett <- tryCatch({
    bt <- psych::cortest.bartlett(cor_matrix, n = n)
    bt$p.value
  }, error = function(e) NA)

  # Average inter-correlation
  avg_cor <- mean(cor_matrix[lower.tri(cor_matrix)], na.rm = TRUE)

  # Determinant (should be > 0 for valid correlation matrix)
  det_cor <- det(cor_matrix)

  # Completeness rate
  completeness <- 1 - sum(is.na(sorts)) / length(sorts)

  data.frame(
    metric = c("KMO", "Bartlett_p", "Avg_Correlation", "Determinant",
               "Completeness", "N_Participants", "N_Statements"),
    value = c(kmo, bartlett, avg_cor, det_cor, completeness, n, p),
    interpretation = c(
      interpret_kmo(kmo),
      interpret_bartlett(bartlett),
      interpret_avg_cor(avg_cor),
      interpret_determinant(det_cor),
      interpret_completeness(completeness),
      interpret_n(n),
      interpret_p(p)
    )
  )
}


# Interpretation helper functions
interpret_kmo <- function(x) {
  if (is.na(x)) return("Could not compute")
  if (x >= 0.9) return("Excellent")
  if (x >= 0.8) return("Good")
  if (x >= 0.7) return("Acceptable")
  if (x >= 0.6) return("Mediocre")
  return("Poor - reconsider analysis")
}

interpret_bartlett <- function(x) {
  if (is.na(x)) return("Could not compute")
  if (x < 0.001) return("Highly significant")
  if (x < 0.05) return("Significant")
  return("Not significant - correlations may be identity")
}

interpret_avg_cor <- function(x) {
  if (is.na(x)) return("Could not compute")
  if (x > 0.5) return("High inter-correlation")
  if (x > 0.3) return("Moderate - typical for Q")
  if (x > 0.1) return("Low")
  return("Very low - check data")
}

interpret_determinant <- function(x) {
  if (is.na(x)) return("Could not compute")
  if (x > 0.00001) return("OK - matrix invertible")
  return("Near singular - possible multicollinearity")
}

interpret_completeness <- function(x) {
  if (x == 1) return("Complete data")
  if (x > 0.95) return("Minimal missing data")
  if (x > 0.90) return("Some missing data")
  return("High missing data - consider imputation")
}

interpret_n <- function(x) {
  if (x >= 40) return("Excellent sample size")
  if (x >= 20) return("Good sample size")
  if (x >= 10) return("Adequate for Q")
  return("Small - interpret with caution")
}

interpret_p <- function(x) {
  if (x >= 60) return("Large Q-set")
  if (x >= 40) return("Typical Q-set size")
  if (x >= 20) return("Small Q-set")
  return("Very small Q-set")
}
