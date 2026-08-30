#' @title S4 Classes for Q-Sort Analysis
#' @description Core S4 class definitions for the canhrQsort package
#' @name qsort-classes
#' @keywords internal
NULL


# QsortData Class


#' @title QsortData Class
#' @description S4 class to store Q-sort data with metadata
#'
#' @slot sorts A matrix of Q-sort data (participants x statements)
#' @slot statements A character vector of statement texts
#' @slot participants A character vector of participant IDs
#' @slot distribution A numeric vector defining the forced distribution
#' @slot metadata A list containing additional metadata
#' @slot source A character string indicating the data source
#' @slot created A POSIXct timestamp of when the object was created
#'
#' @exportClass QsortData
#' @examples
#' \dontrun{
#' # Create QsortData from matrix
#' data <- qsort_data(
#'   sorts = my_matrix,
#'   statements = paste("Statement", 1:40),
#'   distribution = c(2, 3, 4, 5, 6, 7, 6, 5, 4, 3, 2)
#' )
#' }
setClass(
  "QsortData",
  slots = c(
    sorts = "matrix",
    statements = "character",
    participants = "character",
    distribution = "numeric",
    metadata = "list",
    source = "character",
    created = "POSIXct"
  ),
  prototype = list(
    sorts = matrix(numeric(0), nrow = 0, ncol = 0),
    statements = character(0),
    participants = character(0),
    distribution = numeric(0),
    metadata = list(),
    source = "unknown",
    created = Sys.time()
  )
)

#' @title Create QsortData Object
#' @description Constructor function for QsortData S4 class
#'
#' @param sorts A matrix or data.frame with Q-sorts (rows = participants, cols = statements)
#' @param statements Character vector of statement texts (optional if column names exist)
#' @param participants Character vector of participant IDs (optional if row names exist)
#' @param distribution Numeric vector defining the forced distribution shape
#' @param metadata List of additional metadata (demographics, study info, etc.)
#' @param source Character string indicating data source
#' @param validate Logical; whether to validate the data structure (default TRUE)
#'
#' @return A QsortData S4 object
#' @export
#'
#' @examples
#' \dontrun{
#' # From matrix
#' sorts_matrix <- matrix(sample(-4:4, 100, replace = TRUE), nrow = 10, ncol = 10)
#' qdata <- qsort_data(sorts_matrix, distribution = c(1, 2, 3, 2, 1))
#'
#' # From data.frame
#' qdata <- qsort_data(my_df, statements = stmt_texts)
#' }
qsort_data <- function(sorts,
                       statements = NULL,
                       participants = NULL,
                       distribution = NULL,
                       metadata = list(),
                       source = "manual",
                       validate = TRUE) {

 if (is.data.frame(sorts)) {
    numeric_cols <- sapply(sorts, is.numeric)
    if (!all(numeric_cols)) {
      cli::cli_alert_warning("Non-numeric columns detected and will be moved to metadata")
      metadata$extra_cols <- sorts[, !numeric_cols, drop = FALSE]
      sorts <- sorts[, numeric_cols, drop = FALSE]
    }
    sorts <- as.matrix(sorts)
  }

  # Ensure matrix is numeric (matches paper's storage.mode approach)
  if (!is.numeric(sorts)) {
    na_before <- sum(is.na(sorts))
    sorts <- apply(sorts, 2, as.numeric)
    # apply() can drop dimensions for single-row matrices
    if (!is.matrix(sorts)) sorts <- matrix(sorts, nrow = 1)
    na_after <- sum(is.na(sorts))
    if (na_after > na_before) {
      cli::cli_alert_warning(
        "{na_after - na_before} value(s) could not be converted to numeric and became NA"
      )
    }
  }
  storage.mode(sorts) <- "double"

  if (is.null(statements)) {
    if (!is.null(colnames(sorts))) {
      statements <- colnames(sorts)
    } else {
      statements <- paste0("S", seq_len(ncol(sorts)))
      cli::cli_alert_info("Generated default statement labels: S1, S2, ...")
    }
  }

  if (is.null(participants)) {
    if (!is.null(rownames(sorts))) {
      participants <- rownames(sorts)
    } else {
      participants <- paste0("P", seq_len(nrow(sorts)))
      cli::cli_alert_info("Generated default participant IDs: P1, P2, ...")
    }
  }

  if (any(duplicated(participants))) {
    dup_ids <- unique(participants[duplicated(participants)])
    rlang::warn(c(
      "Duplicate participant IDs detected",
      "!" = paste0("Duplicated IDs: ", paste(dup_ids, collapse = ", ")),
      "i" = "Duplicate IDs can cause issues in factor analysis and merging operations",
      "i" = "Consider making participant IDs unique before analysis"
    ))
    # Make IDs unique by appending suffix
    participants <- make.unique(participants, sep = "_")
    cli::cli_alert_info("Made participant IDs unique by appending suffixes")
  }

  colnames(sorts) <- statements
  rownames(sorts) <- participants

  # Infer distribution if not provided
  if (is.null(distribution)) {
    distribution <- infer_distribution(sorts)
    cli::cli_alert_info("Inferred distribution from data: {paste(distribution, collapse = ', ')}")
  }

  obj <- new(
    "QsortData",
    sorts = sorts,
    statements = statements,
    participants = participants,
    distribution = distribution,
    metadata = metadata,
    source = source,
    created = Sys.time()
  )

  if (validate) {
    validation <- validate_qsort(obj)
    if (!validation$valid) {
      cli::cli_alert_warning("Data validation issues detected:")
      for (issue in validation$issues) {
        cli::cli_alert_warning("  - {issue}")
      }
    } else {
      cli::cli_alert_success("Q-sort data validated successfully")
    }
  }

  return(obj)
}

#' Infer Distribution from Q-sort Data
#' @keywords internal
infer_distribution <- function(sorts) {
  if (is.null(sorts) || length(sorts) == 0) {
    return(numeric(0))
  }

  # Get unique values and their expected counts
  vals <- sort(unique(as.vector(sorts[!is.na(sorts)])))

  if (length(vals) == 0) {
    return(numeric(0))
  }

  min_val <- min(vals, na.rm = TRUE)
  max_val <- max(vals, na.rm = TRUE)

  # Handle case where all values are the same: one rank level with J items
  if (min_val == max_val) {
    return(as.numeric(ncol(sorts)))
  }

  # Count occurrences per row and find modal distribution
  dist_counts <- apply(sorts, 1, function(x) {
    table(factor(x, levels = min_val:max_val))
  })

  if (is.matrix(dist_counts)) {
    modal_dist <- apply(dist_counts, 1, function(x) {
      x_clean <- x[!is.na(x)]
      if (length(x_clean) == 0) return(0)
      tab <- table(x_clean)
      if (length(tab) == 0) return(0)
      as.numeric(names(which.max(tab)))
    })
  } else if (is.list(dist_counts)) {
    # Handle case where dist_counts is a list (single row)
    modal_dist <- as.numeric(dist_counts)
  } else {
    modal_dist <- as.numeric(dist_counts)
  }

  # Ensure we return valid numeric values
  modal_dist[is.na(modal_dist)] <- 0

  return(as.numeric(modal_dist))
}


# QsortResults Class


#' @title QsortResults Class
#' @description S4 class to store Q-sort analysis results
#'
#' @slot data The original QsortData object
#' @slot correlation Correlation matrix between Q-sorts
#' @slot extraction List with extraction method details and unrotated loadings
#' @slot rotation List with rotation method details and rotated loadings
#' @slot flagging Matrix of flagging indicators
#' @slot factor_scores Data frame of z-scores for each factor
#' @slot distinguishing List of distinguishing statements per factor
#' @slot consensus Character vector of consensus statements
#' @slot factor_characteristics Data frame of factor characteristics
#' @slot n_factors Number of factors extracted
#' @slot method_details List of method parameters used
#' @slot call The original function call
#'
#' @exportClass QsortResults
setClass(
  "QsortResults",
  slots = c(
    data = "QsortData",
    correlation = "matrix",
    extraction = "list",
    rotation = "list",
    flagging = "matrix",
    factor_scores = "data.frame",
    distinguishing = "list",
    consensus = "character",
    factor_characteristics = "list",  # Contains characteristics df, se_factor, sed_matrix
    n_factors = "integer",
    method_details = "list",
    call = "call"
  )
)

#' @title Create QsortResults Object
#' @description Constructor function for QsortResults S4 class
#' @keywords internal
qsort_results <- function(data, correlation, extraction, rotation, flagging,
                          factor_scores, distinguishing, consensus,
                          factor_characteristics, n_factors, method_details, call) {
  new(
    "QsortResults",
    data = data,
    correlation = correlation,
    extraction = extraction,
    rotation = rotation,
    flagging = flagging,
    factor_scores = factor_scores,
    distinguishing = distinguishing,
    consensus = consensus,
    factor_characteristics = factor_characteristics,
    n_factors = as.integer(n_factors),
    method_details = method_details,
    call = call
  )
}


# QsortBootstrap Class


#' @title QsortBootstrap Class
#' @description S4 class to store bootstrap results for Q-sort analysis
#'
#' @slot original The original QsortResults object
#' @slot n_bootstrap Number of bootstrap iterations
#' @slot loading_samples Array of bootstrapped factor loadings
#' @slot score_samples Array of bootstrapped factor scores
#' @slot loading_ci Data frame of confidence intervals for loadings
#' @slot score_ci Data frame of confidence intervals for scores
#' @slot flag_frequency Matrix of flagging frequencies across bootstraps
#' @slot stability_metrics List of stability metrics
#' @slot seed Random seed used for reproducibility
#'
#' @exportClass QsortBootstrap
setClass(
  "QsortBootstrap",
  slots = c(
    original = "QsortResults",
    n_bootstrap = "integer",
    loading_samples = "array",
    score_samples = "array",
    loading_ci = "data.frame",
    score_ci = "data.frame",
    flag_frequency = "matrix",
    stability_metrics = "list",
    seed = "integer"
  )
)


# S4 Methods


#' Show Method for QsortData
#' @param object A QsortData object
#' @exportMethod show
setMethod("show", "QsortData", function(object) {
  cat("\n")
  cli::cli_h1("Q-Sort Data")
  cli::cli_text("{.strong Participants:} {length(object@participants)}")
  cli::cli_text("{.strong Statements:} {length(object@statements)}")
  cli::cli_text("{.strong Distribution:} [{paste(object@distribution, collapse = ', ')}]")
  cli::cli_text("{.strong Source:} {object@source}")
  cli::cli_text("{.strong Created:} {format(object@created, '%Y-%m-%d %H:%M:%S')}")

  if (length(object@metadata) > 0) {
    cli::cli_text("{.strong Metadata:} {paste(names(object@metadata), collapse = ', ')}")
  }
  cat("\n")
})

#' Show Method for QsortResults
#' @param object A QsortResults object
#' @exportMethod show
setMethod("show", "QsortResults", function(object) {
  cat("\n")
  cli::cli_h1("Q-Sort Analysis Results")

  cli::cli_h2("Data Summary")
  cli::cli_text("{.strong Participants:} {nrow(object@data@sorts)}")
  cli::cli_text("{.strong Statements:} {ncol(object@data@sorts)}")

  cli::cli_h2("Analysis Details")
  cli::cli_text("{.strong Factors extracted:} {object@n_factors}")
  cli::cli_text("{.strong Extraction method:} {object@method_details$extraction}")
  cli::cli_text("{.strong Rotation method:} {object@method_details$rotation}")
  cli::cli_text("{.strong Flagging method:} {object@method_details$flagging}")

  cli::cli_h2("Factor Summary")
  flagged_counts <- colSums(object@flagging)
  for (i in seq_len(object@n_factors)) {
    ev <- object@extraction$eigenvalues[i]
    var_exp <- object@extraction$variance_explained[i] * 100
    cli::cli_text("  Factor {i}: {flagged_counts[i]} Q-sorts, {round(var_exp, 1)}% variance")
  }

  n_consensus <- length(object@consensus)
  cli::cli_text("\n{.strong Consensus statements:} {n_consensus}")

  n_distinguishing <- sum(sapply(object@distinguishing, length))
  cli::cli_text("{.strong Distinguishing statements:} {n_distinguishing}")
  cat("\n")
})

#' Show Method for QsortBootstrap
#' @param object A QsortBootstrap object
#' @exportMethod show
setMethod("show", "QsortBootstrap", function(object) {
  cat("\n")
  cli::cli_h1("Q-Sort Bootstrap Results")
  cli::cli_text("{.strong Bootstrap iterations:} {object@n_bootstrap}")
  cli::cli_text("{.strong Random seed:} {object@seed}")

  cli::cli_h2("Stability Summary")
  mean_stability <- mean(object@stability_metrics$loading_stability, na.rm = TRUE)
  cli::cli_text("{.strong Mean loading stability:} {round(mean_stability, 3)}")

  flag_stability <- mean(diag(object@flag_frequency) / object@n_bootstrap, na.rm = TRUE)
  cli::cli_text("{.strong Mean flagging consistency:} {round(flag_stability * 100, 1)}%")
  cat("\n")
})

#' Summary Method for QsortResults
#' @param object A QsortResults object
#' @exportMethod summary
setMethod("summary", "QsortResults", function(object) {
  qsort_summary(object)
})

