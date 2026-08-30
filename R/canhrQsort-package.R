#' @title canhrQsort: Q-Sort Methodology Analysis
#'
#' @description
#' Q-sort methodology analysis:
#'
#' \itemize{
#'   \item \strong{Statistical methods}: Bayesian Q methodology through
#'         the \pkg{bayesqm} engine (exact rank-order likelihood), bootstrap
#'         confidence intervals, and cross-validation
#'   \item \strong{Data import}: PQMethod, HTMLQ,
#'         FlashQ, Ken-Q, CSV, and Excel
#'   \item \strong{Dashboard}: Shiny app for the full workflow
#' }
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{qsort_analyze}}}{Complete Q-sort analysis pipeline}
#'   \item{\code{\link{qsort_bootstrap}}}{Bootstrap confidence intervals}
#'   \item{\code{\link[bayesqm]{fit_bayesian}}}{Bayesian analysis (bayesqm engine, used by the dashboard)}
#'   \item{\code{\link{run_qsort_app}}}{Interactive Shiny dashboard}
#' }
#'
#' @section Data Import:
#' \describe{
#'   \item{\code{\link{read_qsort_csv}}}{Import from CSV files}
#'   \item{\code{\link{read_pqmethod}}}{Import from PQMethod}
#'   \item{\code{\link{read_htmlq}}}{Import from HTMLQ/FlashQ}
#' }
#'
#' @section Visualization:
#' \describe{
#'   \item{\code{\link{plot_loadings}}}{Factor loadings plot}
#'   \item{\code{\link{plot_scores}}}{Factor scores visualization}
#'   \item{\code{\link{plot_factor_arrays}}}{Q-sort grid arrays}
#' }
#'
#' @author Raymond Dacosta Azadda \email{rdazadda@@alaska.edu}
#'
#' @references
#' Stephenson, W. (1953). The study of behavior: Q-technique and its methodology.
#' University of Chicago Press.
#'
#' Brown, S. R. (1980). Political subjectivity: Applications of Q methodology in
#' political science. Yale University Press.
#'
#' Zabala, A. (2014). qmethod: A package to explore human perspectives using
#' Q methodology. The R Journal, 6(2), 163-173.
#'
#' Zabala, A., & Pascual, U. (2016). Bootstrapping Q methodology to improve the
#' understanding of human perspectives. PloS one, 11(2), e0148087.
#'
#' @docType package
#' @name canhrQsort-package
#' @aliases canhrQsort
#' @keywords package
#'
#' @import methods
#' @import ggplot2
#' @importFrom rlang .data %||% abort warn inform is_empty sym
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble as_tibble
#' @importFrom stringr str_detect str_extract str_replace str_trim str_split
#'   str_extract_all str_replace_all str_wrap str_to_title
#' @importFrom readr read_csv read_lines cols col_character col_double col_integer
#' @importFrom readxl read_excel
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning cli_alert_danger
#'   cli_progress_bar cli_progress_update cli_progress_done cli_h1 cli_h2
#'   cli_text cli_rule cli_bullets
#' @importFrom glue glue
#' @importFrom GPArotation quartimax oblimin
#' @importFrom psych principal fa KMO cortest.bartlett
#' @importFrom stats cor cov sd var qnorm pnorm quantile median prcomp
#'   factanal promax hclust as.dist complete.cases dnorm mad
#'   rnorm setNames na.omit approx reorder
#' @importFrom graphics par plot.new plot.window abline grid
#' @importFrom grDevices colorRampPalette
#' @importFrom utils head tail read.csv write.csv packageVersion zip
#' @importFrom tools file_ext file_path_sans_ext
#'
"_PACKAGE"

# Global variables to avoid R CMD check notes
# These are primarily ggplot2 aesthetics and dplyr non-standard evaluation variables
utils::globalVariables(c(

  # General variables used in NSE contexts
  ".", "statement", "factor", "loading", "zscore", "flag", "sort_value",
  "participant", "value", "variable", "n", "mean", "sd", "ci_lower",
  "ci_upper", "sig", "type", "name", "id", "record_id", "item",

  # ggplot2 aesthetics
  "correlation", "participant_i", "participant_j", "text_color",
  "label", "row_val", "col_val", "statement_num", "text", "flagged",
  "statement_factor", "position", "distinguishing", "eigenvalue",
  "first_order", "factor1", "factor2",

  # Shiny app variables
  "Score", "Participant", "F1", "F2", "Var1", "Var2", "Factor", "Eigenvalue",

  # Bootstrap/analysis variables
  "boot_mean", "original", "bc_estimate", "bias",

  # Bayesian visualization variables
  "ci_50_lower", "ci_50_upper", "group", "item_num", "x",

  # Other variables
  "form_name", "normal",

  # Factor array plot variables
  "n_in_col", "y_offset", "row_in_pos", "y", "score",

  # Visualization plot variables
  "Col", "Row", "Correlation", "FactorLabel", "PointSize",
  "Rank", "Statement", "Zscore", "y_pos", "Component",
  "VarExplained", "comp", "stmt_idx", "stmt_text"
))
