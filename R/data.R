#' @title Demo Datasets for Q-Sort Analysis
#' @name demo-data
#' @description
#' Demonstration datasets for Q-sort methodology analysis. These datasets
#' are from published research and can be used for testing, learning, and
#' demonstrating the capabilities of the canhrQsort package.
#'
#' @details
#' Both datasets are from:
#'
#' Akhtar-Danesh, N. (2023). Impact of factor rotation on Q-methodology analysis.
#' PLOS ONE, 18(9), e0290728. \doi{10.1371/journal.pone.0290728}
#'
#' The data are available under a Creative Commons Attribution 4.0 International
#' (CC-BY 4.0) license.
#'
#' @keywords datasets
NULL


#' @title Grizzly Bear Reintroduction Q-Sort Dataset (via bayesqm)
#'
#' @description
#' The grizzly bear reintroduction Q dataset from Easter et al. (2025):
#' 67 participants each force-sorted 41 statements about reintroducing
#' grizzly bears to the North Cascades onto an eleven-column grid
#' (quotas 1-2-3-5-6-7-6-5-3-2-1, printed values -5 to +5). Shipped by
#' the \pkg{bayesqm} package as \code{\link[bayesqm]{grizzly_sorts}} and
#' offered as a sample dataset in the dashboard.
#'
#' @details
#' In the dashboard, load it from the Upload page ("Sample: Grizzly Bears")
#' or the Overview page's sample chooser. Programmatically:
#'
#' \preformatted{
#' gz <- bayesqm::grizzly_sorts
#' qdata <- qsort_data(
#'   sorts = t(as.matrix(gz$Y)),   # participants as rows
#'   distribution = gz$distribution
#' )
#' }
#'
#' @source
#' Easter, T. S., Santo, A. R., Sage, A. H., Carter, N. H., Chan, K. M. A.,
#' & Ransom, J. I. (2025). Divergent values and perspectives drive three
#' distinct viewpoints on grizzly bear reintroduction in Washington, the
#' United States. People and Nature, 7, 127-145. \doi{10.1002/pan3.10748}.
#' Data from the Dryad repository under CC0, \doi{10.5061/dryad.73n5tb369}.
#'
#' @name grizzly-demo
#' @keywords datasets
NULL


#' @title Childhood Obesity Q-Sort Dataset
#'
#' @description
#' A Q-sort dataset examining parents' perceptions on childhood obesity,
#' its impact on children's health, and barriers in preventing childhood obesity.
#'
#' @format A list with the following components:
#' \describe{
#'   \item{sorts}{A 42 x 33 numeric matrix of Q-sort scores. Rows are statements
#'     (S1-S42), columns are participants (P1-P33). Values range from -4 to +4.}
#'   \item{statements}{A character vector of 42 statement texts about childhood
#'     obesity, health impacts, and prevention barriers.}
#'   \item{distribution}{An integer vector defining the forced distribution:
#'     c(2, 4, 5, 6, 8, 6, 5, 4, 2) for positions -4 to +4.}
#'   \item{n_participants}{Integer: 33}
#'   \item{n_statements}{Integer: 42}
#'   \item{source}{Character: "Akhtar-Danesh (2023) PLOS ONE"}
#'   \item{topic}{Character: "Childhood Obesity"}
#'   \item{description}{Character: Brief description of the study}
#' }
#'
#' @details
#' The dataset includes 33 parents attending well-baby check-up clinics who
#' sorted 42 statements about childhood obesity. The Q-sort grid ranged from
#' -4 (Strongly Disagree) to +4 (Strongly Agree).
#'
#' This dataset is particularly relevant for health research applications,
#' aligning with the CANHR (Center for Alaska Native Health Research) mission.
#'
#' @source
#' Akhtar-Danesh, N. (2023). Impact of factor rotation on Q-methodology analysis.
#' PLOS ONE, 18(9), e0290728. \doi{10.1371/journal.pone.0290728}
#'
#' @details
#' The workbook ships with the package under `inst/Datasets` and loads from
#' the dashboard's sample chooser ("Sample: Obesity"). Programmatically:
#'
#' \preformatted{
#' path <- system.file("Datasets", "Childhood obesity dataset.xlsx",
#'                     package = "canhrQsort")
#' raw <- readxl::read_excel(path)
#' }
#'
#' @name obesity-demo
#' @keywords datasets
NULL
