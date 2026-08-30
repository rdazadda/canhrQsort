#' @title canhrQsort Dashboard
#' @description The Shiny dashboard behind run_qsort_app().
#' @name shiny-app
NULL

#' Run Q-Sort Analysis Dashboard
#'
#' @description
#' Launch the canhrQsort dashboard. The app covers:
#' - Data import (CSV, Excel, PQMethod, HTMLQ, Ken-Q)
#' - Frequentist and Bayesian analysis
#' - Manual rotation, bipolar detection, crib sheets
#' - Factor arrays and the figure gallery
#' - Bootstrap confidence intervals
#' - Workbook and figure exports
#'
#' @param data Optional QsortData object to preload
#' @param launch.browser Logical; launch in browser (default TRUE)
#' @param port Port number for the app
#' @param ... Additional arguments passed to shiny::runApp()
#'
#' @export
#'
#' @examples
#' \dontrun{
#' run_qsort_app()
#' run_qsort_app(my_qdata)
#' }
run_qsort_app <- function(data = NULL, launch.browser = TRUE, port = NULL, ...) {
  # Redirect to the new modern dashboard
  run_canhrqsort(data = data, launch.browser = launch.browser, port = port, ...)
}
