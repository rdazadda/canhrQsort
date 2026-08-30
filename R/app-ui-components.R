#' @title canhrQsort Dashboard UI Components
#' @description Reusable UI component library for the canhrQsort dashboard,
#' ported from the canhrActi component library (shared_components.R /
#' canhr_components.R). All styling via classes in canhrqsort.css.
#' Icons are FontAwesome via shiny::icon(), matching canhrActi.
#' @name app-ui-components
NULL

# Page Header (canhrActi .page-header: white card, blue left border, gold top strip) ----

# Cards (canhrActi canhr_card: standard / hero / info modes) ----

#' Create a Dashboard Card
#'
#' canhrActi-style card. \code{mode = "standard"} gives a header + body card;
#' \code{mode = "hero"} gives a minimal-chrome chart card; \code{mode = "info"}
#' gives an icon + value + label metric card.
#'
#' @param ... Card body content (standard/hero modes).
#' @param mode Card mode: "standard", "hero", or "info".
#' @param title Card title.
#' @param icon_name Optional FontAwesome icon name.
#' @param value,label,sublabel Info-mode content.
#' @param color Info-mode accent: "teal", "gold", "navy".
#' @param footer Optional footer content.
#' @param collapsible,collapsed Collapse behavior (standard mode).
#' @param id Optional element ID.
#' @param header_extra Optional content at the right side of the header.
#' @param class Additional CSS classes.
#'
#' @return A \code{shiny.tag} object.
#' @keywords internal
qsort_card <- function(...,
                       mode = c("standard", "hero", "info"),
                       title = NULL,
                       icon_name = NULL,
                       value = NULL,
                       label = NULL,
                       sublabel = NULL,
                       color = c("teal", "gold", "navy"),
                       footer = NULL,
                       collapsible = FALSE,
                       collapsed = FALSE,
                       id = NULL,
                       header_extra = NULL,
                       class = NULL) {

  mode <- match.arg(mode)
  color <- match.arg(color)

  if (mode == "info") {
    return(htmltools::div(
      id = id,
      class = paste("canhr-custom-card canhr-card-info",
                    paste0("canhr-color-", color), class),
      if (!is.null(icon_name)) htmltools::div(class = "canhr-card-icon", shiny::icon(icon_name)),
      htmltools::div(
        class = "canhr-card-content",
        if (!is.null(value)) htmltools::div(class = "canhr-card-value", value),
        if (!is.null(label)) htmltools::div(class = "canhr-card-label", label),
        if (!is.null(sublabel)) htmltools::div(class = "canhr-card-sublabel", sublabel)
      )
    ))
  }

  hero_class <- if (mode == "hero") "canhr-card-hero" else ""
  collapse_class <- if (collapsible && collapsed) "canhr-card-collapsed" else ""

  header_html <- NULL
  if (!is.null(title) || collapsible || !is.null(header_extra)) {
    collapse_btn <- NULL
    if (collapsible) {
      collapse_btn <- htmltools::tags$button(
        type = "button",
        class = "canhr-collapse-btn",
        onclick = "$(this).closest('.canhr-custom-card').toggleClass('canhr-card-collapsed');",
        shiny::icon(if (collapsed) "chevron-down" else "chevron-up")
      )
    }

    header_html <- htmltools::div(
      class = "canhr-card-header",
      htmltools::div(
        class = "canhr-card-title-group",
        if (!is.null(icon_name)) htmltools::span(class = "canhr-card-title-icon", shiny::icon(icon_name)),
        if (!is.null(title)) htmltools::tags$h4(class = "canhr-card-title", title)
      ),
      htmltools::div(
        class = "canhr-card-actions",
        header_extra,
        collapse_btn
      )
    )
  }

  footer_html <- if (!is.null(footer)) {
    htmltools::div(class = "canhr-card-footer card-footer", footer)
  }

  htmltools::div(
    id = id,
    class = paste("canhr-custom-card", hero_class, collapse_class, class),
    header_html,
    htmltools::div(class = "canhr-card-body", ...),
    footer_html
  )
}

# Metric Components (canhrActi metric-item) ----

#' Create a Compact Metric Item (icon chip + value + label)
#'
#' canhrActi .metric-item, as used in the upload page metrics strip.
#'
#' @param value The metric value.
#' @param label The metric label.
#' @param icon_name FontAwesome icon name.
#' @param icon_class Extra class for the icon chip (e.g. "files", "duration").
#'
#' @return A \code{shiny.tag} object.
#' @keywords internal
metric_item <- function(value, label, icon_name, icon_class = "files") {
  htmltools::div(
    class = "metric-item",
    htmltools::div(class = paste("metric-icon", icon_class), shiny::icon(icon_name)),
    htmltools::div(
      htmltools::div(class = "metric-value", value),
      htmltools::div(class = "metric-label", label)
    )
  )
}

# Status Components ----

# Empty States (canhrActi empty_state) ----

#' Create an Empty State Placeholder
#'
#' @param icon_name FontAwesome icon name for the placeholder icon.
#' @param title Heading text.
#' @param message Descriptive message text.
#' @param action_button Optional action button.
#' @param small If TRUE, renders a compact variant.
#'
#' @return A \code{shiny.tag} object.
#' @keywords internal
empty_state <- function(icon_name = NULL,
                        title = NULL,
                        message = NULL,
                        action_button = NULL,
                        small = FALSE) {

  htmltools::div(
    class = paste("empty-state", if (small) "empty-state-sm"),
    if (!is.null(icon_name)) {
      htmltools::div(class = "empty-state-icon", shiny::icon(icon_name))
    },
    if (!is.null(title)) htmltools::div(class = "empty-state-title", title),
    if (!is.null(message)) htmltools::div(class = "empty-state-message", message),
    if (!is.null(action_button)) {
      htmltools::div(class = "empty-state-action", action_button)
    }
  )
}

# Chart Panel (canhrActi hero-chart-container pattern) ----

# Form helpers ----

