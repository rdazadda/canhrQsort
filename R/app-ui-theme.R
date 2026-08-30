#' @title canhrQsort Dashboard Theme System
#' @description Theme for the canhrQsort Shiny dashboard, following the
#' canhrActi design system: UAF Blue (#236192) + Gold (#FFCD00) branding,
#' cool slate neutrals, Bootstrap 5 / bslib integration.
#' @name app-ui-theme
NULL

#' Create canhrQsort Theme
#'
#' @description
#' Creates a bslib Bootstrap 5 theme with UAF Blue + Gold branding,
#' matching the canhrActi dashboard design system.
#'
#' @param font_base Base font family
#' @param font_heading Heading font family
#'
#' @return A bslib bs_theme object
#' @keywords internal
canhrqsort_theme <- function(font_base = NULL,
                              font_heading = NULL) {

  if (!requireNamespace("bslib", quietly = TRUE)) {
    rlang::abort(c(
      "Package 'bslib' is required for the dashboard theme",
      "i" = "Install with: install.packages('bslib')"
    ))
  }

  # System font stack matching canhrActi: no Google fonts
  system_fonts <- c(
    "Segoe UI", "-apple-system", "BlinkMacSystemFont",
    "Roboto", "Helvetica Neue", "Arial", "sans-serif"
  )
  mono_fonts <- c(
    "SF Mono", "Monaco", "Inconsolata", "Fira Mono",
    "Droid Sans Mono", "Source Code Pro", "monospace"
  )

  bslib::bs_theme(
    version = 5,
    preset = "bootstrap",

    base_font = system_fonts,
    heading_font = system_fonts,
    code_font = mono_fonts,
    font_scale = 1.0,

    # UAF Blue + Gold branded colors (canhrActi design system)
    primary = "#236192",
    secondary = "#64748b",
    success = "#17a589",
    info = "#236192",
    warning = "#f4b942",
    danger = "#e67e22",

    # Body
    bg = "#f5f7fa",
    fg = "#1a202c",

    "border-radius" = "0.375rem",
    "border-radius-sm" = "0.25rem",
    "border-radius-lg" = "0.75rem",
    "card-border-radius" = "0.5rem",
    "border-color" = "#e2e8f0"
  )
}

#' Get Theme Colors
#'
#' @description
#' Returns theme colors for plots and visualizations, following the
#' canhrActi design system (UAF Blue + Gold).
#'
#' @param mode Theme mode (kept for backward compatibility)
#' @return Named list of color values
#' @keywords internal
get_theme_colors <- function(mode = "light") {

  list(
    bg_primary = "#ffffff",
    bg_secondary = "#f8fafc",
    bg_card = "#ffffff",
    bg_body = "#f5f7fa",
    text_primary = "#1a202c",
    text_secondary = "#2d3748",
    text_muted = "#4a5568",
    border = "#e2e8f0",
    grid = "#e2e8f0",
    accent_primary = "#236192",
    accent_secondary = "#FFCD00",
    accent_success = "#17a589",
    accent_warning = "#f4b942",
    accent_danger = "#e67e22",
    accent_info = "#236192",

    # canhrActi primary chart palette (UAF brand)
    factor_colors = c(
      "#236192", "#FFB800", "#71984A", "#DF6A2E",
      "#F45197", "#87D1E6", "#1a4a6f", "#E6358B"
    ),

    diverging = c("#DF6A2E", "#F4B183", "#f8fafc", "#7FA8C9", "#236192"),
    sequential = c("#DEEBF7", "#9ECAE1", "#6BAED6", "#4292C6", "#236192")
  )
}

#' Create Theme-Aware ggplot2 Theme
#'
#' @description
#' Creates a clean ggplot2 theme matching the dashboard, ported from
#' canhrActi's theme_canhrActi(): minimal base, slate grid, near-black text.
#'
#' @param mode Theme mode (kept for backward compatibility)
#' @param base_size Base font size
#'
#' @return A ggplot2 theme object
#' @export
theme_canhrqsort <- function(mode = "light", base_size = 14) {

  colors <- list(
    text_primary = "#111111",
    text_secondary = "#1F2937",
    text_muted = "#374151",
    background = "#FFFFFF",
    grid = "#E2E8F0",
    border = "#CBD5E1"
  )

  type_scale <- list(
    caption = 0.79, label = 0.86, body = 1.0, heading = 1.43
  )

  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = base_size * type_scale$heading, face = "bold",
        color = colors$text_primary, hjust = 0,
        margin = ggplot2::margin(b = 8), lineheight = 1.2
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size * type_scale$body, color = colors$text_secondary,
        hjust = 0, margin = ggplot2::margin(b = 12), lineheight = 1.4
      ),
      plot.caption = ggplot2::element_text(
        size = base_size * type_scale$caption, color = colors$text_muted,
        hjust = 1, margin = ggplot2::margin(t = 8)
      ),
      axis.title = ggplot2::element_text(
        size = base_size * type_scale$body, face = "bold",
        color = colors$text_primary
      ),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10), angle = 90),
      axis.text = ggplot2::element_text(
        size = base_size * type_scale$label, color = colors$text_secondary
      ),
      axis.ticks = ggplot2::element_line(color = colors$border, linewidth = 0.5),
      axis.ticks.length = ggplot2::unit(4, "pt"),
      legend.title = ggplot2::element_text(
        size = base_size * type_scale$body, face = "bold",
        color = colors$text_primary
      ),
      legend.text = ggplot2::element_text(
        size = base_size * type_scale$label, color = colors$text_secondary
      ),
      legend.background = ggplot2::element_rect(fill = colors$background, color = NA),
      legend.key = ggplot2::element_rect(fill = colors$background, color = NA),
      panel.background = ggplot2::element_rect(fill = colors$background, color = NA),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = colors$grid, linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = colors$background, color = NA),
      plot.margin = ggplot2::margin(12, 12, 12, 12),
      strip.text = ggplot2::element_text(
        size = base_size * type_scale$body, face = "bold",
        color = colors$text_primary,
        margin = ggplot2::margin(b = 8, t = 8)
      ),
      strip.background = ggplot2::element_rect(fill = colors$grid, color = NA)
    )
}

#' Setup Thematic
#'
#' @param mode Theme mode (kept for backward compatibility)
#' @keywords internal
setup_thematic <- function(mode = "light") {

  if (!requireNamespace("thematic", quietly = TRUE)) return(invisible(NULL))

  colors <- get_theme_colors()

  thematic::thematic_shiny(
    bg = colors$bg_card,
    fg = colors$text_primary,
    accent = colors$accent_primary,
    font = "Segoe UI"
  )
}

