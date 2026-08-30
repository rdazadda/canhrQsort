#' @title canhrQsort Dashboard
#' @description CANHR-branded Shiny dashboard for Q-sort analysis, following
#' the canhrActi design system: UAF Blue header, navy sidebar, gold accents.
#' @name app-main
NULL

#' Run canhrQsort Dashboard
#'
#' @description
#' Launch the canhrQsort dashboard:
#' - CANHR / UAF design system (shared with canhrActi)
#' - Workflow: Upload, Analyze (frequentist + Bayesian), Visualize, Export
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
#' # Launch the dashboard
#' run_canhrqsort()
#'
#' # Launch with preloaded data
#' run_canhrqsort(my_qdata)
#' }
run_canhrqsort <- function(data = NULL,
                           launch.browser = TRUE,
                           port = NULL,
                           ...) {
  app <- qsort_app(data = data)
  if (!is.null(port)) {
    shiny::runApp(app, launch.browser = launch.browser, port = port, ...)
  } else {
    shiny::runApp(app, launch.browser = launch.browser, ...)
  }
}

#' Build the Dashboard App Object
#'
#' The app object without running it, for hosts like Posit Connect.
#' [run_qsort_app()] runs it locally.
#'
#' @param data Optional QsortData object to preload
#' @return A shiny app object
#' @export
qsort_app <- function(data = NULL) {

  required_pkgs <- c("shiny", "bslib", "htmltools", "DT", "plotly")
  missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

  if (length(missing) > 0) {
    rlang::abort(c(
      "Missing packages required for the dashboard",
      "x" = paste("Missing:", paste(missing, collapse = ", ")),
      "i" = paste0("Install with: install.packages(c('", paste(missing, collapse = "', '"), "'))")
    ))
  }

  # Add resource path for static files (CSS, JS, logo)
  www_dir <- system.file("www", package = "canhrQsort")

  # Fallback for development mode (devtools::load_all)
  if (!nzchar(www_dir) || !dir.exists(www_dir)) {
    pkg_path <- find.package("canhrQsort", quiet = TRUE)
    if (length(pkg_path) > 0) {
      www_dir <- file.path(pkg_path, "inst", "www")
    }
  }

  if (nzchar(www_dir) && dir.exists(www_dir)) {
    shiny::addResourcePath("www", www_dir)
    css_dir <- file.path(www_dir, "css")
    if (dir.exists(css_dir)) {
      shiny::addResourcePath("css", css_dir)
    }
    js_dir <- file.path(www_dir, "js")
    if (dir.exists(js_dir)) {
      shiny::addResourcePath("js", js_dir)
    }
  } else {
    message("Warning: Could not find www directory for static resources")
  }

  shiny::shinyApp(
    ui = canhrqsort_ui(),
    server = canhrqsort_server(preload_data = data)
  )
}


#' Asset cache-busting version (file mtime as integer)
#' @keywords internal
asset_version <- function(rel_path) {
  path <- system.file("www", rel_path, package = "canhrQsort")
  v <- if (nzchar(path)) suppressWarnings(as.integer(file.mtime(path))) else NA_integer_
  if (is.na(v)) 1L else v
}


#' Sidebar navigation link helper
#' @keywords internal
sidebar_nav_item <- function(id, label, active = FALSE) {
  htmltools::tags$li(
    class = if (active) "active" else NULL,
    shiny::actionLink(id, label)
  )
}


#' canhrQsort Dashboard UI
#'
#' @return Shiny UI object
#' @keywords internal
canhrqsort_ui <- function() {

  app_theme <- canhrqsort_theme()

  bslib::page(
    theme = app_theme,
    title = "canhrQsort",

    # Head resources (mtime-based ?v= defeats stale browser caches after redesigns)
    htmltools::tags$head(
      htmltools::tags$link(rel = "stylesheet",
                           href = paste0("css/canhrqsort.css?v=", asset_version("css/canhrqsort.css"))),
      htmltools::tags$script(src = paste0("js/canhrqsort.js?v=", asset_version("js/canhrqsort.js"))),
      htmltools::tags$meta(charset = "UTF-8"),
      htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
    ),

    # ---- Fixed header (UAF Blue, gold bottom border) ----
    htmltools::tags$header(
      class = "main-header",

      htmltools::tags$span(
        class = "logo",
        htmltools::tags$span(
          class = "header-brand",
          htmltools::tags$img(src = "www/logo.png", alt = "", class = "brand-logo-img",
                              height = "42",
                              style = "height:42px;width:auto;"),
          htmltools::tags$span(class = "brand-name", "canhrQsort")
        )
      ),

      htmltools::tags$nav(
        class = "navbar",

        htmltools::tags$a(
          href = "#",
          class = "sidebar-toggle",
          role = "button",
          `aria-label` = "Toggle sidebar",
          shiny::icon("bars")
        ),

        htmltools::tags$ul(
          class = "nav",

          htmltools::tags$li(
            htmltools::div(
              class = "header-page-title",
              htmltools::div(class = "page-title", id = "header_page_title", "Overview")
            )
          ),

          htmltools::tags$li(
            htmltools::div(
              class = "header-file-info",
              htmltools::div(
                class = "file-badge file-badge-empty",
                id = "header_status",
                shiny::icon("folder-open"),
                "Ready to analyze"
              )
            )
          ),

          htmltools::tags$li(
            htmltools::tags$a(
              href = "https://github.com/canhr/canhrQsort",
              target = "_blank",
              class = "header-link",
              title = "Report issues or request features",
              shiny::icon("question-circle"),
              htmltools::tags$span(class = "header-link-text", "Support")
            )
          )
        )
      )
    ),

    # ---- Fixed sidebar (navy, gold brand) ----
    htmltools::tags$aside(
      class = "main-sidebar",
      htmltools::tags$section(
        class = "sidebar",

        htmltools::div(
          class = "sidebar-brand-section",
          title = "Center for Alaska Native Health Research",
          htmltools::div(class = "sidebar-brand-mark", "CANHR"),
          htmltools::div(
            class = "sidebar-brand-descriptor",
            "Center for Alaska Native Health Research"
          )
        ),

        htmltools::tags$ul(
          class = "sidebar-menu",

          sidebar_nav_item("nav_home", "Overview", active = TRUE),

          htmltools::tags$li(class = "sidebar-section-header", "Analysis"),
          sidebar_nav_item("nav_analyze", "Frequentist"),
          sidebar_nav_item("nav_bayesian", "Bayesian"),

          htmltools::tags$li(class = "sidebar-section-header", "Output"),
          sidebar_nav_item("nav_visualize", "Visualization")
        ),

        htmltools::div(
          class = "sidebar-footer",
          htmltools::div(
            class = "sidebar-version",
            paste0("Version ", utils::packageVersion("canhrQsort"))
          ),
          htmltools::div(
            class = "sidebar-copyright",
            "© CANHR | UAF"
          )
        )
      )
    ),

    # ---- Main content area ----
    htmltools::div(
      class = "content-wrapper",
      htmltools::tags$section(
        class = "content",

        bslib::navset_hidden(
          id = "main_nav",

          bslib::nav_panel_hidden(
            value = "home",
            home_page_ui("home")
          ),
          bslib::nav_panel_hidden(
            value = "analyze",
            analyze_page_ui("analyze")
          ),
          bslib::nav_panel_hidden(
            value = "bayesian",
            bayesian_page_ui("bayesian")
          ),
          bslib::nav_panel_hidden(
            value = "visualize",
            visualize_page_ui("visualize")
          )
        )
      )
    )
  )
}


#' canhrQsort Dashboard Server
#'
#' @param preload_data Optional QsortData to preload
#' @return Shiny server function
#' @keywords internal
canhrqsort_server <- function(preload_data = NULL) {

  function(input, output, session) {

    # Reactive values (shared state)
    rv <- shiny::reactiveValues(
      # Data
      qdata = preload_data,
      data_loaded = !is.null(preload_data),
      validation = NULL,

      # Frequentist Analysis (from Analyze tab)
      results = NULL,
      analysis_complete = FALSE,

      # Bootstrap
      bootstrap = NULL,

      # Bayesian Analysis (from Bayesian tab)
      bayesian = NULL,
      bayesian_results = NULL,

      # Wizard state (legacy)
      wizard_step = 1
    )

    # Initialize thematic for consistent plot styling
    setup_thematic()

    # Theme every bayesqm plot with the CANHR/UAF palette
    if (requireNamespace("bayesqm", quietly = TRUE)) {
      bayesqm::bayesqm_set_colors(list(
        dark = "#1a4a6f", accent = "#8F272C", grey = "grey40",
        gridgrey = "grey75", fill = "#E1ECF4",
        qual = get_theme_colors()$factor_colors
      ))
    }

    # Sidebar navigation handlers
    shiny::observeEvent(input$nav_home, {
      bslib::nav_select("main_nav", "home")
    })
    shiny::observeEvent(input$nav_analyze, {
      bslib::nav_select("main_nav", "analyze")
    })
    shiny::observeEvent(input$nav_bayesian, {
      bslib::nav_select("main_nav", "bayesian")
    })
    shiny::observeEvent(input$nav_visualize, {
      bslib::nav_select("main_nav", "visualize")
    })

    # Sync sidebar active state + header page title when tab changes
    shiny::observe({
      current_tab <- input$main_nav
      if (!is.null(current_tab)) {
        session$sendCustomMessage("syncSidebar", current_tab)
      }
    })

    # Update header file/status badge
    shiny::observe({
      if (!isTRUE(rv$data_loaded) || is.null(rv$qdata)) {
        status <- list(icon = "folder-open", text = "No data loaded", active = FALSE)
      } else {
        n_part <- nrow(rv$qdata@sorts)
        n_stmt <- ncol(rv$qdata@sorts)
        base_text <- paste0(n_part, " × ", n_stmt, " loaded")
        if (isTRUE(rv$analysis_complete) || !is.null(rv$bayesian)) {
          status <- list(icon = "check-circle", text = paste0(base_text, " · analyzed"), active = TRUE)
        } else {
          status <- list(icon = "database", text = base_text, active = TRUE)
        }
      }
      session$sendCustomMessage("updateHeaderStatus", status)
    })

    # Module servers
    home_page_server("home", rv = rv, parent_session = session)
    analyze_page_server("analyze", rv = rv, parent_session = session)
    visualize_page_server("visualize", rv = rv, parent_session = session)
    bayesian_page_server("bayesian", rv = rv, parent_session = session)

    # Session cleanup
    session$onSessionEnded(function() {
      # Cleanup if needed
    })
  }
}
