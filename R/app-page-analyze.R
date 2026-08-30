#' @title Analysis Page Module
#' @description Frequentist Q-sort analysis page for the canhrQsort dashboard,
#' following the canhrActi analysis-page pattern: page header, control bar
#' with a Run button, collapsible settings, factor metric strip, and tabbed
#' results.
#' @name app-page-analyze
NULL

#' Analysis Page UI
#'
#' @param id Module ID
#' @return Shiny UI elements
#' @keywords internal
analyze_page_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::div(
    id = ns("fr_page"),
    class = "fr-page mode-priorities",

    # One control bar: the mode decision, then mode-specific controls
    htmltools::div(
      class = "bq2-bar",
      htmltools::div(
        class = "bq2-group",
        htmltools::div(class = "bq2-label", "Approach"),
        shiny::uiOutput(ns("fr_mode_toggle"))
      ),

      htmltools::div(class = "bq2-bar-spacer"),

      htmltools::div(
        class = "pr-only bq2-bar-actions",
        priorities_bar_ui(ns)
      )
    ),

    # Main content area (dynamic per mode)
    shiny::uiOutput(ns("main_content"))
  )
}


#' Analysis Page Server
#'
#' @param id Module ID
#' @param rv Reactive values from parent
#' @param parent_session Parent session for navigation
#' @keywords internal
analyze_page_server <- function(id, rv, parent_session) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Approach: Priorities (quick, descriptive) | Factors (full model) ----
    fr_mode <- shiny::reactiveVal("priorities")

    output$fr_mode_toggle <- shiny::renderUI({
      mode <- fr_mode()
      htmltools::div(
        class = "bq2-seg",
        shiny::actionButton(
          ns("seg_priorities"), "Priorities",
          class = paste("btn bq2-seg-btn", if (mode == "priorities") "active")
        ),
        shiny::actionButton(
          ns("seg_factors"), "Factors",
          class = paste("btn bq2-seg-btn", if (mode == "factors") "active")
        )
      )
    })

    set_mode <- function(mode) {
      fr_mode(mode)
      session$sendCustomMessage("setClassIf", list(
        id = ns("fr_page"), className = "mode-priorities",
        on = identical(mode, "priorities")
      ))
    }
    shiny::observeEvent(input$seg_priorities, set_mode("priorities"))
    shiny::observeEvent(input$seg_factors, set_mode("factors"))

    # A completed factor run lands the user on its results
    shiny::observeEvent(rv$analysis_complete, {
      if (isTRUE(rv$analysis_complete)) set_mode("factors")
    }, ignoreInit = TRUE)

    # The Priorities layer: verdict, ranked table, flow chart, differences
    priorities_server_bind(input, output, session, rv, ns)

    # Analysis configuration
    config <- shiny::reactiveValues(
      n_factors = 3,
      extraction = "pca",
      rotation = "varimax",
      correlation = "pearson",
      auto_flag = TRUE,
      flag_threshold = 0.45,
      flag_p_level = 0.05,
      flag_majority = TRUE
    )

    shiny::observeEvent(input$n_factors, { config$n_factors <- input$n_factors })
    shiny::observeEvent(input$extraction_method, { config$extraction <- input$extraction_method })
    shiny::observeEvent(input$correlation_method, { config$correlation <- input$correlation_method })


    # RUN ANALYSIS
    # One pipeline: the Extract button runs it and advances to Flag; the
    # step-2 controls (rotation, flagging settings) re-run it in place so
    # their edits recompute live without navigating

    do_run <- function(advance = TRUE, msg = "Running Analysis") {
      shiny::req(rv$data_loaded, rv$qdata)

      shiny::withProgress(message = msg, value = 0, {

        shiny::incProgress(0.1, detail = "Computing correlations...")

        tryCatch({
          rv$results <- qsort_analyze(
            rv$qdata,
            nfactors = config$n_factors,
            extraction = config$extraction,
            rotation = config$rotation,
            cor_method = config$correlation,
            flagging = if (config$auto_flag) "auto" else "theoretical",
            flag_threshold = if (!config$auto_flag) config$flag_threshold else NULL,
            flag_p_level = config$flag_p_level,
            flag_majority = config$flag_majority
          )

          shiny::incProgress(0.9, detail = "Finalizing...")

          rv$analysis_complete <- TRUE
          rv$fr_run_id <- (rv$fr_run_id %||% 0) + 1
          rv$fr_boot <- NULL
          rv$fr_manual_flags <- FALSE
          if (advance) rv$fr_step <- 2

          if (advance) {
            k_run <- rv$results@n_factors
            n_def_run <- sum(rowSums(rv$results@flagging) > 0)
            session$sendCustomMessage("showToast", list(
              message = sprintf("%d %s extracted · %d of %d sorts defining",
                                k_run,
                                if (k_run == 1) "factor" else "factors",
                                n_def_run, nrow(rv$results@flagging)),
              type = "success"
            ))
          }

        }, error = function(e) {
          session$sendCustomMessage("showToast", list(
            message = paste("Analysis error:", e$message),
            type = "error",
            duration = 5000
          ))
        })
      })
    }

    shiny::observeEvent(input$run_analysis, do_run(advance = TRUE))

    # Step-2 controls recompute live once a solution exists. On the initial
    # input binding analysis_complete is FALSE, so only the config updates;
    # re-created inputs re-register with their current value and never fire.
    shiny::observeEvent(input$rotation_method, {
      config$rotation <- input$rotation_method
      if (isTRUE(rv$analysis_complete)) do_run(advance = FALSE, msg = "Rotating")
    })
    shiny::observeEvent(input$flag_p_level, {
      config$flag_p_level <- as.numeric(input$flag_p_level)
      if (isTRUE(rv$analysis_complete)) do_run(advance = FALSE, msg = "Re-flagging")
    })
    shiny::observeEvent(input$auto_flag, {
      config$auto_flag <- input$auto_flag
      if (isTRUE(rv$analysis_complete)) do_run(advance = FALSE, msg = "Re-flagging")
    })
    shiny::observeEvent(input$flag_majority, {
      config$flag_majority <- isTRUE(input$flag_majority)
      if (isTRUE(rv$analysis_complete)) do_run(advance = FALSE, msg = "Re-flagging")
    })


    # MAIN CONTENT AREA

    output$main_content <- shiny::renderUI({
      # State 1: No data loaded (either approach). qdata is isolated so
      # in-place mutations (attribute attach, re-binning) refresh the inner
      # outputs without rebuilding the panel, which would reset the open tab.
      if (!rv$data_loaded || is.null(shiny::isolate(rv$qdata))) {
        return(htmltools::p(
          class = "bq2-plan",
          "Import a dataset on the Overview page to begin."
        ))
      }

      # Priorities: live immediately, no model run required
      if (identical(fr_mode(), "priorities")) {
        return(priorities_body_ui(ns))
      }

      # Factors: retention evidence before a run, the full results after
      factors_body_ui(ns)
    })

    # The Factors layer: the four-step guided flow
    factors_server_bind(input, output, session, rv, ns, config)

  })
}
