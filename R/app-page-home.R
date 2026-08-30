#' @title Overview Page Module (merged Overview + Upload)
#' @description The single front door of the dashboard. Before data: a plain
#' product statement, a drag-and-drop import zone showing the forced
#' distribution as outline tiles with one gold apex, and a quiet list of
#' sample datasets. After data: the dataset title with provenance and
#' validation, the distribution fingerprint in the rank-color ramp, a
#' four-step workflow rail with a single gold action, and a tabbed card
#' holding the data preview and per-participant Q-sort pyramids.
#' @name app-page-home
NULL

# The rank-color ramp shared by every pyramid in the app
ov2_ramp_anchors <- c("#00274C", "#1a4a6f", "#236192", "#87D1E6",
                      "#FFE066", "#FFB800", "#DF6A2E")

#' Build the outline pyramid for the empty drop zone (one gold apex tile)
#' @keywords internal
ov2_outline_pyramid <- function() {
  heights <- c(1, 2, 3, 4, 5, 4, 3, 2, 1)
  apex_col <- which.max(heights)
  htmltools::div(
    class = "ov2-pyr",
    lapply(seq_along(heights), function(i) {
      htmltools::div(
        class = "ov2-pyr-col",
        lapply(seq_len(heights[i]), function(k) {
          gold <- i == apex_col && k == 1
          htmltools::div(class = paste("ov2-pyr-tile", if (gold) "gold"))
        })
      )
    })
  )
}

#' Build the loaded-data distribution fingerprint in the rank ramp
#' @keywords internal
ov2_fingerprint <- function(distribution) {
  C <- length(distribution)
  if (C == 0) return(NULL)
  cols <- grDevices::colorRampPalette(ov2_ramp_anchors)(C)
  labels <- if (C %% 2 == 0) c(seq(-C / 2, -1), seq(1, C / 2)) else seq_len(C) - (C + 1) / 2
  fmt <- function(x) sub("-", "−", as.character(x), fixed = TRUE)
  htmltools::div(
    class = "ov2-fp",
    htmltools::div(
      class = "ov2-fp-tiles",
      lapply(seq_len(C), function(i) {
        htmltools::div(
          class = "ov2-fp-col",
          lapply(seq_len(distribution[i]), function(k) {
            htmltools::div(class = "ov2-fp-tile",
                           style = paste0("background:", cols[i], ";"))
          })
        )
      })
    ),
    htmltools::div(
      class = "ov2-fp-caption",
      paste0("Forced distribution, ", fmt(labels[1]), " to +", labels[C])
    )
  )
}

#' One sample dataset row for the empty state
#' @keywords internal
ov2_sample_row <- function(input_id, name, meta, description) {
  shiny::actionLink(
    input_id,
    class = "ov2-sample-row",
    label = htmltools::div(
      class = "ov2-sample-grid",
      htmltools::div(
        htmltools::div(class = "ov2-sample-name", name),
        htmltools::div(class = "ov2-sample-meta", meta)
      ),
      htmltools::div(class = "ov2-sample-desc", description),
      htmltools::div(class = "ov2-sample-load", "Load")
    )
  )
}

#' One step of the workflow rail
#' @keywords internal
ov2_step <- function(title, state, description = NULL, actions = NULL, last = FALSE,
                     line_done = FALSE) {
  node <- switch(
    state,
    done = htmltools::div(class = "ov2-node done", shiny::icon("check")),
    current = htmltools::div(class = "ov2-node current", htmltools::div(class = "ov2-node-dot")),
    htmltools::div(class = "ov2-node next")
  )
  htmltools::div(
    class = "ov2-step",
    htmltools::div(
      class = "ov2-step-track",
      node,
      if (!last) htmltools::div(class = paste("ov2-line", if (line_done) "done"))
    ),
    htmltools::div(class = paste("ov2-step-title", state), title),
    if (!is.null(description)) htmltools::div(class = "ov2-step-desc", description),
    actions
  )
}


#' Overview Page UI
#'
#' @param id Module ID
#' @return Shiny UI elements
#' @keywords internal
home_page_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::uiOutput(ns("overview_body")),
    htmltools::div(
      class = "ov2-footer",
      htmltools::tags$a(href = "https://www.uaf.edu/canhr/", target = "_blank",
                        class = "ov2-footer-link", "CANHR Website"),
      shiny::actionLink(ns("action_docs"), "Documentation", class = "ov2-footer-link")
    )
  )
}


#' Overview Page Server
#'
#' @param id Module ID
#' @param rv Reactive values from parent
#' @param parent_session Parent session for navigation
#' @keywords internal
home_page_server <- function(id, rv, parent_session) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # The page body: import surface before data, mission control after ----
    output$overview_body <- shiny::renderUI({
      if (!isTRUE(rv$data_loaded) || is.null(rv$qdata)) ov2_empty_ui() else ov2_loaded_ui()
    })

    ov2_empty_ui <- function() {
      htmltools::tagList(
        htmltools::div(
          class = "ov2-hero",
          htmltools::div(
            htmltools::h1(class = "ov2-h1", "Factor analysis for Q sorts"),
            htmltools::p(
              class = "ov2-lede",
              "Run centroid or PCA with varimax or manual rotation and bootstrap ",
              "confidence intervals, or fit a Bayesian model with credible intervals ",
              "and model comparison for K."
            )
          ),
          htmltools::div(
            class = "ov2-dropzone",
            id = ns("upload_zone"),
            role = "region",
            `aria-label` = "File upload area",
            onclick = paste0("var el = document.getElementById('", ns("import_file"),
                             "'); if (el) el.click();"),
            ov2_outline_pyramid(),
            htmltools::div(class = "ov2-drop-title", "Drag & drop your Q-sort file"),
            htmltools::div(
              class = "ov2-drop-actions",
              htmltools::span(class = "btn-ov2-gold ov2-choose",
                              shiny::icon("upload"), "Choose file")
            ),
            htmltools::div(
              class = "ov2-accepts",
              "Accepts CSV, Excel, PQMethod .DAT, Ken-Q JSON, KADE .ZIP, TXT"
            ),
            shiny::fileInput(
              ns("import_file"), NULL,
              accept = c(".csv", ".txt", ".xlsx", ".xls", ".dat", ".DAT",
                         ".sta", ".STA", ".json", ".zip", ".rds"),
              buttonLabel = "", placeholder = ""
            )
          )
        ),
        htmltools::div(
          class = "ov2-samples",
          htmltools::h2(class = "ov2-h2", "Sample datasets"),
          htmltools::div(
            class = "ov2-sample-list",
            ov2_sample_row(ns("demo_lipset"), "Lipset Democracy",
                           "Lipset (1963) · 9 sorts × 33 statements",
                           "Political values. The classic Q example dataset."),
            ov2_sample_row(ns("demo_grizzly"), "Grizzly Bear Reintroduction",
                           "Easter et al. (2025) · 67 sorts × 41 statements",
                           "Conservation viewpoints from a large P set."),
            ov2_sample_row(ns("demo_obesity"), "Childhood Obesity",
                           "Akhtar-Danesh (2023) · 33 sorts × 42 statements",
                           "Health perceptions across professions.")
          )
        )
      )
    }

    ov2_loaded_ui <- function() {
      sorts <- rv$qdata@sorts
      n_part <- nrow(sorts)
      n_stmt <- ncol(sorts)
      dist <- rv$qdata@distribution

      freq_done <- isTRUE(rv$analysis_complete) && !is.null(rv$results)
      bayes_done <- !is.null(rv$bayesian)
      any_done <- freq_done || bayes_done

      v <- rv$validation
      v_ok <- is.null(v) || (length(v$issues) == 0 && length(v$warnings) == 0)

      analyze_desc <- if (!any_done) {
        "Bayesian adds credible intervals and model comparison for K"
      } else {
        paste(c(if (bayes_done) "Bayesian complete",
                if (freq_done) "Frequentist complete"), collapse = " · ")
      }
      analyze_actions <- if (!any_done) {
        htmltools::div(
          class = "ov2-actions",
          shiny::actionButton(ns("go_bayes"), "Run Bayesian", class = "btn-ov2-blue"),
          shiny::actionButton(ns("go_freq"), "Run frequentist", class = "btn-ov2-blue")
        )
      }
      viz_actions <- if (any_done) {
        htmltools::div(
          class = "ov2-actions",
          shiny::actionButton(ns("go_viz"), "Open visualization", class = "btn-ov2-blue")
        )
      }

      htmltools::tagList(
        htmltools::div(
          class = "ov2-loaded-head",
          htmltools::div(
            htmltools::h1(class = "ov2-title", rv$qdata@source),
            htmltools::div(
              class = "ov2-meta",
              htmltools::span(paste0("loaded ",
                                     format(rv$qdata@created, "%d %b %Y, %H:%M"))),
              htmltools::span(class = "ov2-dot", "·"),
              if (v_ok) {
                htmltools::span(class = "ov2-ok", shiny::icon("circle-check"),
                                "Validation passed")
              } else {
                htmltools::span(class = "ov2-warn", shiny::icon("triangle-exclamation"),
                                "Validation warnings")
              }
            )
          ),
          htmltools::div(
            class = "ov2-head-right",
            ov2_fingerprint(dist),
            shiny::actionButton(
              ns("remove_data"),
              htmltools::tagList(shiny::icon("rotate-left"), "Replace data"),
              class = "btn-ov2-ghost"
            )
          )
        ),

        shiny::uiOutput(ns("validation_alert")),

        htmltools::div(
          class = "ov2-stepper",
          ov2_step("Import", "done",
                   paste0(n_part, " sorts × ", n_stmt, " statements"),
                   line_done = TRUE),
          ov2_step("Analyze", if (any_done) "done" else "current",
                   analyze_desc, analyze_actions, line_done = any_done),
          ov2_step("Visualize", if (any_done) "current" else "next",
                   if (any_done) "Loadings, factor arrays, and crib sheets",
                   viz_actions, last = TRUE)
        ),

        htmltools::div(
          class = "ov2-card",
          htmltools::div(
            class = "ov2-tabbar",
            shiny::actionLink(ns("tab_table"), "Data preview",
                              class = "ov2-tab active"),
            shiny::actionLink(ns("tab_pyramid"), "Q-sort pyramids",
                              class = "ov2-tab")
          ),
          htmltools::div(
            class = "ov2-card-body",
            htmltools::div(id = ns("panel_table"), DT::DTOutput(ns("data_preview_table"))),
            htmltools::div(
              id = ns("panel_pyramid"),
              style = "display: none;",
              htmltools::div(
                class = "data-pyramid-controls",
                htmltools::div(
                  class = "data-pyramid-selector-group",
                  htmltools::span(class = "data-pyramid-label",
                                  shiny::icon("user"), "Participant"),
                  htmltools::div(
                    class = "data-pyramid-nav",
                    shiny::actionButton(ns("pyramid_prev"),
                                        label = shiny::icon("chevron-left"),
                                        class = "btn data-pyramid-nav-btn"),
                    shiny::selectInput(ns("pyramid_participant"), label = NULL,
                                       choices = rv$qdata@participants,
                                       selected = rv$qdata@participants[1],
                                       width = "250px"),
                    shiny::actionButton(ns("pyramid_next"),
                                        label = shiny::icon("chevron-right"),
                                        class = "btn data-pyramid-nav-btn")
                  )
                ),
                htmltools::span(class = "data-pyramid-counter",
                                shiny::textOutput(ns("pyramid_counter"), inline = TRUE))
              ),
              shiny::uiOutput(ns("pyramid_html")),
              shiny::uiOutput(ns("pyramid_pane"))
            )
          )
        )
      )
    }

    # Import: file upload ----
    shiny::observeEvent(input$import_file, {
      shiny::req(input$import_file)
      tryCatch({
        file_path <- input$import_file$datapath
        file_name <- input$import_file$name
        ext <- tolower(tools::file_ext(file_name))

        # A saved session restores full app state through the same drop zone
        if (ext == "rds") {
          load_session_file(file_path, rv, session, parent_session)
          return(invisible(NULL))
        }

        qdata <- switch(
          ext,
          dat = read_pqmethod(file_path),
          sta = rlang::abort(c(
            "A .STA file contains only statement texts, not Q-sort data",
            i = "To import PQMethod data, upload the .DAT file instead",
            i = "The .STA file can be used alongside the .DAT file for statement labels"
          )),
          json = read_kenq(file_path, format = "json"),
          csv = ,
          txt = smart_detect_csv(file_path, file_name),
          xlsx = ,
          xls = read_qsort_excel(file_path),
          zip = read_kade_zip(file_path),
          rlang::abort(paste("Unsupported file type:", ext))
        )

        # Importers only see Shiny's temp path; label the data with the
        # name of the file the user actually uploaded.
        qdata@source <- tools::file_path_sans_ext(file_name)

        rv$qdata <- qdata
        rv$data_loaded <- TRUE
        rv$validation <- validate_qsort(qdata)

        session$sendCustomMessage("showToast", list(
          message = paste("Successfully imported:", file_name), type = "success"
        ))
      }, error = function(e) {
        session$sendCustomMessage("showToast", list(
          message = paste("Import error:", e$message), type = "error", duration = 5000
        ))
      })
    })

    # Import: sample datasets ----
    shiny::observeEvent(input$demo_lipset, {
      tryCatch({
        rv$qdata <- lipset_qdata()
        rv$data_loaded <- TRUE
        rv$validation <- validate_qsort(rv$qdata)
        session$sendCustomMessage("showToast", list(
          message = "Lipset Democracy loaded: 9 sorts, 33 statements",
          type = "success"
        ))
      }, error = function(e) {
        session$sendCustomMessage("showToast", list(
          message = paste("Sample data error:", e$message), type = "error"
        ))
      })
    })

    shiny::observeEvent(input$demo_grizzly, {
      tryCatch({
        rv$qdata <- load_grizzly_qdata()
        rv$data_loaded <- TRUE
        rv$validation <- validate_qsort(rv$qdata)
        session$sendCustomMessage("showToast", list(
          message = "Grizzly Bear Reintroduction loaded: 67 sorts, 41 statements",
          type = "success"
        ))
      }, error = function(e) {
        session$sendCustomMessage("showToast", list(
          message = paste("Sample data error:", e$message), type = "error"
        ))
      })
    })

    shiny::observeEvent(input$demo_obesity, {
      tryCatch({
        file_path <- system.file("Datasets", "Childhood obesity dataset.xlsx",
                                 package = "canhrQsort")
        raw <- readxl::read_excel(file_path)
        n_part <- sum(grepl("^qsort", names(raw)))
        sorts <- t(as.matrix(raw[, paste0("qsort", seq_len(n_part))]))
        stmts <- if ("statement" %in% names(raw)) raw$statement else NULL

        rv$qdata <- qsort_data(
          sorts = sorts, statements = stmts,
          distribution = c(2, 4, 5, 6, 8, 6, 5, 4, 2),
          source = "Childhood Obesity (Akhtar-Danesh, 2023)"
        )
        rv$data_loaded <- TRUE
        rv$validation <- validate_qsort(rv$qdata)
        session$sendCustomMessage("showToast", list(
          message = "Childhood Obesity loaded: 33 sorts, 42 statements",
          type = "success"
        ))
      }, error = function(e) {
        session$sendCustomMessage("showToast", list(
          message = paste("Sample data error:", e$message), type = "error"
        ))
      })
    })

    # Replace data (back to the import surface) ----
    reset_app_data <- function() {
      rv$qdata <- NULL
      rv$data_loaded <- FALSE
      rv$validation <- NULL
      rv$results <- NULL
      rv$analysis_complete <- FALSE
      rv$bootstrap <- NULL
      rv$bayesian <- NULL
      rv$bayesian_results <- NULL
      session$sendCustomMessage("showToast", list(message = "Dataset cleared", type = "info"))
    }

    shiny::observeEvent(input$remove_data, {
      has_results <- isTRUE(rv$analysis_complete) || !is.null(rv$results) ||
        !is.null(rv$bayesian) || !is.null(rv$bootstrap) || !is.null(rv$bayesian_results)
      if (has_results) {
        shiny::showModal(shiny::modalDialog(
          title = "Replace data?",
          size = "s",
          easyClose = TRUE,
          htmltools::p("This clears the loaded dataset and every analysis result with it."),
          footer = htmltools::tagList(
            shiny::actionButton(ns("remove_confirm"), "Replace data", class = "btn-primary"),
            shiny::modalButton("Cancel")
          )
        ))
      } else {
        reset_app_data()
      }
    })

    shiny::observeEvent(input$remove_confirm, {
      shiny::removeModal()
      reset_app_data()
    })

    # Workflow rail navigation ----
    shiny::observeEvent(input$go_freq, {
      bslib::nav_select("main_nav", "analyze", session = parent_session)
    })
    shiny::observeEvent(input$go_bayes, {
      bslib::nav_select("main_nav", "bayesian", session = parent_session)
    })
    shiny::observeEvent(input$go_viz, {
      bslib::nav_select("main_nav", "visualize", session = parent_session)
    })

    # Validation banner ----
    output$validation_alert <- shiny::renderUI({
      shiny::req(rv$validation)
      v <- rv$validation
      msgs <- c(v$issues, v$warnings)
      if (length(msgs) == 0) return(NULL)
      htmltools::div(
        class = "validation-alert",
        id = ns("validation_banner"),
        htmltools::tags$button(
          class = "validation-alert__dismiss",
          onclick = paste0("document.getElementById('", ns("validation_banner"),
                           "').remove()"),
          shiny::icon("times")
        ),
        lapply(msgs, function(msg) {
          htmltools::div(class = "validation-alert__item",
                         shiny::icon("triangle-exclamation"), msg)
        })
      )
    })

    # Data preview table ----
    output$data_preview_table <- DT::renderDT({
      shiny::req(rv$data_loaded, rv$qdata)
      sorts <- rv$qdata@sorts
      stmts <- rv$qdata@statements
      participants <- rv$qdata@participants
      n_stmt <- ncol(sorts)

      preview <- as.data.frame(t(sorts))
      colnames(preview) <- participants
      preview <- cbind(
        StatNo = seq_len(n_stmt),
        if (!is.null(stmts) && length(stmts) == n_stmt) {
          data.frame(statement = stmts, stringsAsFactors = FALSE)
        },
        preview
      )

      has_text <- "statement" %in% names(preview)
      DT::datatable(
        preview,
        options = list(
          scrollX = TRUE, scrollY = "440px", pageLength = 50, dom = "frtip",
          autoWidth = TRUE,
          columnDefs = if (has_text) list(list(width = "420px", targets = 1)),
          language = list(info = "Showing _START_ to _END_ of _TOTAL_ statements")
        ),
        rownames = FALSE, class = "compact stripe", selection = "none"
      )
    })

    # Tab switching (shared JS handler) ----
    shiny::observeEvent(input$tab_table, {
      session$sendCustomMessage("switchDataTab", list(
        activeTab = "table",
        tabTableId = ns("tab_table"), tabPyramidId = ns("tab_pyramid"),
        panelTableId = ns("panel_table"), panelPyramidId = ns("panel_pyramid")
      ))
    })
    shiny::observeEvent(input$tab_pyramid, {
      session$sendCustomMessage("switchDataTab", list(
        activeTab = "pyramid",
        tabTableId = ns("tab_table"), tabPyramidId = ns("tab_pyramid"),
        panelTableId = ns("panel_table"), panelPyramidId = ns("panel_pyramid")
      ))
    })

    # Q-sort pyramid viewer ----
    shiny::observeEvent(input$pyramid_prev, {
      shiny::req(rv$qdata, input$pyramid_participant)
      participants <- rv$qdata@participants
      current_idx <- which(participants == input$pyramid_participant)
      if (length(current_idx) == 1 && current_idx > 1) {
        shiny::updateSelectInput(session, "pyramid_participant",
                                 selected = participants[current_idx - 1])
      }
    })

    shiny::observeEvent(input$pyramid_next, {
      shiny::req(rv$qdata, input$pyramid_participant)
      participants <- rv$qdata@participants
      current_idx <- which(participants == input$pyramid_participant)
      if (length(current_idx) == 1 && current_idx < length(participants)) {
        shiny::updateSelectInput(session, "pyramid_participant",
                                 selected = participants[current_idx + 1])
      }
    })

    output$pyramid_counter <- shiny::renderText({
      shiny::req(rv$qdata, input$pyramid_participant)
      participants <- rv$qdata@participants
      current_idx <- which(participants == input$pyramid_participant)
      paste0(current_idx, " of ", length(participants))
    })

    pyramid_geometry <- shiny::reactive({
      shiny::req(rv$qdata)
      sorts <- rv$qdata@sorts
      val_range <- range(sorts, na.rm = TRUE)
      dist <- rv$qdata@distribution
      max_h <- if (length(dist) > 0) max(dist) else {
        max(apply(sorts, 1, function(row) max(table(row[!is.na(row)]))))
      }
      list(min_rank = val_range[1], max_rank = val_range[2],
           n_ranks = val_range[2] - val_range[1] + 1, max_h = max_h)
    })

    pyramid_colors <- shiny::reactive({
      geo <- pyramid_geometry()
      rank_range <- geo$min_rank:geo$max_rank
      colors <- (grDevices::colorRampPalette(ov2_ramp_anchors))(geo$n_ranks)
      names(colors) <- as.character(rank_range)
      colors
    })

    # Selected statement in the pyramid (reset when the participant changes)
    pyr_sel <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$pyr_sel, pyr_sel(as.integer(input$pyr_sel)))
    shiny::observeEvent(input$pyramid_participant, pyr_sel(NULL), ignoreInit = TRUE)

    tile_text_color <- function(col) {
      rgb_vals <- grDevices::col2rgb(col)
      lum <- (0.299 * rgb_vals[1] + 0.587 * rgb_vals[2] + 0.114 * rgb_vals[3]) / 255
      if (lum > 0.55) "#1a202c" else "#ffffff"
    }

    fmt_rank <- function(r) {
      if (r > 0) paste0("+", r) else sub("-", "−", as.character(r), fixed = TRUE)
    }

    # The pyramid itself is the statement reference: every tile is clickable
    output$pyramid_html <- shiny::renderUI({
      shiny::req(rv$data_loaded, rv$qdata, input$pyramid_participant)
      sorts <- rv$qdata@sorts
      participant <- input$pyramid_participant
      shiny::req(participant %in% rownames(sorts))
      p_sorts <- sorts[participant, ]
      geo <- pyramid_geometry()
      colors <- pyramid_colors()
      rank_range <- geo$min_rank:geo$max_rank
      stmts <- rv$qdata@statements
      sel <- pyr_sel()

      cols_ui <- lapply(rank_range, function(r) {
        idx <- which(!is.na(p_sorts) & p_sorts == r)
        bg <- colors[as.character(r)]
        fg <- tile_text_color(bg)
        tiles <- lapply(idx, function(i) {
          tip <- if (!is.null(stmts) && length(stmts) >= i) stmts[i] else paste("Statement", i)
          htmltools::tags$div(
            class = paste("ov2-qs-tile", if (identical(sel, i)) "sel"),
            style = paste0("background:", bg, ";color:", fg, ";"),
            title = tip,
            onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})",
                              ns("pyr_sel"), i),
            paste0("S", i)
          )
        })
        htmltools::div(
          class = "ov2-qs-col",
          htmltools::div(class = "ov2-qs-stack", tiles),
          htmltools::div(class = "ov2-qs-axis", fmt_rank(r))
        )
      })

      htmltools::div(
        class = "ov2-qs-wrap",
        style = sprintf("max-width: %dpx;", length(rank_range) * 82),
        htmltools::div(class = "ov2-qs-row", cols_ui),
        htmltools::div(
          class = "ov2-qs-ends",
          htmltools::div("Most disagree"),
          htmltools::div("Neutral"),
          htmltools::div("Most agree")
        )
      )
    })
    shiny::outputOptions(output, "pyramid_html", suspendWhenHidden = FALSE)

    # Reading pane: one statement at a time, on demand
    output$pyramid_pane <- shiny::renderUI({
      shiny::req(rv$data_loaded, rv$qdata, input$pyramid_participant)
      sel <- pyr_sel()
      if (is.null(sel)) {
        return(htmltools::div(class = "ov2-qs-pane empty",
                              "Click a tile to read its statement."))
      }
      sorts <- rv$qdata@sorts
      participant <- input$pyramid_participant
      shiny::req(participant %in% rownames(sorts))
      r <- sorts[participant, sel]
      colors <- pyramid_colors()
      bg <- colors[as.character(r)]
      fg <- tile_text_color(bg)
      stmts <- rv$qdata@statements
      txt <- if (!is.null(stmts) && length(stmts) >= sel) stmts[sel] else {
        "No statement text available for this dataset."
      }
      htmltools::div(
        class = "ov2-qs-pane",
        htmltools::div(
          class = "ov2-qs-pane-side",
          htmltools::span(class = "ov2-qs-pane-badge",
                          style = paste0("background:", bg, ";color:", fg, ";"),
                          paste0("S", sel)),
          htmltools::div(class = "ov2-qs-pane-rank", paste0("Rank ", fmt_rank(r)))
        ),
        htmltools::div(class = "ov2-qs-pane-text", txt)
      )
    })
    shiny::outputOptions(output, "pyramid_pane", suspendWhenHidden = FALSE)

    # Documentation ----
    shiny::observeEvent(input$action_docs, {
      shiny::showModal(shiny::modalDialog(
        title = "Documentation",
        size = "l",
        easyClose = TRUE,
        htmltools::div(
          class = "row",
          htmltools::div(
            class = "col-md-6",
            htmltools::h5("Getting Started"),
            htmltools::tags$ol(
              htmltools::tags$li("Import Q-sort data"),
              htmltools::tags$li("Review data quality"),
              htmltools::tags$li("Configure analysis"),
              htmltools::tags$li("Run factor extraction"),
              htmltools::tags$li("Interpret with crib sheets"),
              htmltools::tags$li("Export reports")
            )
          ),
          htmltools::div(
            class = "col-md-6",
            htmltools::h5("Key References"),
            htmltools::tags$ul(
              htmltools::tags$li("Brown (1980). Political Subjectivity"),
              htmltools::tags$li("Watts & Stenner (2012). Doing Q"),
              htmltools::tags$li("Zabala (2014). qmethod package"),
              htmltools::tags$li("Zabala & Pascual (2016). Bootstrap Q")
            )
          )
        ),
        footer = shiny::modalButton("Close")
      ))
    })
  })
}


#' The Lipset (1963) benchmark dataset as a QsortData object
#' @keywords internal
lipset_qdata <- function() {
  demo_sorts <- data.frame(
    S1  = c(-1, -1, 2, 3, -4, 1, 2, -2, 3),
    S2  = c(0, 0, -2, 1, -1, -3, 0, 2, 1),
    S3  = c(-2, -1, -2, -3, 3, 0, -2, 0, 0),
    S4  = c(0, -3, 4, -1, -1, 3, 1, -3, 1),
    S5  = c(-2, 2, -1, -1, 1, 3, 0, -4, -4),
    S6  = c(1, 3, 0, 3, 1, 4, 1, 4, -3),
    S7  = c(0, 1, -4, -3, 4, -2, -1, 0, 2),
    S8  = c(-1, 1, -3, -2, 2, 0, -3, -1, 2),
    S9  = c(0, -4, 1, 0, -4, -2, 0, -1, -2),
    S10 = c(-1, 0, -4, -4, 4, -2, -1, -1, 0),
    S11 = c(1, 2, -3, -1, 2, 1, 1, 1, 0),
    S12 = c(1, -1, 3, 0, 0, -1, -1, -1, -2),
    S13 = c(2, 4, 3, 3, -1, 1, 1, 1, 1),
    S14 = c(3, -1, 2, -2, 3, 0, 2, -1, 4),
    S15 = c(-1, 1, 0, -3, 0, -4, -4, 4, -1),
    S16 = c(-4, -3, -3, -4, 2, 3, 4, 0, -2),
    S17 = c(-3, 0, 2, -1, 0, -1, 3, 0, 2),
    S18 = c(-3, -2, -1, 2, -2, 0, 2, 1, -1),
    S19 = c(-1, -2, 1, 4, -2, 1, 0, -3, 2),
    S20 = c(-4, 0, 0, -2, 0, -1, 2, 1, 1),
    S21 = c(3, 3, 1, 4, -1, -2, -2, 2, 3),
    S22 = c(2, -2, -2, 1, 2, -3, -1, 2, -3),
    S23 = c(3, 1, 0, 1, 1, -1, -3, 3, -3),
    S24 = c(1, 0, 2, -2, -3, -4, -4, 3, -2),
    S25 = c(1, 2, -2, 0, -3, 2, 3, -3, 1),
    S26 = c(0, 1, 3, 1, -3, 2, -2, 1, 0),
    S27 = c(4, 2, 1, -1, 3, 0, 0, -4, -1),
    S28 = c(2, 3, -1, 2, 0, 4, 4, 3, -4),
    S29 = c(2, -1, 0, 0, 1, -1, -3, 0, 0),
    S30 = c(-3, -4, -1, 2, -2, 2, 1, -2, -1),
    S31 = c(-2, -2, -1, 1, 1, 1, -1, -2, 3),
    S32 = c(-2, -3, 4, 2, -2, 2, 3, -2, -1),
    S33 = c(4, 4, 1, 0, -1, -3, -2, 2, 4)
  )
  rownames(demo_sorts) <- c("US1", "US2", "US3", "US4", "JP5", "CA6", "UK7", "US8", "FR9")

  demo_statements <- c(
    "We accept improvements in the status and power of the lower classes without feeling morally offended.",
    "All men are expected to try to improve their position vis-a-vis others.",
    "Success in life by a previously deprived person is resented.",
    "Men can expect, and within limits receive, fair treatment according to their merits.",
    "Lower-class individuals and groups do not have revolutionary inclinations.",
    "Political goals and methods are relatively moderate in this country -- and even conservative.",
    "We believe that those born to high place in society should retain it, more or less automatically.",
    "We take the view that anyone with wealth deserves a place in any high society if he wishes it.",
    "We try to eliminate the privileged classes in this country -- socially and economically.",
    "We accept aristocratic-type titles and other honors.",
    "The government has its secrets, and this is generally accepted without much resentment.",
    "We place great emphasis on publicity in political matters: there should be no secrets.",
    "We are encouraged to think of ourselves as competing for success -- on our own merits.",
    "You can tell the social status of a person the moment he opens his mouth -- his manner of speech gives him away.",
    "We tend to take the law into our own hands, through mob action and the organization of vigilantes.",
    "We like to think of close ties to the Mother Country -- as Britain still is for many.",
    "We prefer companionship and a helping hand as required, freely given.",
    "There is some disdain for acquiring wealth for its own sake.",
    "High value is placed on activities aimed at protecting and promoting the standing of the 'underdog' in this country.",
    "We like the idea of a welfare state: each of us looks after his own best interests this way.",
    "We value the 'race for success.'",
    "Corrupt means of achieving success are accepted: for example, we put up with boss rule, and corruption in trade unions.",
    "It is an axiom that there is one law for the rich and another for the poor: the rich man can usually get his way in court.",
    "Lack of respect for the police, and law enforcement is evident.",
    "Trust in the police has sunk deeply into our national character: basically we like and trust the police.",
    "The worth of a man is judged by what he is -- not by whether he has gone to a private college, a state university, or to neither.",
    "We have deep respect, when all is said and done, for the elite -- the rich, the educated, the social elite.",
    "We are tolerant of popular opinion: we are essentially middle-of-the-roaders in politics. Extremes don't go down well with us.",
    "We still believe that the poor on earth will enjoy higher status in after-life.",
    "There is considerable respect for civil liberties and minority rights in this country.",
    "Virtue tends to be its own reward in this country, for one's self and one's children.",
    "We believe that the position of depressed classes must be raised, that they are morally as good as any others.",
    "Here, the emphasis is on 'getting ahead.'"
  )

  qsort_data(
    sorts = demo_sorts,
    statements = demo_statements,
    distribution = c(2, 3, 4, 5, 5, 5, 4, 3, 2),
    source = "Lipset (1963) Benchmark Dataset",
    validate = FALSE
  )
}


#' The Grizzly Bear Reintroduction dataset
#'
#' Sorts and full statement texts are read from the packaged workbook
#' (verified identical to bayesqm::grizzly_sorts, which remains the
#' fallback if the workbook is unavailable).
#' @keywords internal
load_grizzly_qdata <- function() {
  file_path <- system.file("Datasets", "Grizzly bear dataset.xlsx",
                           package = "canhrQsort")
  if (nzchar(file_path) && file.exists(file_path)) {
    raw <- readxl::read_excel(file_path)
    n_part <- sum(grepl("^qsort", names(raw)))
    sorts <- t(as.matrix(raw[, paste0("qsort", seq_len(n_part))]))
    stmts <- if ("statement" %in% names(raw)) as.character(raw$statement) else NULL
    return(qsort_data(
      sorts = sorts, statements = stmts,
      distribution = c(1, 2, 3, 5, 6, 7, 6, 5, 3, 2, 1),
      source = "Grizzly Bear Reintroduction (Easter et al., 2025)"
    ))
  }

  gz <- bayesqm::grizzly_sorts
  sorts <- t(as.matrix(gz$Y))
  stmts <- as.character(gz$statements)
  if (is.null(stmts) || all(grepl("^[0-9]+$", stmts))) {
    stmts <- paste("Statement", seq_len(nrow(gz$Y)))
  }
  qsort_data(
    sorts = sorts, statements = stmts, distribution = gz$distribution,
    source = "Grizzly Bear Reintroduction (Easter et al., 2025)"
  )
}


#' Load Session File Helper
#' @keywords internal
load_session_file <- function(file_path, rv, session, parent_session) {
  tryCatch({
    data <- readRDS(file_path)

    # Reset state before restoring
    rv$qdata <- NULL
    rv$data_loaded <- FALSE
    rv$results <- NULL
    rv$analysis_complete <- FALSE
    rv$bootstrap <- NULL
    rv$bayesian <- NULL
    rv$bayesian_results <- NULL

    qdata <- data$qdata %||% NULL

    if (is.null(qdata) && !is.null(data$results) && inherits(data$results, "QsortResults")) {
      qdata <- data$results@data
    }

    if (!is.null(qdata)) {
      rv$qdata <- qdata
      rv$data_loaded <- TRUE
      rv$validation <- validate_qsort(qdata)
    }

    if (!is.null(data$results) && inherits(data$results, "QsortResults")) {
      rv$results <- data$results
      rv$analysis_complete <- TRUE
    }

    if (!is.null(data$bootstrap)) {
      rv$bootstrap <- data$bootstrap
    }

    # Bayesian state is a bayesqm bundle (older Stan-era sessions are skipped)
    is_bq_bundle <- is.list(data$bayesian) &&
      identical(data$bayesian$engine, "bayesqm") && !is.null(data$bayesian$fit)
    if (is_bq_bundle) {
      rv$bayesian <- data$bayesian
    } else if (!is.null(data$bayesian)) {
      session$sendCustomMessage("showToast", list(
        message = "This session's Bayesian results came from the old Stan engine and were skipped; re-run the Bayesian analysis.",
        type = "warning"))
    }

    if (!is.null(data$bayesian_results) && inherits(data$bayesian_results, "QsortResults")) {
      rv$bayesian_results <- data$bayesian_results
    } else if (!is.null(rv$bayesian) && !is.null(rv$qdata)) {
      rv$bayesian_results <- tryCatch(
        bayesqm_to_results(rv$bayesian, rv$qdata),
        error = function(e) NULL
      )
    }

    session$sendCustomMessage("showToast", list(message = "Session loaded", type = "success"))

    if (!is.null(data$results)) {
      bslib::nav_select("main_nav", "analyze", session = parent_session)
    } else if (!is.null(data$bayesian)) {
      bslib::nav_select("main_nav", "bayesian", session = parent_session)
    } else {
      bslib::nav_select("main_nav", "home", session = parent_session)
    }
  }, error = function(e) {
    session$sendCustomMessage("showToast", list(message = paste("Error:", e$message), type = "error"))
  })
}
