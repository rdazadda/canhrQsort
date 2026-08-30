#' @title Visualization Page Module
#' @description The Visualization page: every publication figure on one page,
#' the Frequentist and Bayesian sets picked by the same seg the analysis
#' pages use. Underline tabs group the figures; each figure block carries
#' its name, its own control where it has one, and an Export button; the
#' plots title and caption themselves. Static skeleton with client-side tab
#' switching; plots render lazily when their panel first shows.
#' @name app-page-visualize
NULL

#' Visualization Page UI
#'
#' @param id Module ID
#' @return Shiny UI elements
#' @keywords internal
visualize_page_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::div(
    id = ns("vz_page"),
    class = "fr-page vz-page",

    htmltools::div(
      class = "bq2-bar",
      htmltools::div(
        class = "bq2-group",
        htmltools::div(class = "bq2-label", "Figures"),
        shiny::uiOutput(ns("vz_seg"))
      ),
      htmltools::div(class = "bq2-bar-spacer"),
      htmltools::div(
        class = "bq2-bar-actions",
        shiny::uiOutput(ns("vz_download_area"), inline = TRUE)
      )
    ),

    shiny::uiOutput(ns("vz_content"))
  )
}


#' Visualization Page Server
#'
#' @param id Module ID
#' @param rv Reactive values from parent
#' @param parent_session Parent session for navigation
#' @keywords internal
visualize_page_server <- function(id, rv, parent_session) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Side: Frequentist | Bayesian ----

    vz_side <- shiny::reactiveVal("fr")

    has_bq_fit <- function() {
      b <- rv$bayesian
      is.list(b) && identical(b$engine, "bayesqm") && !is.null(b$fit)
    }

    output$vz_seg <- shiny::renderUI({
      side <- vz_side()
      htmltools::div(
        class = "bq2-seg",
        shiny::actionButton(
          ns("vz_seg_fr"), "Frequentist",
          class = paste("btn bq2-seg-btn", if (side == "fr") "active")
        ),
        shiny::actionButton(
          ns("vz_seg_bq"), "Bayesian",
          class = paste("btn bq2-seg-btn", if (side == "bq") "active")
        )
      )
    })

    shiny::observeEvent(input$vz_seg_fr, vz_side("fr"))
    shiny::observeEvent(input$vz_seg_bq, vz_side("bq"))

    # The seg picks the method, so the frequentist figures always draw from
    # the frequentist solution; the old blended data source is gone
    active_results <- shiny::reactive({
      if (isTRUE(rv$analysis_complete)) rv$results else NULL
    })

    output$vz_download_area <- shiny::renderUI({
      shiny::req(rv$qdata)
      if (is.null(rv$results) && !has_bq_fit()) return(NULL)
      shiny::downloadButton(ns("download_all"), "Download all",
                            class = "btn-ov2-blue bq2-run", icon = NULL)
    })


    # Option sources ----

    factor_choices <- shiny::reactive({
      results <- active_results()
      n <- if (!is.null(results)) results@n_factors else 3
      stats::setNames(seq_len(n), paste("Factor", seq_len(n)))
    })

    bq_factor_choices <- shiny::reactive({
      k <- rv$bayesian$K %||% 2
      stats::setNames(seq_len(k), paste("Factor", seq_len(k)))
    })

    bq_pair_choices <- shiny::reactive({
      k <- rv$bayesian$K %||% 2
      if (k < 2) return(list("F1" = 1))
      pairs <- utils::combn(k, 2)
      stats::setNames(seq_len(ncol(pairs)),
                      apply(pairs, 2, function(p) sprintf("F%d vs F%d", p[1], p[2])))
    })

    statement_choices <- shiny::reactive({
      shiny::req(rv$qdata)
      stmts <- rv$qdata@statements
      labs <- sprintf("S%d · %s", seq_along(stmts),
                      substr(as.character(stmts), 1, 44))
      stats::setNames(seq_along(stmts), labs)
    })

    flow_attr_choices <- shiny::reactive({
      shiny::req(rv$qdata, has_participant_attributes(rv$qdata))
      groups <- attribute_groups(rv$qdata)
      fg <- names(groups)[vapply(groups, is.factor, logical(1))]
      stats::setNames(fg, vapply(fg, pr_attr_label, character(1)))
    })


    # Figure-block builders ----

    vz_ctl <- function(inputId, label, choices, default = NULL) {
      htmltools::div(
        htmltools::div(class = "bq2-label", label),
        shiny::selectInput(
          ns(inputId), NULL, choices = choices,
          selected = shiny::isolate(input[[inputId]]) %||% default,
          width = "170px", selectize = FALSE
        )
      )
    }

    vz_block <- function(title, out_id, h, dl_id, controls = NULL) {
      htmltools::div(
        class = "vz-block",
        htmltools::div(
          class = "vz-figrow",
          htmltools::tags$b(title),
          htmltools::div(style = "flex: 1;"),
          controls,
          shiny::downloadButton(ns(dl_id), "Export",
                                class = "btn-bq2-quiet", icon = NULL)
        ),
        shiny::plotOutput(ns(out_id), height = paste0(h, "px"))
      )
    }

    vz_line <- function(text) {
      htmltools::div(class = "bq2-tab-note vz-gate", text)
    }

    vz_tab_link <- function(id, label, count, tabs, panels, active = FALSE) {
      panel_id <- panels[match(id, tabs)]
      htmltools::tags$a(
        id = ns(id), href = "#",
        class = paste(c("ov2-tab", if (active) "active"), collapse = " "),
        onclick = sprintf(
          "canhrPrTab(this, %s, %s); Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;",
          pr_js_array(vapply(tabs, ns, character(1))),
          pr_js_array(vapply(panels, ns, character(1))),
          ns("vz_seen"), panel_id),
        label,
        htmltools::span(class = "vz-count", count)
      )
    }

    # Plots start suspended; the first visit to a tab flips its plots to
    # eager so they render behind the client-side switch and stay live
    vz_panel_outputs <- list(
      vz_fpan_factors = c("factor_array_grid", "scree_plot"),
      vz_fpan_stmts = c("comparison_plot", "ranking_plot"),
      vz_fpan_interp = c("crib_sheet_panel_plot", "factor_comparison_plot"),
      vz_fpan_prio = c("priorities_ranking_plot", "panel_composition_plot",
                       "priorities_flow_plot"),
      vz_bpan_solution = c("bq_loadings_plot", "bq_flags_plot",
                           "bq_zscores_plot", "bq_array_plot"),
      vz_bpan_stmts = c("bq_contrasts_plot", "bq_statement_plot"),
      vz_bpan_diag = c("bq_convergence_plot", "bq_ppc_plot",
                       "bq_person_plot", "bq_choice_plot")
    )
    vz_woken <- new.env(parent = emptyenv())
    shiny::observeEvent(input$vz_seen, {
      key <- input$vz_seen
      shiny::req(is.character(key), length(key) == 1)
      if (isTRUE(vz_woken[[key]])) return()
      vz_woken[[key]] <- TRUE
      for (o in vz_panel_outputs[[key]] %||% character(0)) {
        shiny::outputOptions(output, o, suspendWhenHidden = FALSE)
      }
    })


    # Page content: one line, or the active side ----

    vz_fr_tabs <- c("vz_ftab_factors", "vz_ftab_stmts", "vz_ftab_interp",
                    "vz_ftab_prio")
    vz_fr_panels <- c("vz_fpan_factors", "vz_fpan_stmts", "vz_fpan_interp",
                      "vz_fpan_prio")
    vz_bq_tabs <- c("vz_btab_solution", "vz_btab_stmts", "vz_btab_diag")
    vz_bq_panels <- c("vz_bpan_solution", "vz_bpan_stmts", "vz_bpan_diag")

    output$vz_content <- shiny::renderUI({
      if (!rv$data_loaded || is.null(shiny::isolate(rv$qdata))) {
        return(htmltools::p(class = "bq2-plan",
                            "Import a dataset on the Overview page to begin."))
      }

      if (identical(vz_side(), "bq")) {
        if (!has_bq_fit()) {
          return(htmltools::p(class = "bq2-plan",
                              "Run the Bayesian analysis to unlock these figures."))
        }
        return(htmltools::div(
          class = "ov2-card pr-card",
          shiny::uiOutput(ns("vz_bq_tabbar")),
          htmltools::div(
            class = "ov2-card-body",
            htmltools::div(id = ns("vz_bpan_solution"),
                           shiny::uiOutput(ns("vz_bq_solution_body"))),
            htmltools::div(id = ns("vz_bpan_stmts"), style = "display: none;",
                           shiny::uiOutput(ns("vz_bq_stmts_body"))),
            htmltools::div(id = ns("vz_bpan_diag"), style = "display: none;",
                           shiny::uiOutput(ns("vz_bq_diag_body")))
          )
        ))
      }

      htmltools::div(
        class = "ov2-card pr-card",
        shiny::uiOutput(ns("vz_fr_tabbar")),
        htmltools::div(
          class = "ov2-card-body",
          htmltools::div(id = ns("vz_fpan_factors"),
                         shiny::uiOutput(ns("vz_fr_factors_body"))),
          htmltools::div(id = ns("vz_fpan_stmts"), style = "display: none;",
                         shiny::uiOutput(ns("vz_fr_stmts_body"))),
          htmltools::div(id = ns("vz_fpan_interp"), style = "display: none;",
                         shiny::uiOutput(ns("vz_fr_interp_body"))),
          htmltools::div(id = ns("vz_fpan_prio"), style = "display: none;",
                         shiny::uiOutput(ns("vz_fr_prio_body")))
        )
      )
    })


    # Tab bars (client-side switching, default set server-side) ----

    output$vz_fr_tabbar <- shiny::renderUI({
      rv$fr_run_id
      ran <- isTRUE(rv$analysis_complete)
      active_id <- if (ran) "vz_ftab_factors" else "vz_ftab_prio"
      active_panel <- if (ran) "vz_fpan_factors" else "vz_fpan_prio"

      links <- list(
        vz_tab_link("vz_ftab_factors", "Factors", 2, vz_fr_tabs, vz_fr_panels,
                    active_id == "vz_ftab_factors"),
        vz_tab_link("vz_ftab_stmts", "Statements", 2, vz_fr_tabs, vz_fr_panels,
                    active_id == "vz_ftab_stmts"),
        vz_tab_link("vz_ftab_interp", "Interpretation", 2, vz_fr_tabs, vz_fr_panels,
                    active_id == "vz_ftab_interp"),
        vz_tab_link("vz_ftab_prio", "Priorities", 3, vz_fr_tabs, vz_fr_panels,
                    active_id == "vz_ftab_prio")
      )

      htmltools::tagList(
        htmltools::div(class = "ov2-tabbar", links),
        htmltools::tags$script(htmltools::HTML(sprintf(
          "var t = document.getElementById('%s'); if (t && window.canhrPrTab) canhrPrTab(t, %s, %s);",
          ns(active_id),
          pr_js_array(vapply(vz_fr_tabs, ns, character(1))),
          pr_js_array(vapply(vz_fr_panels, ns, character(1)))
        )))
      )
    })

    output$vz_bq_tabbar <- shiny::renderUI({
      links <- list(
        vz_tab_link("vz_btab_solution", "Solution", 4, vz_bq_tabs, vz_bq_panels, TRUE),
        vz_tab_link("vz_btab_stmts", "Statements", 2, vz_bq_tabs, vz_bq_panels),
        vz_tab_link("vz_btab_diag", "Diagnostics", 4, vz_bq_tabs, vz_bq_panels)
      )
      htmltools::tagList(
        htmltools::div(class = "ov2-tabbar", links),
        htmltools::tags$script(htmltools::HTML(sprintf(
          "var t = document.getElementById('%s'); if (t && window.canhrPrTab) canhrPrTab(t, %s, %s);",
          ns("vz_btab_solution"),
          pr_js_array(vapply(vz_bq_tabs, ns, character(1))),
          pr_js_array(vapply(vz_bq_panels, ns, character(1)))
        )))
      )
    })


    # Frequentist tab bodies ----

    vz_fr_gate <- function() {
      vz_line("Extract factors on the Frequentist page; Priorities figures work now.")
    }

    output$vz_fr_factors_body <- shiny::renderUI({
      if (!isTRUE(rv$analysis_complete)) return(vz_fr_gate())
      htmltools::tagList(
        vz_block("Factor Array", "factor_array_grid", 680, "download_factor_array",
                 vz_ctl("factor_array_factor", "Factor", factor_choices(), 1)),
        vz_block("Scree", "scree_plot", 500, "download_scree_plot")
      )
    })

    output$vz_fr_stmts_body <- shiny::renderUI({
      if (!isTRUE(rv$analysis_complete)) return(vz_fr_gate())
      htmltools::tagList(
        vz_block("Z-Score Comparison", "comparison_plot", 720,
                 "download_comparison_plot"),
        vz_block("Statement Rankings", "ranking_plot", 720,
                 "download_ranking_plot",
                 vz_ctl("ranking_factor", "Factor", factor_choices(), 1))
      )
    })

    output$vz_fr_interp_body <- shiny::renderUI({
      if (!isTRUE(rv$analysis_complete)) return(vz_fr_gate())
      htmltools::tagList(
        vz_block("Crib Sheets", "crib_sheet_panel_plot", 760,
                 "download_crib_panel"),
        vz_block("Factor Comparison", "factor_comparison_plot", 900,
                 "download_factor_comparison",
                 vz_ctl("comparison_sort", "Sort by",
                        list("Sort by F1" = 1, "Sort by F2" = 2,
                             "Sort by F3" = 3, "Sort by Variance" = "variance"),
                        1))
      )
    })

    output$vz_fr_prio_body <- shiny::renderUI({
      shiny::req(rv$qdata)

      if (!has_participant_attributes(rv$qdata)) {
        return(htmltools::tagList(
          vz_block("Priorities Ranking", "priorities_ranking_plot", 640,
                   "download_priorities_ranking"),
          htmltools::div(
            class = "vz-block",
            htmltools::div(
              class = "vz-figrow",
              htmltools::tags$b("Panel Composition · Priorities Flow")
            ),
            vz_line("Add demographics on the Priorities view.")
          )
        ))
      }

      k_attrs <- length(attribute_groups(rv$qdata))
      comp_h <- 80 + 380 * ceiling(k_attrs / 2)
      fg <- flow_attr_choices()

      htmltools::tagList(
        vz_block("Priorities Ranking", "priorities_ranking_plot", 640,
                 "download_priorities_ranking"),
        vz_block("Panel Composition", "panel_composition_plot", comp_h,
                 "download_panel_composition"),
        vz_block("Priorities Flow", "priorities_flow_plot", 560,
                 "download_priorities_flow",
                 htmltools::tagList(
                   htmltools::div(
                     htmltools::div(class = "bq2-label", "Split by"),
                     shiny::checkboxGroupInput(
                       ns("flow_attrs"), NULL, choices = fg,
                       selected = shiny::isolate(input$flow_attrs) %||%
                         unname(fg),
                       inline = TRUE
                     )
                   ),
                   htmltools::div(
                     htmltools::div(class = "bq2-label", "Top statements"),
                     shiny::numericInput(
                       ns("flow_n"), NULL,
                       value = shiny::isolate(input$flow_n) %||% 5,
                       min = 3, max = 10, width = "72px"
                     )
                   )
                 ))
      )
    })


    # Bayesian tab bodies ----

    output$vz_bq_solution_body <- shiny::renderUI({
      shiny::req(has_bq_fit())
      htmltools::tagList(
        vz_block("Factor Array", "bq_array_plot", 680, "download_bq_array",
                 vz_ctl("bq_array_factor", "Factor", bq_factor_choices(), 1)),
        vz_block("Loading Intervals", "bq_loadings_plot", 520,
                 "download_bq_loadings"),
        vz_block("Flag Probabilities", "bq_flags_plot", 520,
                 "download_bq_flags"),
        vz_block("Statement Scores", "bq_zscores_plot", 640,
                 "download_bq_zscores",
                 vz_ctl("bq_zscore_order", "Order by",
                        list("Divergence between factors" = "divergence",
                             "Score" = "score"),
                        "divergence"))
      )
    })

    output$vz_bq_stmts_body <- shiny::renderUI({
      shiny::req(has_bq_fit())
      htmltools::tagList(
        if (is.null(rv$bayesian$tables$qdc)) {
          htmltools::div(
            class = "vz-block",
            htmltools::div(class = "vz-figrow",
                           htmltools::tags$b("Statement Contrasts")),
            vz_line("Statement contrasts compare factor pairs; fit K ≥ 2.")
          )
        } else {
          vz_block("Statement Contrasts", "bq_contrasts_plot", 620,
                   "download_bq_contrasts",
                   vz_ctl("bq_pair", "Factor pair", bq_pair_choices(), 1))
        },
        vz_block("Statement Detail", "bq_statement_plot", 420,
                 "download_bq_statement",
                 vz_ctl("bq_statement_pick", "Statement", statement_choices(), 1))
      )
    })

    output$vz_bq_diag_body <- shiny::renderUI({
      shiny::req(has_bq_fit())
      htmltools::tagList(
        vz_block("Convergence & Alignment", "bq_convergence_plot", 520,
                 "download_bq_convergence"),
        vz_block("Posterior-Predictive Checks", "bq_ppc_plot", 440,
                 "download_bq_ppc"),
        vz_block("Person Check", "bq_person_plot", 500, "download_bq_person"),
        if (is.null(rv$bayesian$ladder)) {
          htmltools::div(
            class = "vz-block",
            htmltools::div(class = "vz-figrow",
                           htmltools::tags$b("Choice of K")),
            vz_line("Comes from a ladder fit; run the Bayesian analysis in ladder mode.")
          )
        } else {
          vz_block("Choice of K", "bq_choice_plot", 460, "download_bq_choice")
        }
      )
    })

    # Tab bodies and bars live inside hidden panels; without this they stay
    # suspended and never render on reveal. The plots themselves stay lazy.
    for (o in c("vz_seg", "vz_download_area", "vz_fr_tabbar", "vz_bq_tabbar",
                "vz_fr_factors_body", "vz_fr_stmts_body", "vz_fr_interp_body",
                "vz_fr_prio_body",
                "vz_bq_solution_body", "vz_bq_stmts_body",
                "vz_bq_diag_body")) {
      shiny::outputOptions(output, o, suspendWhenHidden = FALSE)
    }

    # Bayesian Visualization Plots (bayesqm native figures) ----

    # The dashboard's palette for every bayesqm figure, set once per session
    bayesqm::bayesqm_set_colors(list(
      dark = "#236192", accent = "#DF6A2E", grey = "grey40",
      gridgrey = "grey80", fill = "#D9E4EE",
      qual = c("#236192", "#FFB800", "#DF6A2E", "#17A589",
               "#6C3483", "#0E6655", "#B03A2E", "#5D6D7E")
    ))

    bq_q <- function() rv$bayesian$q %||% 0.05

    save_base_plot <- function(file, width, height, draw, res = 150) {
      grDevices::png(file, width = width, height = height, units = "in", res = res)
      on.exit(grDevices::dev.off(), add = TRUE)
      draw()
    }

    output$bq_loadings_plot <- shiny::renderPlot({
      shiny::req(has_bq_fit())
      bayesqm::plot_loading_posterior(rv$bayesian$fit, q = bq_q())
    }, res = 96)

    output$bq_flags_plot <- shiny::renderPlot({
      shiny::req(has_bq_fit())
      bayesqm::plot_flags(rv$bayesian$fit, q = bq_q())
    }, res = 96)

    output$bq_zscores_plot <- shiny::renderPlot({
      shiny::req(has_bq_fit())
      bayesqm::plot_zscores(rv$bayesian$fit,
                            order_by = input$bq_zscore_order %||% "divergence",
                            q = bq_q())
    }, res = 96)

    output$bq_contrasts_plot <- shiny::renderPlot({
      shiny::req(has_bq_fit(), !is.null(rv$bayesian$tables$qdc))
      bayesqm::plot_contrasts(rv$bayesian$fit,
                              pair = as.integer(input$bq_pair %||% 1),
                              q = bq_q())
    }, res = 96)

    output$bq_statement_plot <- shiny::renderPlot({
      shiny::req(has_bq_fit())
      p <- create_plot_bq_statement(
        rv$bayesian$fit, rv$qdata,
        statement = as.integer(input$bq_statement_pick %||% 1))
      if (!is.null(p)) print(p)
    }, res = 96)

    output$bq_array_plot <- shiny::renderPlot({
      shiny::req(has_bq_fit())
      p <- create_plot_bq_factor_array(
        rv$bayesian$fit, rv$qdata,
        factor_num = as.integer(input$bq_array_factor %||% 1))
      if (!is.null(p)) print(p)
    }, res = 96)

    output$bq_convergence_plot <- shiny::renderPlot({
      shiny::req(has_bq_fit())
      bayesqm::plot_convergence(rv$bayesian$fit)
    }, res = 96)

    output$bq_ppc_plot <- shiny::renderPlot({
      shiny::req(has_bq_fit())
      bayesqm::plot_ppc(rv$bayesian$tables$checks)
    }, res = 96)

    output$bq_person_plot <- shiny::renderPlot({
      shiny::req(has_bq_fit())
      bayesqm::plot_person_check(rv$bayesian$tables$persons)
    }, res = 96)

    output$bq_choice_plot <- shiny::renderPlot({
      shiny::req(has_bq_fit(), !is.null(rv$bayesian$ladder))
      bayesqm::plot_choice_k(rv$bayesian$ladder$selection)
    }, res = 96)

    # Bayesian download handlers, through the package's own saver
    output$download_bq_loadings <- shiny::downloadHandler(
      filename = function() paste0("bayesqm_loadings_", Sys.Date(), ".png"),
      content = function(file) {
        shiny::req(has_bq_fit())
        bayesqm::save_bayesqm_plot(
          file, bayesqm::plot_loading_posterior(rv$bayesian$fit, q = bq_q()),
          width = 10, height = 8)
      }
    )

    output$download_bq_flags <- shiny::downloadHandler(
      filename = function() paste0("bayesqm_flags_", Sys.Date(), ".png"),
      content = function(file) {
        shiny::req(has_bq_fit())
        bayesqm::save_bayesqm_plot(
          file, bayesqm::plot_flags(rv$bayesian$fit, q = bq_q()),
          width = 10, height = 8)
      }
    )

    output$download_bq_zscores <- shiny::downloadHandler(
      filename = function() paste0("bayesqm_statement_scores_", Sys.Date(), ".png"),
      content = function(file) {
        shiny::req(has_bq_fit())
        bayesqm::save_bayesqm_plot(
          file, bayesqm::plot_zscores(rv$bayesian$fit,
                                      order_by = input$bq_zscore_order %||% "divergence",
                                      q = bq_q()),
          width = 10, height = 11)
      }
    )

    output$download_bq_contrasts <- shiny::downloadHandler(
      filename = function() paste0("bayesqm_contrasts_", Sys.Date(), ".png"),
      content = function(file) {
        shiny::req(has_bq_fit(), !is.null(rv$bayesian$tables$qdc))
        bayesqm::save_bayesqm_plot(
          file, bayesqm::plot_contrasts(rv$bayesian$fit,
                                        pair = as.integer(input$bq_pair %||% 1),
                                        q = bq_q()),
          width = 10, height = 11)
      }
    )

    output$download_bq_statement <- shiny::downloadHandler(
      filename = function() paste0("bayesqm_statement_", input$bq_statement_pick %||% 1,
                                   "_", Sys.Date(), ".png"),
      content = function(file) {
        shiny::req(has_bq_fit())
        p <- create_plot_bq_statement(
          rv$bayesian$fit, rv$qdata,
          statement = as.integer(input$bq_statement_pick %||% 1))
        ggplot2::ggsave(file, p, width = 10, height = 4.6, dpi = 300,
                        bg = "white")
      }
    )

    output$download_bq_array <- shiny::downloadHandler(
      filename = function() paste0("bayesqm_factor_array_F", input$bq_array_factor %||% 1,
                                   "_", Sys.Date(), ".png"),
      content = function(file) {
        shiny::req(has_bq_fit(), rv$qdata)
        p <- create_plot_bq_factor_array(
          rv$bayesian$fit, rv$qdata,
          factor_num = as.integer(input$bq_array_factor %||% 1))
        n_cols <- length(rv$qdata@distribution)
        max_height <- max(rv$qdata@distribution)
        ggplot2::ggsave(file, p, width = max(15, n_cols * 1.45),
                        height = max_height * 1.15 + 2.4, dpi = 150,
                        bg = "white")
      }
    )

    output$download_bq_convergence <- shiny::downloadHandler(
      filename = function() paste0("bayesqm_convergence_", Sys.Date(), ".png"),
      content = function(file) {
        shiny::req(has_bq_fit())
        save_base_plot(file, 10, 8, function() bayesqm::plot_convergence(rv$bayesian$fit))
      }
    )

    output$download_bq_ppc <- shiny::downloadHandler(
      filename = function() paste0("bayesqm_ppc_", Sys.Date(), ".png"),
      content = function(file) {
        shiny::req(has_bq_fit())
        save_base_plot(file, 11, 5.5, function() bayesqm::plot_ppc(rv$bayesian$tables$checks))
      }
    )

    output$download_bq_person <- shiny::downloadHandler(
      filename = function() paste0("bayesqm_person_check_", Sys.Date(), ".png"),
      content = function(file) {
        shiny::req(has_bq_fit())
        save_base_plot(file, 9, 7, function() bayesqm::plot_person_check(rv$bayesian$tables$persons))
      }
    )

    output$download_bq_choice <- shiny::downloadHandler(
      filename = function() paste0("bayesqm_choice_of_k_", Sys.Date(), ".png"),
      content = function(file) {
        shiny::req(has_bq_fit(), !is.null(rv$bayesian$ladder))
        save_base_plot(file, 10, 6, function() bayesqm::plot_choice_k(rv$bayesian$ladder$selection))
      }
    )


    # Standard Visualization Plots ----

    output$comparison_plot <- shiny::renderPlot({
      results <- active_results()
      shiny::req(results)
      print(create_plot_zscore(results, rv$qdata))
    }, res = 96)

    output$ranking_plot <- shiny::renderPlot({
      results <- active_results()
      shiny::req(results)
      factor_num <- as.integer(input$ranking_factor %||% 1)
      print(create_plot_ranking(results, rv$qdata, factor_num))
    }, res = 96)

    output$scree_plot <- shiny::renderPlot({
      results <- active_results()
      shiny::req(results)
      p <- create_plot_scree(results, rv$qdata)
      if (is.null(p)) return(NULL)
      print(p)
    }, res = 96)

    output$factor_array_grid <- shiny::renderPlot({
      results <- active_results()
      shiny::req(results)
      factor_num <- as.integer(input$factor_array_factor %||% 1)
      p <- create_plot_factor_array(results, rv$qdata, factor_num)
      if (!is.null(p)) print(p)
    }, res = 96)

    output$factor_comparison_plot <- shiny::renderPlot({
      results <- active_results()
      shiny::req(results)

      sort_by <- input$comparison_sort %||% 1
      if (sort_by != "variance") {
        sort_by <- as.integer(sort_by)
      }

      tryCatch({
        print(plot_factor_comparison(results, sort_by = sort_by, n_show = NULL))
      }, error = function(e) {
        plot.new(); graphics::text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 1.2)
      })
    }, res = 96)

    output$crib_sheet_panel_plot <- shiny::renderPlot({
      results <- active_results()
      shiny::req(results)
      tryCatch({
        print(plot_crib_sheet_panel(results, n_poles = 3, text_width = 30))
      }, error = function(e) {
        plot.new(); graphics::text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 1.2)
      })
    }, res = 96)

    # Stability and Priorities figures ----

    output$priorities_ranking_plot <- shiny::renderPlot({
      shiny::req(rv$qdata)
      p <- create_plot_priorities_ranking(rv$qdata)
      if (!is.null(p)) print(p)
    }, res = 96)

    viz_flow_gg <- function() {
      qd <- rv$qdata
      groups <- attribute_groups(qd)
      fg <- names(groups)[vapply(groups, is.factor, logical(1))]
      sel <- intersect(input$flow_attrs %||% fg, fg)
      if (length(sel) == 0) sel <- fg
      n <- max(3, min(10, input$flow_n %||% 5))
      pr <- compute_priorities(qd)
      top_tbl <- priorities_top_n(qd, groups = groups[sel], n = n)
      plot_priorities_flow(
        top_tbl,
        statements = stats::setNames(qd@statements,
                                     paste0("S", seq_along(qd@statements))),
        overall_means = stats::setNames(pr$table$mean, pr$table$stmt),
        gates = pr$gates,
        palette = "ramp",
        show_legend = FALSE
      )
    }

    output$priorities_flow_plot <- shiny::renderPlot({
      shiny::req(rv$qdata, has_participant_attributes(rv$qdata))
      print(viz_flow_gg())
    }, res = 96)

    output$panel_composition_plot <- shiny::renderPlot({
      shiny::req(rv$qdata, has_participant_attributes(rv$qdata))
      g <- create_plot_panel_composition(rv$qdata)
      shiny::req(!is.null(g))
      grid::grid.draw(g)
    }, res = 96)


    # Download handlers (standard charts) ----

    output$download_comparison_plot <- shiny::downloadHandler(
      filename = function() paste0("zscore_comparison_", Sys.Date(), ".png"),
      content = function(file) {
        results <- active_results()
        shiny::req(results)
        p <- create_plot_zscore(results, rv$qdata)
        n_statements <- nrow(results@factor_scores)
        plot_height <- max(8, n_statements * 0.22)
        ggplot2::ggsave(file, p, width = 12, height = plot_height, dpi = 150, bg = "white")
      }
    )

    output$download_ranking_plot <- shiny::downloadHandler(
      filename = function() paste0("statement_ranking_", Sys.Date(), ".png"),
      content = function(file) {
        results <- active_results()
        shiny::req(results)
        factor_num <- as.integer(input$ranking_factor %||% 1)
        p <- create_plot_ranking(results, rv$qdata, factor_num)
        n_statements <- nrow(results@factor_scores)
        ggplot2::ggsave(file, p, width = 12, height = max(8, n_statements * 0.25), dpi = 150, bg = "white")
      }
    )

    output$download_scree_plot <- shiny::downloadHandler(
      filename = function() paste0("scree_plot_", Sys.Date(), ".png"),
      content = function(file) {
        results <- active_results()
        shiny::req(results)
        p <- create_plot_scree(results, rv$qdata)
        if (!is.null(p)) {
          ggplot2::ggsave(file, p, width = 10, height = 7, dpi = 150, bg = "white")
        }
      }
    )

    output$download_factor_array <- shiny::downloadHandler(
      filename = function() paste0("factor_array_", Sys.Date(), ".png"),
      content = function(file) {
        results <- active_results()
        shiny::req(results)
        factor_num <- as.integer(input$factor_array_factor %||% 1)
        p <- create_plot_factor_array(results, rv$qdata, factor_num)
        if (!is.null(p)) {
          n_cols <- length(rv$qdata@distribution)
          max_height <- max(rv$qdata@distribution)
          ggplot2::ggsave(file, p, width = max(15, n_cols * 1.45), height = max_height * 1.15 + 2.4, dpi = 150, bg = "white")
        }
      }
    )

    output$download_factor_comparison <- shiny::downloadHandler(
      filename = function() paste0("factor_comparison_", Sys.Date(), ".png"),
      content = function(file) {
        results <- active_results()
        shiny::req(results)
        sort_by <- input$comparison_sort %||% 1
        if (sort_by != "variance") {
          sort_by <- as.integer(sort_by)
        }
        p <- plot_factor_comparison(results, sort_by = sort_by, n_show = NULL)
        ggplot2::ggsave(file, p, width = 10, height = 14, dpi = 300, bg = "white")
      }
    )

    output$download_crib_panel <- shiny::downloadHandler(
      filename = function() paste0("crib_sheet_panel_", Sys.Date(), ".png"),
      content = function(file) {
        results <- active_results()
        shiny::req(results)
        p <- plot_crib_sheet_panel(results, n_poles = 3, text_width = 35)
        ggplot2::ggsave(file, p, width = 16, height = 10, dpi = 300, bg = "white")
      }
    )


    output$download_priorities_ranking <- shiny::downloadHandler(
      filename = function() paste0("priorities_ranking_", Sys.Date(), ".png"),
      content = function(file) {
        shiny::req(rv$qdata)
        p <- create_plot_priorities_ranking(rv$qdata)
        J <- ncol(rv$qdata@sorts)
        ggplot2::ggsave(file, p, width = 9, height = max(7, J * 0.2),
                        dpi = 300, bg = "white")
      }
    )

    output$download_priorities_flow <- shiny::downloadHandler(
      filename = function() paste0("priorities_flow_", Sys.Date(), ".png"),
      content = function(file) {
        shiny::req(rv$qdata, has_participant_attributes(rv$qdata))
        ggplot2::ggsave(file, viz_flow_gg(), width = 12, height = 7.5,
                        dpi = 300, bg = "white")
      }
    )

    output$download_panel_composition <- shiny::downloadHandler(
      filename = function() paste0("panel_composition_", Sys.Date(), ".png"),
      content = function(file) {
        shiny::req(rv$qdata, has_participant_attributes(rv$qdata))
        g <- create_plot_panel_composition(rv$qdata)
        shiny::req(!is.null(g))
        k_attrs <- length(attribute_groups(rv$qdata))
        save_base_plot(file, 13, 1 + 4.2 * ceiling(k_attrs / 2),
                       function() grid::grid.draw(g), res = 300)
      }
    )


    # Download All (ZIP) ----

    output$download_all <- shiny::downloadHandler(
      filename = function() paste0("canhrqsort_visualizations_", Sys.Date(), ".zip"),
      content = function(file) {
        shiny::req(rv$qdata)
        results <- active_results()
        shiny::req(!is.null(results) || has_bq_fit())

        temp_dir <- file.path(tempdir(), paste0("canhrqsort_plots_", format(Sys.time(), "%Y%m%d_%H%M%S")))
        dir.create(temp_dir, showWarnings = FALSE)

        save_safe <- function(name, plot_expr, width, height) {
          tryCatch({
            p <- plot_expr
            if (!is.null(p)) {
              ggplot2::ggsave(file.path(temp_dir, paste0(name, ".png")), p,
                              width = width, height = height, dpi = 150, bg = "white")
            }
          }, error = function(e) NULL)
        }

        save_native <- function(name, draw, width, height) {
          tryCatch(
            save_base_plot(file.path(temp_dir, paste0(name, ".png")),
                           width, height, draw),
            error = function(e) NULL
          )
        }

        shiny::withProgress(message = "Exporting all charts...", value = 0, {

          if (!is.null(results)) {
            n_factors <- results@n_factors
            shiny::incProgress(0.1, detail = "Factor charts...")
            n_cols <- length(rv$qdata@distribution)
            max_height <- max(rv$qdata@distribution)
            for (f in seq_len(n_factors)) {
              save_safe(sprintf("01_factor_array_F%d", f),
                        create_plot_factor_array(results, rv$qdata, factor_num = f),
                        max(15, n_cols * 1.45), max_height * 1.15 + 2.4)
              save_safe(sprintf("04_rankings_F%d", f),
                        create_plot_ranking(results, rv$qdata, f), 12,
                        max(8, nrow(results@factor_scores) * 0.25))
            }
            save_safe("02_scree_plot", create_plot_scree(results, rv$qdata), 10, 7)
            save_safe("03_zscore_comparison", create_plot_zscore(results, rv$qdata), 12,
                      max(8, nrow(results@factor_scores) * 0.22))
            save_safe("05_crib_sheet_panel",
                      plot_crib_sheet_panel(results, n_poles = 3, text_width = 35), 16, 10)
            save_safe("06_factor_comparison",
                      plot_factor_comparison(results, sort_by = 1, n_show = NULL), 10, 14)
          }

          shiny::incProgress(0.5, detail = "Priorities...")
          save_safe("07_priorities_ranking",
                    create_plot_priorities_ranking(rv$qdata),
                    9, max(7, ncol(rv$qdata@sorts) * 0.2))
          if (has_participant_attributes(rv$qdata)) {
            save_safe("08_priorities_flow", viz_flow_gg(), 12, 7.5)
            g_comp <- create_plot_panel_composition(rv$qdata)
            if (!is.null(g_comp)) {
              k_attrs <- length(attribute_groups(rv$qdata))
              save_native("09_panel_composition",
                          function() grid::grid.draw(g_comp),
                          13, 1 + 4.2 * ceiling(k_attrs / 2))
            }
          }

          # Bayesian charts if available (bayesqm native figures)
          if (has_bq_fit()) {
            shiny::incProgress(0.7, detail = "Bayesian charts...")
            fit <- rv$bayesian$fit
            save_native("10_bayesqm_loadings",
                        function() bayesqm::plot_loading_posterior(fit, q = bq_q()), 10, 8)
            save_native("11_bayesqm_flags",
                        function() bayesqm::plot_flags(fit, q = bq_q()), 10, 8)
            save_native("12_bayesqm_statement_scores",
                        function() bayesqm::plot_zscores(fit, q = bq_q()), 10, 11)
            if (!is.null(rv$bayesian$tables$qdc)) {
              n_pairs <- ncol(utils::combn(rv$bayesian$K %||% 2, 2))
              for (pr_i in seq_len(n_pairs)) {
                save_native(sprintf("13_bayesqm_contrasts_pair%d", pr_i),
                            function() bayesqm::plot_contrasts(fit, pair = pr_i, q = bq_q()),
                            10, 11)
              }
            }
            for (f in seq_len(rv$bayesian$K %||% 1)) {
              save_safe(sprintf("14_bayesqm_factor_array_F%d", f),
                        create_plot_bq_factor_array(fit, rv$qdata, factor_num = f),
                        max(15, length(rv$qdata@distribution) * 1.45),
                        max(rv$qdata@distribution) * 1.15 + 2.4)
            }
            save_native("15_bayesqm_convergence",
                        function() bayesqm::plot_convergence(fit), 10, 8)
            save_native("16_bayesqm_ppc",
                        function() bayesqm::plot_ppc(rv$bayesian$tables$checks), 11, 5.5)
            save_native("17_bayesqm_person_check",
                        function() bayesqm::plot_person_check(rv$bayesian$tables$persons), 9, 7)
            if (!is.null(rv$bayesian$ladder)) {
              save_native("18_bayesqm_choice_of_k",
                          function() bayesqm::plot_choice_k(rv$bayesian$ladder$selection), 10, 6)
            }
          }

          shiny::incProgress(0.95, detail = "Creating ZIP...")

          plot_files <- list.files(temp_dir, pattern = "\\.png$", full.names = TRUE)
          if (length(plot_files) > 0) {
            if (requireNamespace("zip", quietly = TRUE)) {
              zip::zip(zipfile = file, files = plot_files, mode = "cherry-pick")
            } else {
              old_wd <- getwd()
              on.exit(setwd(old_wd), add = TRUE)
              setwd(temp_dir)
              utils::zip(zipfile = file, files = basename(plot_files))
            }
          }

          unlink(temp_dir, recursive = TRUE)
        })
      }
    )

    # Download links are outputs too: suspended inside a hidden panel the
    # button's href never binds and Export does nothing. Make every download
    # on the page eager; the file itself is still only built on click.
    for (dl in c("download_bq_loadings", "download_bq_flags",
                 "download_bq_zscores", "download_bq_contrasts",
                 "download_bq_statement", "download_bq_array",
                 "download_bq_convergence", "download_bq_ppc",
                 "download_bq_person", "download_bq_choice",
                 "download_comparison_plot", "download_ranking_plot",
                 "download_scree_plot", "download_factor_array",
                 "download_factor_comparison", "download_crib_panel",
                 "download_priorities_ranking", "download_priorities_flow",
                 "download_panel_composition", "download_all")) {
      shiny::outputOptions(output, dl, suspendWhenHidden = FALSE)
    }
  })
}
