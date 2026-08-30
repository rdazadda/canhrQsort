#' @title Factors Panel (Frequentist page, full model layer)
#' @description The factor analysis layer of the Frequentist page as a
#' four-step guided flow: Extract (retention evidence, k, scree), Flag
#' (verdict, rotation, loadings with live re-flagging), Interpret (arrays,
#' scores, distinguishing and consensus, crib sheets, participants), Export
#' (bootstrap stability, audit trail, workbook). Steps 2-4 unlock together
#' on the first run; step switching and Details disclosures are client-side
#' so they work while R computes. Pure UI and server binding; every
#' computation lives in analysis-core.R, bootstrap.R, and export-workbook.R.
#' @name app-page-analyze-factors
#' @keywords internal
NULL

fr_int_tab_ids <- c("fr_tab_stmt", "fr_tab_dist", "fr_tab_crib",
                    "fr_tab_part")
fr_int_panel_ids <- c("fr_panel_stmt", "fr_panel_dist", "fr_panel_crib",
                      "fr_panel_part")

# A title whose bold name carries a hover tip: the copy stays a fragment,
# the formula lives on hover
fr_title <- function(main, ..., tip = NULL) {
  facts <- c(...)
  htmltools::div(
    class = "pr-ttl",
    if (is.null(tip)) {
      htmltools::tags$b(main)
    } else {
      htmltools::tags$b(class = "fr-tip", title = tip, main)
    },
    if (length(facts) > 0) {
      htmltools::span(paste0(" · ", paste(facts, collapse = " · ")))
    }
  )
}

fr_details_link <- function(ns, det_id) {
  htmltools::tags$a(
    class = "fr-details-link", href = "#",
    onclick = sprintf("return canhrFrToggle('%s');", ns(det_id)),
    "Details"
  )
}

fr_stepper_ui <- function(ns) {
  titles <- c("Extract", "Flag", "Interpret", "Export")
  parts <- list()
  for (i in 1:4) {
    parts[[length(parts) + 1]] <- htmltools::div(
      id = ns(paste0("fr_step_", i)),
      class = paste("fr-step", if (i == 1) "cur" else "locked"),
      onclick = sprintf(
        "Shiny.setInputValue('%s', %d, {priority: 'event'});",
        ns("fr_step_click"), i
      ),
      htmltools::span(class = "fr-dot", i),
      htmltools::span(class = "fr-step-t", titles[i])
    )
    if (i < 4) parts[[length(parts) + 1]] <- htmltools::div(class = "fr-track")
  }
  htmltools::div(class = "fr-stepper", parts)
}

#' Factors body UI (rendered inside the analyze page)
#'
#' A static skeleton: the stepper plus four step panels that are shown and
#' hidden client-side, so re-flags and step switches never rebuild the DOM.
#' @keywords internal
factors_body_ui <- function(ns) {
  htmltools::tagList(
    shiny::uiOutput(ns("fr_step_sync")),
    fr_stepper_ui(ns),

    # Step 1: Extract ----
    htmltools::div(
      id = ns("fr_step_p1"),
      shiny::uiOutput(ns("fr_pre_verdict")),
      htmltools::div(
        class = "fr-ctl-row",
        shiny::uiOutput(ns("fr_k_input")),
        shiny::uiOutput(ns("fr_extract_btn")),
        fr_details_link(ns, "fr_det_1")
      ),
      htmltools::div(
        id = ns("fr_det_1"), class = "fr-details",
        shiny::uiOutput(ns("fr_step1_settings"))
      ),
      plotly::plotlyOutput(ns("fr_scree"), height = "340px"),
      shiny::uiOutput(ns("fr_ret_head")),
      shiny::uiOutput(ns("fr_ret_tbl"))
    ),

    # Step 2: Flag ----
    htmltools::div(
      id = ns("fr_step_p2"), style = "display: none;",
      shiny::uiOutput(ns("fr_verdict")),
      htmltools::div(
        class = "fr-ctl-row",
        shiny::uiOutput(ns("fr_rotation_input")),
        shiny::actionButton(ns("fr_confirm_flags"), "Confirm flags",
                            class = "btn-ov2-blue bq2-run"),
        fr_details_link(ns, "fr_det_2"),
        shiny::uiOutput(ns("fr_flag_chip"), inline = TRUE)
      ),
      htmltools::div(
        id = ns("fr_det_2"), class = "fr-details",
        shiny::uiOutput(ns("fr_flag_settings")),
        shiny::uiOutput(ns("fr_bipolar_line")),
        shiny::uiOutput(ns("fr_rot_controls")),
        plotly::plotlyOutput(ns("fr_rot_plot"), height = "380px")
      ),
      shiny::uiOutput(ns("fr_load_head")),
      DT::DTOutput(ns("fr_loadings"))
    ),

    # Step 3: Interpret ----
    htmltools::div(
      id = ns("fr_step_p3"), style = "display: none;",
      htmltools::div(
        class = "ov2-card pr-card",
        shiny::uiOutput(ns("fr_int_tabbar")),
        htmltools::div(
          class = "ov2-card-body",
          htmltools::div(
            id = ns("fr_panel_stmt"),
            shiny::uiOutput(ns("fr_arrays")),
            shiny::uiOutput(ns("fr_array_pane")),
            shiny::uiOutput(ns("fr_scores_head")),
            DT::DTOutput(ns("fr_scores"))
          ),
          htmltools::div(
            id = ns("fr_panel_dist"), style = "display: none;",
            shiny::uiOutput(ns("fr_dist_head")),
            DT::DTOutput(ns("fr_dist")),
            shiny::uiOutput(ns("fr_cons_head")),
            DT::DTOutput(ns("fr_cons"))
          ),
          htmltools::div(
            id = ns("fr_panel_crib"), style = "display: none;",
            shiny::uiOutput(ns("fr_crib_head")),
            shiny::uiOutput(ns("fr_crib_body"))
          ),
          htmltools::div(
            id = ns("fr_panel_part"), style = "display: none;",
            shiny::uiOutput(ns("fr_part_head")),
            DT::DTOutput(ns("fr_part"))
          )
        )
      ),
      htmltools::div(
        class = "fr-ctl-row",
        shiny::actionButton(ns("fr_continue"), "Continue",
                            class = "btn-ov2-blue bq2-run")
      )
    ),

    # Step 4: Export ----
    htmltools::div(
      id = ns("fr_step_p4"), style = "display: none;",
      shiny::uiOutput(ns("fr_stab_head")),
      htmltools::div(
        class = "fr-ctl-row",
        htmltools::div(
          htmltools::div(class = "bq2-label", "Resamples"),
          shiny::numericInput(ns("fr_boot_n"), NULL, value = 500,
                              min = 100, max = 2000, width = "90px")
        ),
        shiny::actionButton(ns("fr_run_boot"), "Run bootstrap",
                            class = "btn-bq2-quiet"),
        shiny::uiOutput(ns("fr_stale_chip"), inline = TRUE),
        fr_details_link(ns, "fr_det_4"),
        htmltools::div(style = "flex: 1;"),
        shiny::downloadButton(ns("fr_workbook"), "Download workbook",
                              class = "btn-ov2-blue bq2-run", icon = NULL)
      ),
      htmltools::div(
        id = ns("fr_det_4"), class = "fr-details",
        shiny::uiOutput(ns("fr_m_chars_head")),
        DT::DTOutput(ns("fr_m_chars")),
        shiny::uiOutput(ns("fr_m_sed_head")),
        DT::DTOutput(ns("fr_m_sed")),
        shiny::uiOutput(ns("fr_m_fsc_head")),
        DT::DTOutput(ns("fr_m_fsc")),
        shiny::uiOutput(ns("fr_m_unrot_head")),
        DT::DTOutput(ns("fr_m_unrot")),
        shiny::uiOutput(ns("fr_m_cor_head")),
        DT::DTOutput(ns("fr_m_cor")),
        shiny::uiOutput(ns("fr_m_defcor"))
      ),
      DT::DTOutput(ns("fr_boot_load")),
      shiny::uiOutput(ns("fr_boot_scores_head")),
      DT::DTOutput(ns("fr_boot_scores"))
    )
  )
}

#' Bind the Factors server logic inside the analyze module server
#' @keywords internal
factors_server_bind <- function(input, output, session, rv, ns, config) {

  fmt_m2 <- function(x) gsub("-", "−", sprintf("%+.2f", x), fixed = TRUE)
  fmt_z <- function(x) gsub("-", "−", sprintf("%.2f", x), fixed = TRUE)
  fmt_q <- function(x) gsub("-", "−", sprintf("%+d", as.integer(x)), fixed = TRUE)
  fmt_p_cell <- function(p) {
    ifelse(is.na(p), "",
           ifelse(p < 0.001, "&lt;0.001", sprintf("%.3f", p)))
  }
  fcolors <- get_theme_colors()$factor_colors
  fcol <- function(k) fcolors[((k - 1) %% length(fcolors)) + 1]

  # Step state ----

  output$fr_step_sync <- shiny::renderUI({
    cur <- as.integer(rv$fr_step %||% 1L)
    unlocked <- if (isTRUE(rv$analysis_complete)) 4L else 1L
    if (is.na(cur) || cur < 1L || cur > unlocked) cur <- 1L
    htmltools::tags$script(htmltools::HTML(sprintf(
      "if (window.canhrFrStep) canhrFrStep(%s, %s, %d, %d);",
      pr_js_array(vapply(paste0("fr_step_", 1:4), ns, character(1))),
      pr_js_array(vapply(paste0("fr_step_p", 1:4), ns, character(1))),
      cur, unlocked
    )))
  })

  shiny::observeEvent(input$fr_step_click, {
    i <- as.integer(input$fr_step_click)
    shiny::req(!is.na(i), i >= 1, i <= 4)
    if (i > 1 && !isTRUE(rv$analysis_complete)) return()
    rv$fr_step <- i
  })

  shiny::observeEvent(input$fr_confirm_flags, { rv$fr_step <- 3L })
  shiny::observeEvent(input$fr_continue, { rv$fr_step <- 4L })

  # Shared reactives ----

  fr_ret <- shiny::reactive({
    qd <- rv$qdata
    shiny::req(qd)
    # Seed the permutations so the k suggestion is stable across visits
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      old_seed <- get(".Random.seed", envir = globalenv())
      on.exit(assign(".Random.seed", old_seed, envir = globalenv()),
              add = TRUE)
    }
    set.seed(42)
    pa <- parallel_analysis(qd, n_iter = 100)
    re <- retention_evidence(qd, pa = pa)
    qm <- tryCatch(
      suppressWarnings(suppressMessages(compute_quality_metrics(qd))),
      error = function(e) NULL
    )
    kmo <- NA_real_
    bartlett_txt <- NULL
    if (!is.null(qm) && is.data.frame(qm)) {
      km_row <- grep("KMO", qm$metric, ignore.case = TRUE)
      if (length(km_row) > 0) {
        kmo <- suppressWarnings(as.numeric(qm$value[km_row[1]]))
      }
      bt_row <- grep("Bartlett", qm$metric, ignore.case = TRUE)
      if (length(bt_row) > 0) {
        bv <- suppressWarnings(as.numeric(qm$value[bt_row[1]]))
        bartlett_txt <- if (!is.na(bv) && bv < 0.001) {
          "Bartlett p < 0.001"
        } else if (!is.na(bv)) {
          sprintf("Bartlett p = %.3f", bv)
        }
      }
    }
    list(pa = pa, re = re, kmo = kmo, bartlett = bartlett_txt)
  })

  # Auto-set the Factors count to the parallel-analysis suggestion, but
  # only for genuinely new sorts and never over a completed run: attaching
  # demographics or re-binning also reassigns rv$qdata and must not clobber
  # a user-chosen k
  fr_sorts_sig <- shiny::reactiveVal(NULL)
  shiny::observeEvent(rv$qdata, {
    qd <- rv$qdata
    shiny::req(qd)
    s <- qd@sorts
    sig <- paste(nrow(s), ncol(s), sum(s, na.rm = TRUE), sep = "|")
    if (identical(sig, fr_sorts_sig())) return()
    fr_sorts_sig(sig)
    if (isTRUE(rv$analysis_complete)) return()
    ret <- tryCatch(fr_ret(), error = function(e) NULL)
    if (!is.null(ret)) {
      k_pa <- max(1L, attr(ret$re, "k_parallel") %||% 2L)
      config$n_factors <- min(k_pa, 12L)
      shiny::updateNumericInput(session, "n_factors", value = min(k_pa, 12L))
    }
  }, ignoreInit = FALSE)

  fr_res <- shiny::reactive({
    shiny::req(rv$analysis_complete)
    rv$results
  })

  # Distinguishing lens and criterion; re-judged from the stored solution
  fr_lens <- shiny::reactiveVal(0.05)
  shiny::observeEvent(input$fr_lens_05, fr_lens(0.05))
  shiny::observeEvent(input$fr_lens_01, fr_lens(0.01))
  shiny::observeEvent(input$fr_lens_001, fr_lens(0.001))
  fr_lens_level <- shiny::reactive({
    switch(as.character(fr_lens()), "0.05" = 1L, "0.01" = 2L, "0.001" = 3L)
  })

  fr_dist_obj <- shiny::reactive({
    res <- fr_res()
    shiny::req(res)
    crit <- input$fr_dist_from %||% "all"
    base <- res@method_details$distinction
    if (!is.null(base) && identical(base$criterion, crit) &&
        length(base$distinguishing) == res@n_factors) {
      base
    } else {
      # A stale SED matrix (for example after a bipolar split changed the
      # factor count) must not be indexed against the new solution
      sed <- res@factor_characteristics$sed_matrix
      se <- res@factor_characteristics$characteristics$se_factor
      if (is.null(sed) || nrow(sed) != res@n_factors) {
        sed <- NULL
        if (length(se) != res@n_factors) se <- NULL
      }
      qsort_distinguish(
        res@factor_scores,
        statements = res@data@statements,
        sed_matrix = sed,
        se_factors = se,
        criterion = crit
      )
    }
  })

  # Step 1: Extract: the reason to pick k ----

  output$fr_k_input <- shiny::renderUI({
    htmltools::div(
      htmltools::div(class = "bq2-label", "Factors"),
      shiny::numericInput(ns("n_factors"), NULL,
                          value = shiny::isolate(config$n_factors) %||% 3,
                          min = 1, max = 12, width = "72px")
    )
  })

  output$fr_extract_btn <- shiny::renderUI({
    k <- suppressWarnings(as.integer(input$n_factors %||% config$n_factors))
    if (is.na(k)) k <- 3L
    k <- max(1L, min(12L, k))
    shiny::actionButton(
      ns("run_analysis"),
      sprintf("Extract %d %s", k, if (k == 1) "factor" else "factors"),
      class = "btn-ov2-blue bq2-run"
    )
  })

  output$fr_step1_settings <- shiny::renderUI({
    htmltools::div(
      class = "fr-ctl-row", style = "margin: 0;",
      htmltools::div(
        htmltools::div(class = "bq2-label", "Extraction"),
        shiny::selectInput(ns("extraction_method"), NULL,
          choices = c(
            "Principal components" = "pca",
            "Centroid (Brown)" = "centroid",
            "Minimum residuals" = "minres"
          ),
          selected = shiny::isolate(config$extraction),
          width = "200px", selectize = FALSE
        )
      ),
      htmltools::div(
        htmltools::div(class = "bq2-label", "Correlation"),
        shiny::radioButtons(ns("correlation_method"), NULL,
          choices = c("Pearson" = "pearson", "Spearman" = "spearman",
                      "Kendall" = "kendall"),
          selected = shiny::isolate(config$correlation), inline = TRUE
        )
      )
    )
  })

  output$fr_pre_verdict <- shiny::renderUI({
    ret <- fr_ret()
    k_pa <- max(1L, attr(ret$re, "k_parallel") %||% 1L)
    k_kaiser <- attr(ret$re, "k_kaiser") %||% NA_integer_
    k_two <- attr(ret$re, "k_two") %||% 0L
    n <- nrow(rv$qdata@sorts)

    headline <- sprintf("%d %s supported", k_pa,
                        if (k_pa == 1) "factor" else "factors")
    status <- paste(c(
      sprintf("%d above the parallel 95th", k_pa),
      sprintf("two-loadings holds through %d", k_two),
      if (!is.na(ret$kmo)) sprintf("KMO %.2f", ret$kmo),
      ret$bartlett
    ), collapse = " · ")
    gate <- if (!is.na(k_kaiser) && k_kaiser != k_pa) {
      sprintf("Kaiser suggests %d; with %d sorts, trust the permutation test.",
              k_kaiser, n)
    }

    htmltools::div(
      class = "bq2-verdict pr-verdict",
      htmltools::div(
        class = "bq2-verdict-main",
        htmltools::div(class = "bq2-verdict-headline", headline),
        htmltools::div(
          class = "bq2-verdict-status",
          htmltools::span(class = "bq2-status-rest", status)
        ),
        if (!is.null(gate)) {
          htmltools::div(class = "bq2-verdict-body pr-gate-note", gate)
        }
      )
    )
  })

  output$fr_scree <- plotly::renderPlotly({
    ret <- fr_ret()
    pa <- ret$pa
    n_show <- min(nrow(pa), 15L)
    d <- pa[seq_len(n_show), ]
    plotly::plot_ly(d) |>
      plotly::add_trace(x = ~factor, y = ~eigenvalue, type = "scatter",
                        mode = "lines+markers", name = "Observed",
                        line = list(color = "#236192", width = 2),
                        marker = list(color = "#236192", size = 7)) |>
      plotly::add_trace(x = ~factor, y = ~threshold, type = "scatter",
                        mode = "lines", name = "Parallel 95th",
                        line = list(color = "#DF6A2E", width = 2,
                                    dash = "dot")) |>
      plotly::add_trace(x = c(1, n_show), y = c(1, 1), type = "scatter",
                        mode = "lines", name = "Kaiser",
                        line = list(color = "#94a3b8", width = 1,
                                    dash = "dash")) |>
      plotly::layout(
        xaxis = list(title = "Factor", dtick = 1, fixedrange = TRUE),
        yaxis = list(title = "Eigenvalue", fixedrange = TRUE),
        legend = list(orientation = "h", x = 0.55, y = 0.98),
        margin = list(l = 50, r = 10, t = 10, b = 40),
        font = list(family = "Segoe UI, sans-serif", size = 12)
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  output$fr_ret_head <- shiny::renderUI({
    qd <- rv$qdata
    shiny::req(qd)
    pr_title("Retention evidence",
             sprintf("%d sorts", nrow(qd@sorts)),
             sprintf("%d statements", ncol(qd@sorts)),
             "100 permutations")
  })

  output$fr_ret_tbl <- shiny::renderUI({
    qd <- rv$qdata
    shiny::req(qd)
    ret <- fr_ret()
    re <- ret$re
    k_pa <- attr(re, "k_parallel") %||% 0L
    thr <- attr(re, "flag_threshold")
    J <- ncol(qd@sorts)
    yn <- function(x) {
      if (isTRUE(x)) "yes" else htmltools::span(class = "pr-cell-muted", "no")
    }

    rows <- lapply(seq_len(nrow(re)), function(r) {
      htmltools::tags$tr(
        class = if (re$k[r] == k_pa) "fr-ret-pick",
        htmltools::tags$td(re$k[r]),
        htmltools::tags$td(class = "r", sprintf("%.2f", re$eigenvalue[r])),
        htmltools::tags$td(class = "r",
                           sprintf("%.0f", 100 * re$cumulative[r])),
        htmltools::tags$td(class = "r", sprintf("%.2f", re$parallel_95[r])),
        htmltools::tags$td(yn(re$kaiser[r])),
        htmltools::tags$td(yn(re$humphrey[r])),
        htmltools::tags$td(yn(re$two_loadings[r]))
      )
    })

    htmltools::div(
      class = "fr-tbl-scroll",
      htmltools::tags$table(
        class = "fr-tbl",
        htmltools::tags$thead(htmltools::tags$tr(
          htmltools::tags$th("k"),
          htmltools::tags$th(class = "r", "Eigenvalue"),
          htmltools::tags$th(class = "r", "Cumulative %"),
          htmltools::tags$th(class = "r", "Parallel 95th"),
          htmltools::tags$th("Kaiser"),
          htmltools::tags$th(htmltools::span(
            class = "fr-tip",
            title = sprintf(
              "cross-product of the two largest loadings above 2/sqrt(%d)", J),
            "Humphrey"
          )),
          htmltools::tags$th(htmltools::span(
            class = "fr-tip",
            title = sprintf("at least two sorts above %.2f", thr),
            "Two loadings"
          ))
        )),
        htmltools::tags$tbody(rows)
      )
    )
  })

  # Step 2: Flag: verdict, rotation, loadings ----

  output$fr_verdict <- shiny::renderUI({
    res <- fr_res()
    shiny::req(res)
    k <- res@n_factors
    cumvar <- res@extraction$cumulative_variance[k]
    flags <- res@flagging
    n <- nrow(flags)
    n_def <- sum(rowSums(flags) > 0)
    p_flag <- res@method_details$flag_p_level %||% 0.05
    chars <- res@factor_characteristics$characteristics

    headline <- sprintf("%d %s explain%s %.0f%% of variance",
                        k, if (k == 1) "factor" else "factors",
                        if (k == 1) "s" else "", 100 * cumvar)

    mid_fact <- if (k >= 2) {
      zmat <- as.matrix(
        res@factor_scores[, paste0("F", seq_len(k), "_zscore"), drop = FALSE]
      )
      fsc <- stats::cor(zmat, use = "pairwise.complete.obs")
      sprintf("max array correlation %.2f", max(abs(fsc[upper.tri(fsc)])))
    } else {
      sprintf("composite reliability %.2f", chars$composite_reliability[1])
    }

    status <- paste(c(
      sprintf("%d of %d sorts defining", n_def, n),
      mid_fact,
      sprintf("flags at p < %s", format(p_flag))
    ), collapse = " · ")

    # One gate line, first match only
    gate <- NULL
    bp <- tryCatch(detect_bipolar(res), error = function(e) NULL)
    if (!is.null(bp) && any(bp$is_bipolar)) {
      f <- which(bp$is_bipolar)[1]
      gate <- sprintf("Factor %d is bipolar (%d negative-pole sorts); split under Details.",
                      f, bp$n_negative[f])
    }
    if (is.null(gate) && n - n_def > 0) {
      gate <- sprintf("%d %s no factor; the bootstrap on the Export step shows how often %s would flag.",
                      n - n_def,
                      if (n - n_def == 1) "sort defines" else "sorts define",
                      if (n - n_def == 1) "it" else "they")
    }
    if (is.null(gate) && k >= 2) {
      zmat <- as.matrix(
        res@factor_scores[, paste0("F", seq_len(k), "_zscore"), drop = FALSE]
      )
      fsc <- stats::cor(zmat, use = "pairwise.complete.obs")
      hi <- which(abs(fsc) == max(abs(fsc[upper.tri(fsc)])), arr.ind = TRUE)
      hi <- hi[hi[, 1] < hi[, 2], , drop = FALSE]
      if (nrow(hi) > 0 && abs(fsc[hi[1, 1], hi[1, 2]]) > 0.6) {
        gate <- sprintf("Factors %d and %d correlate at %.2f; consider %d %s or an oblique rotation.",
                        hi[1, 1], hi[1, 2], fsc[hi[1, 1], hi[1, 2]],
                        max(1, k - 1),
                        if (k - 1 == 1) "factor" else "factors")
      }
    }

    htmltools::div(
      class = "bq2-verdict pr-verdict",
      htmltools::div(
        class = "bq2-verdict-main",
        htmltools::div(class = "bq2-verdict-headline", headline),
        htmltools::div(
          class = "bq2-verdict-status",
          htmltools::span(class = "bq2-status-rest", status)
        ),
        if (!is.null(gate)) {
          htmltools::div(class = "bq2-verdict-body pr-gate-note", gate)
        }
      )
    )
  })

  output$fr_rotation_input <- shiny::renderUI({
    rv$fr_run_id
    res <- shiny::isolate(rv$results)
    if (isTRUE(shiny::isolate(rv$analysis_complete)) && !is.null(res) &&
        res@n_factors < 2) {
      return(NULL)
    }
    htmltools::div(
      htmltools::div(class = "bq2-label", "Rotation"),
      shiny::selectInput(ns("rotation_method"), NULL,
        choices = c(
          "Varimax" = "varimax",
          "Promax" = "promax",
          "Oblimin" = "oblimin",
          "None" = "none"
        ),
        selected = shiny::isolate(config$rotation),
        width = "150px", selectize = FALSE
      )
    )
  })

  output$fr_flag_settings <- shiny::renderUI({
    htmltools::div(
      class = "fr-ctl-row",
      style = "margin: 0 0 8px; align-items: flex-start; gap: 26px;",
      htmltools::div(
        htmltools::div(class = "bq2-label", "Flagging significance"),
        shiny::selectInput(ns("flag_p_level"), NULL,
          choices = c("p < 0.05" = "0.05", "p < 0.01" = "0.01",
                      "p < 0.001" = "0.001"),
          selected = format(shiny::isolate(config$flag_p_level)),
          width = "140px", selectize = FALSE
        )
      ),
      htmltools::div(
        htmltools::div(class = "bq2-label", "Rule"),
        shiny::checkboxInput(ns("auto_flag"), "Flag automatically",
                             value = shiny::isolate(config$auto_flag)),
        shiny::checkboxInput(ns("flag_majority"),
                             "Require a majority of common variance",
                             value = shiny::isolate(config$flag_majority))
      )
    )
  })

  output$fr_flag_chip <- shiny::renderUI({
    if (!isTRUE(rv$fr_manual_flags)) return(NULL)
    htmltools::span(class = "fr-chip", "Manual flags · cleared on re-extract")
  })

  output$fr_load_head <- shiny::renderUI({
    res <- fr_res()
    shiny::req(res)
    J <- ncol(res@data@sorts)
    p_flag <- res@method_details$flag_p_level %||% 0.05
    zc <- stats::qnorm(1 - p_flag / 2)
    thr <- zc / sqrt(J)
    tip <- sprintf("%.2f/sqrt(%d) = %.2f (p < %s)%s", zc, J, thr,
                   format(p_flag),
                   if (isTRUE(res@method_details$flag_majority %||% TRUE)) {
                     " · plus a squared loading above half the communality"
                   } else {
                     ""
                   })
    htmltools::div(
      class = "pr-ttl",
      htmltools::tags$b("Rotated loadings"),
      htmltools::span(sprintf(" · %d sorts · ", nrow(res@flagging))),
      htmltools::span(class = "fr-tip", title = tip,
                      sprintf("flag |a| > %.2f", thr))
    )
  })

  output$fr_loadings <- DT::renderDT(server = FALSE, {
    res <- fr_res()
    shiny::req(res)
    loadings <- res@rotation$loadings
    flags <- res@flagging
    k <- ncol(loadings)
    n <- nrow(loadings)
    ids <- rownames(loadings) %||% paste0("sort", seq_len(n))

    dominant <- max.col(abs(loadings), ties.method = "first")
    grp <- ifelse(rowSums(flags) > 0,
                  max.col(flags + 0, ties.method = "first"), k + 1L)
    ord <- order(grp, -abs(loadings[cbind(seq_len(n), dominant)]))

    df <- data.frame(Sort = ids, check.names = FALSE,
                     stringsAsFactors = FALSE)
    for (f in seq_len(k)) {
      cells <- vapply(seq_len(n), function(i) {
        val <- gsub("-", "−", sprintf("%.2f", loadings[i, f]), fixed = TRUE)
        chk <- sprintf(
          paste0('<input type="checkbox" %s onclick=',
                 '"Shiny.setInputValue(\'%s\', {i: %d, f: %d, on: this.checked}, ',
                 '{priority: \'event\'})"/>'),
          if (flags[i, f]) "checked" else "", ns("fr_toggle_flag"), i, f
        )
        val_span <- if (flags[i, f]) {
          sprintf('<span style="color:%s;font-weight:700;">%s</span>',
                  fcol(f), val)
        } else {
          val
        }
        paste0('<label class="fr-load-cell">', chk, val_span, "</label>")
      }, character(1))
      df[[paste0("F", f)]] <- cells
    }
    df[["h²"]] <- sprintf("%.2f", rowSums(loadings^2))
    df <- df[ord, , drop = FALSE]

    DT::datatable(
      df, escape = FALSE, rownames = FALSE,
      options = list(pageLength = 15, dom = "ftp", scrollX = TRUE,
                     ordering = FALSE),
      class = "compact stripe"
    )
  })

  shiny::observeEvent(input$fr_toggle_flag, {
    res <- rv$results
    shiny::req(res, rv$qdata)
    info <- input$fr_toggle_flag
    i <- as.integer(info$i)
    f <- as.integer(info$f)
    flags <- res@flagging
    shiny::req(i >= 1, i <= nrow(flags), f >= 1, f <= ncol(flags))
    flags[i, f] <- isTRUE(info$on)
    res@flagging <- flags
    out <- tryCatch(recalculate_after_flagging(res, rv$qdata),
                    error = function(e) e)
    if (inherits(out, "error")) {
      session$sendCustomMessage("showToast", list(
        message = paste("Re-flag failed:", conditionMessage(out)),
        type = "error", duration = 6000
      ))
      return()
    }
    rv$results <- out
    rv$fr_manual_flags <- TRUE
    if (!is.null(rv$fr_boot)) rv$fr_boot$stale <- TRUE
    sort_id <- rownames(flags)[i] %||% paste0("sort ", i)
    sort_label <- tools::toTitleCase(gsub("[_.]?(qsort|sort)[_.]?", "Sort ",
                                          sort_id, ignore.case = TRUE))
    session$sendCustomMessage("showToast", list(
      message = sprintf("%s %s on Factor %d", sort_label,
                        if (isTRUE(info$on)) "flagged" else "unflagged", f),
      type = "success"
    ))
  })

  # Bipolar split and rotate-by-hand live under the step-2 Details

  output$fr_bipolar_line <- shiny::renderUI({
    res <- fr_res()
    shiny::req(res)
    bp <- tryCatch(detect_bipolar(res), error = function(e) NULL)
    if (is.null(bp) || !any(bp$is_bipolar)) return(NULL)
    f <- which(bp$is_bipolar)[1]
    htmltools::div(
      class = "pr-ttl-sub",
      sprintf("Factor %d is bipolar (%d negative-pole sorts). ",
              f, bp$n_negative[f]),
      shiny::actionLink(ns("fr_split_bipolar"),
                        "Split into positive and negative poles",
                        class = "pr-reset")
    )
  })

  shiny::observeEvent(input$fr_split_bipolar, {
    res <- rv$results
    shiny::req(res, rv$qdata)
    bp <- tryCatch(detect_bipolar(res), error = function(e) NULL)
    shiny::req(!is.null(bp), any(bp$is_bipolar))
    f <- which(bp$is_bipolar)[1]
    out <- tryCatch(split_bipolar_factor(res, f), error = function(e) e)
    if (inherits(out, "error")) {
      session$sendCustomMessage("showToast", list(
        message = paste("Split failed:", conditionMessage(out)),
        type = "error", duration = 6000
      ))
    } else {
      rv$results <- out
      rv$fr_run_id <- (rv$fr_run_id %||% 0) + 1
      rv$fr_manual_flags <- FALSE
      # A bootstrap of the old factor count is meaningless for the new one
      rv$fr_boot <- NULL
      session$sendCustomMessage("showToast", list(
        message = sprintf("Factor %d split into its positive and negative poles", f),
        type = "success"
      ))
    }
  })

  # Judgmental rotation: pick a factor pair, set degrees, watch the live
  # preview, apply. Re-extracting restores the analytic rotation.
  output$fr_rot_controls <- shiny::renderUI({
    res <- fr_res()
    shiny::req(res)
    k <- res@n_factors
    if (k < 2) return(NULL)
    pairs <- utils::combn(k, 2)
    choices <- stats::setNames(
      apply(pairs, 2, paste, collapse = "-"),
      apply(pairs, 2, function(p) sprintf("F%d and F%d", p[1], p[2]))
    )
    htmltools::div(
      class = "pr-ttl-row",
      htmltools::div(
        htmltools::div(class = "bq2-label", "Rotate by hand"),
        shiny::selectInput(ns("fr_rot_pair"), NULL, choices = choices,
                           selected = shiny::isolate(input$fr_rot_pair) %||%
                             choices[[1]],
                           width = "150px", selectize = FALSE)
      ),
      htmltools::div(
        htmltools::div(
          class = "bq2-label fr-tip",
          title = "positive degrees rotate counterclockwise · re-extracting restores the analytic rotation",
          "Degrees"
        ),
        shiny::numericInput(ns("fr_rot_deg"), NULL, value = 0,
                            min = -180, max = 180, step = 1, width = "90px")
      ),
      htmltools::div(
        style = "align-self: flex-end;",
        shiny::actionButton(ns("fr_rot_apply"), "Apply",
                            class = "btn-ov2-blue bq2-run")
      )
    )
  })

  output$fr_rot_plot <- plotly::renderPlotly({
    res <- fr_res()
    shiny::req(res, res@n_factors >= 2)
    pair <- strsplit(input$fr_rot_pair %||% "1-2", "-", fixed = TRUE)[[1]]
    i <- as.integer(pair[1])
    j <- as.integer(pair[2])
    shiny::req(!is.na(i), !is.na(j), j <= res@n_factors)
    deg <- input$fr_rot_deg %||% 0
    if (!is.finite(deg)) deg <- 0
    L <- res@rotation$loadings
    Lr <- qsort_rotate_manual(L, c(i, j), deg)
    J <- ncol(res@data@sorts)
    p_flag <- res@method_details$flag_p_level %||% 0.05
    thr <- stats::qnorm(1 - p_flag / 2) / sqrt(J)
    ids <- rownames(L) %||% paste0("sort", seq_len(nrow(L)))
    lim <- max(1, max(abs(c(L[, c(i, j)], Lr[, c(i, j)]))) * 1.08)

    p <- plotly::plot_ly()
    if (abs(deg) > 0.01) {
      p <- p |> plotly::add_trace(
        x = L[, i], y = L[, j], type = "scatter", mode = "markers",
        name = "Current", text = ids, hoverinfo = "text",
        marker = list(color = "#cbd5e1", size = 7)
      )
    }
    p |>
      plotly::add_trace(
        x = Lr[, i], y = Lr[, j], type = "scatter", mode = "markers",
        name = if (abs(deg) > 0.01) sprintf("Rotated %d°", round(deg)) else "Current",
        text = ids, hoverinfo = "text",
        marker = list(color = "#236192", size = 8)
      ) |>
      plotly::layout(
        xaxis = list(title = paste0("F", i), range = c(-lim, lim),
                     zeroline = TRUE, fixedrange = TRUE),
        yaxis = list(title = paste0("F", j), range = c(-lim, lim),
                     zeroline = TRUE, fixedrange = TRUE,
                     scaleanchor = "x", scaleratio = 1),
        shapes = list(
          list(type = "line", x0 = thr, x1 = thr, y0 = -lim, y1 = lim,
               line = list(color = "#94a3b8", width = 1, dash = "dash")),
          list(type = "line", x0 = -thr, x1 = -thr, y0 = -lim, y1 = lim,
               line = list(color = "#94a3b8", width = 1, dash = "dash")),
          list(type = "line", y0 = thr, y1 = thr, x0 = -lim, x1 = lim,
               line = list(color = "#94a3b8", width = 1, dash = "dash")),
          list(type = "line", y0 = -thr, y1 = -thr, x0 = -lim, x1 = lim,
               line = list(color = "#94a3b8", width = 1, dash = "dash"))
        ),
        legend = list(orientation = "h", x = 0, y = 1.08),
        margin = list(l = 50, r = 20, t = 10, b = 45),
        font = list(family = "Segoe UI, sans-serif", size = 12)
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  shiny::observeEvent(input$fr_rot_apply, {
    res <- rv$results
    shiny::req(res, res@n_factors >= 2)
    pair <- strsplit(input$fr_rot_pair %||% "1-2", "-", fixed = TRUE)[[1]]
    i <- as.integer(pair[1])
    j <- as.integer(pair[2])
    deg <- input$fr_rot_deg %||% 0
    if (!is.finite(deg) || abs(deg) < 0.01) {
      session$sendCustomMessage("showToast", list(
        message = "Set a rotation angle first.", type = "info"
      ))
      return()
    }
    out <- tryCatch(qsort_reanalyze_rotated(res, factors = c(i, j),
                                            angle = deg),
                    error = function(e) e)
    if (inherits(out, "error")) {
      session$sendCustomMessage("showToast", list(
        message = paste("Rotation failed:", conditionMessage(out)),
        type = "error", duration = 6000
      ))
    } else {
      rv$results <- out
      rv$fr_manual_flags <- FALSE
      if (!is.null(rv$fr_boot)) rv$fr_boot$stale <- TRUE
      shiny::updateNumericInput(session, "fr_rot_deg", value = 0)
      session$sendCustomMessage("showToast", list(
        message = sprintf("Rotated F%d and F%d by %d°", i, j, round(deg)),
        type = "success"
      ))
    }
  })

  # Step 3: Interpret: sub-tabs ----

  output$fr_int_tabbar <- shiny::renderUI({
    rv$fr_run_id
    res <- shiny::isolate(rv$results)
    k <- if (!is.null(res)) res@n_factors else 2L
    show_dist <- k >= 2
    keep <- if (show_dist) 1:4 else c(1L, 3L, 4L)
    tabs <- vapply(fr_int_tab_ids[keep], ns, character(1))
    panels <- vapply(fr_int_panel_ids[keep], ns, character(1))
    labels <- c("Statements", "Distinguishing & consensus", "Crib sheets",
                "Participants")[keep]

    links <- lapply(seq_along(tabs), function(t) {
      htmltools::tags$a(
        id = tabs[t], href = "#",
        class = paste(c("ov2-tab", if (t == 1) "active"), collapse = " "),
        onclick = sprintf("return canhrPrTab(this, %s, %s);",
                          pr_js_array(tabs), pr_js_array(panels)),
        labels[t]
      )
    })

    # A fresh solution lands on Statements; any tab dropped for k = 1 is
    # hidden outright since the static panels keep their last display state
    reset_js <- sprintf(
      "var t = document.getElementById('%s'); if (t && window.canhrPrTab) canhrPrTab(t, %s, %s);%s",
      ns("fr_tab_stmt"), pr_js_array(tabs), pr_js_array(panels),
      if (show_dist) "" else sprintf(
        " var d = document.getElementById('%s'); if (d) d.style.display = 'none';",
        ns("fr_panel_dist"))
    )

    htmltools::tagList(
      htmltools::div(class = "ov2-tabbar", links),
      htmltools::tags$script(htmltools::HTML(reset_js))
    )
  })

  # Statements: pyramids plus the score table ----

  fr_arr_sel <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$fr_arr_sel, fr_arr_sel(input$fr_arr_sel))
  # A tile picked under the previous solution must not survive a re-run
  shiny::observeEvent(rv$fr_run_id, fr_arr_sel(NULL), ignoreInit = TRUE)

  output$fr_arrays <- shiny::renderUI({
    res <- fr_res()
    qd <- rv$qdata
    shiny::req(res, qd)
    fs <- res@factor_scores
    k <- res@n_factors
    C <- length(qd@distribution)
    labs <- bq_grid_labels(C)
    ramp <- grDevices::colorRampPalette(ov2_ramp_anchors)(C)
    stmts <- qd@statements
    fmt_lab <- function(x) sub("-", "−", as.character(x), fixed = TRUE)
    sel <- fr_arr_sel()
    sel_i <- if (!is.null(sel) && !is.null(sel$s)) as.integer(sel$s) else NA_integer_
    tile_txt <- function(col) {
      rgbv <- grDevices::col2rgb(col)
      lum <- (0.299 * rgbv[1] + 0.587 * rgbv[2] + 0.114 * rgbv[3]) / 255
      if (lum > 0.5) "#1e293b" else "#ffffff"
    }

    pyramids <- lapply(seq_len(k), function(f) {
      scores_f <- fs[[paste0("F", f, "_score")]]
      cols_ui <- lapply(seq_len(C), function(col) {
        idx <- which(scores_f == labs[col])
        tiles <- lapply(idx, function(i) {
          sid <- paste0("S", fs$statement_num[i])
          tip <- paste0(sid, " · column ", fmt_lab(labs[col]),
                        if (!is.na(fs$statement_num[i]) &&
                            fs$statement_num[i] <= length(stmts)) {
                          paste0("\n", stmts[fs$statement_num[i]])
                        })
          is_sel <- !is.na(sel_i) && sel_i == i
          htmltools::div(
            class = paste("ov2-qs-tile", if (is_sel) "sel"),
            style = paste0("background:", ramp[col], ";color:",
                           tile_txt(ramp[col]), ";"),
            title = tip,
            onclick = sprintf(
              "Shiny.setInputValue('%s', {s: %d, f: %d}, {priority: 'event'})",
              ns("fr_arr_sel"), i, f),
            sid
          )
        })
        htmltools::div(
          class = "ov2-qs-col",
          htmltools::div(class = "ov2-qs-stack", tiles),
          htmltools::div(class = "ov2-qs-axis", fmt_lab(labs[col]))
        )
      })
      n_def_f <- sum(res@flagging[, f])
      var_f <- res@extraction$variance_explained[f]
      htmltools::div(
        class = "bq2-array",
        htmltools::div(
          class = "bq2-array-title",
          htmltools::span(class = "bq2-comp-dot",
                          style = paste0("background:", fcol(f), ";")),
          paste("Factor", f)
        ),
        htmltools::div(class = "bq2-array-sub",
                       sprintf("%d defining · %.0f%% variance",
                               n_def_f, 100 * var_f)),
        htmltools::div(
          class = "ov2-qs-wrap",
          style = sprintf("max-width: %dpx;", C * 82),
          htmltools::div(class = "ov2-qs-row", cols_ui),
          htmltools::div(
            class = "ov2-qs-ends",
            htmltools::div("Most disagree"),
            htmltools::div("Most agree")
          )
        )
      )
    })

    htmltools::div(class = "bq2-array-grid", pyramids)
  })

  output$fr_array_pane <- shiny::renderUI({
    res <- fr_res()
    qd <- rv$qdata
    shiny::req(res, qd)
    sel <- fr_arr_sel()
    if (is.null(sel) || is.null(sel$s)) {
      return(htmltools::div(class = "ov2-qs-pane empty",
                            "Click a tile to read its statement."))
    }
    fs <- res@factor_scores
    i <- as.integer(sel$s)
    shiny::req(i >= 1, i <= nrow(fs))
    k <- res@n_factors
    kf <- min(max(as.integer(sel$f %||% 1), 1L), k)
    J <- nrow(fs)
    C <- length(qd@distribution)
    labs <- bq_grid_labels(C)
    ramp <- grDevices::colorRampPalette(ov2_ramp_anchors)(C)

    s_num <- fs$statement_num[i]
    sid <- paste0("S", s_num)
    txt <- if (!is.na(s_num) && s_num <= length(qd@statements)) {
      qd@statements[s_num]
    } else {
      "No statement text available for this dataset."
    }
    col_sel <- match(fs[[paste0("F", kf, "_score")]][i], labs)
    badge_bg <- if (!is.na(col_sel)) ramp[col_sel] else "#236192"

    d <- fr_dist_obj()
    lens_lv <- fr_lens_level()
    stars_of <- function(f) {
      if (f > length(d$distinguishing %||% list())) return(NULL)
      pos <- match(fs$statement[i], d$distinguishing[[f]])
      if (is.na(pos)) return(NULL)
      st <- d$distinguishing_significance[[f]][pos]
      if (nchar(st) >= lens_lv) st else NULL
    }

    per_factor <- lapply(seq_len(k), function(f) {
      z <- fs[[paste0("F", f, "_zscore")]][i]
      qv <- fs[[paste0("F", f, "_score")]][i]
      rk <- fs[[paste0("F", f, "_rank")]][i]
      st <- stars_of(f)
      htmltools::div(
        class = "bq2-pane-fact",
        htmltools::span(class = "bq2-comp-dot",
                        style = paste0("background:", fcol(f), ";")),
        htmltools::span(class = "bq2-comp-name", paste("Factor", f)),
        htmltools::span(
          class = "bq2-comp-facts",
          paste0("q ", fmt_q(qv), " · z ", fmt_m2(z),
                 " · rank ", rk, " of ", J,
                 if (!is.null(st)) paste0(" · distinguishes", st),
                 if (identical(unname(d$consensus_level[fs$statement[i]]), "strict")) {
                   " · consensus"
                 })
        )
      )
    })

    htmltools::div(
      class = "ov2-qs-pane",
      htmltools::div(
        class = "ov2-qs-pane-side",
        htmltools::span(class = "ov2-qs-pane-badge",
                        style = paste0("background:", badge_bg, ";color:",
                                       contrast_text_color(badge_bg), ";"),
                        sid)
      ),
      htmltools::div(
        class = "bq2-pane-body",
        htmltools::div(class = "ov2-qs-pane-text", txt),
        htmltools::div(class = "bq2-pane-facts", per_factor)
      )
    )
  })

  output$fr_scores_head <- shiny::renderUI({
    res <- fr_res()
    shiny::req(res)
    htmltools::div(
      class = "pr-ttl",
      htmltools::tags$b(
        class = "fr-tip",
        title = "z: weighted average of the defining sorts · q: z forced back into the deck · rank variance: spread of a statement's ranks across factors",
        "Factor scores"
      ),
      htmltools::span(sprintf(" · %d statements", nrow(res@factor_scores)))
    )
  })

  output$fr_scores <- DT::renderDT({
    res <- fr_res()
    qd <- rv$qdata
    shiny::req(res, qd)
    fs <- res@factor_scores
    k <- res@n_factors
    d <- fr_dist_obj()

    df <- data.frame(
      Statement = paste0("S", fs$statement_num),
      Text = as.character(qd@statements)[fs$statement_num],
      check.names = FALSE, stringsAsFactors = FALSE
    )
    for (f in seq_len(k)) {
      df[[paste0("F", f)]] <- sprintf(
        "%s (z %s, rank %d)",
        fmt_q(fs[[paste0("F", f, "_score")]]),
        fmt_m2(fs[[paste0("F", f, "_zscore")]]),
        fs[[paste0("F", f, "_rank")]]
      )
    }
    rv_col <- if (is.null(d$ranking_variance)) {
      rep(NA_real_, nrow(fs))
    } else {
      unname(d$ranking_variance[fs$statement])
    }
    df[["Rank variance"]] <- ifelse(is.na(rv_col), "",
                                    sprintf("%.2f", rv_col))
    df <- df[order(-fs$F1_zscore), , drop = FALSE]

    DT::datatable(
      df, escape = TRUE, rownames = FALSE,
      options = list(pageLength = 15, dom = "ftp", scrollX = TRUE),
      class = "compact stripe"
    )
  })

  # Distinguishing & consensus ----

  fr_dist_rows <- shiny::reactive({
    res <- fr_res()
    shiny::req(res)
    d <- fr_dist_obj()
    lens_lv <- fr_lens_level()
    fs <- res@factor_scores
    k <- res@n_factors
    if (k < 2 || length(d$distinguishing) < k) return(NULL)

    out <- list()
    for (f in seq_len(k)) {
      stmts_f <- d$distinguishing[[f]]
      stars_f <- d$distinguishing_significance[[f]]
      if (length(stmts_f) == 0) next
      keep <- nchar(stars_f) >= lens_lv
      if (!any(keep)) next
      idx <- match(stmts_f[keep], fs$statement)
      out[[length(out) + 1]] <- data.frame(
        i = idx, factor = f, stars = stars_f[keep],
        stringsAsFactors = FALSE
      )
    }
    if (length(out) == 0) return(NULL)
    do.call(rbind, out)
  })

  output$fr_dist_head <- shiny::renderUI({
    res <- fr_res()
    shiny::req(res)
    if (res@n_factors < 2) {
      return(htmltools::div(
        class = "bq2-tab-note",
        "1 factor · no distinguishing or consensus tests"
      ))
    }
    rows <- fr_dist_rows()
    n_d <- if (is.null(rows)) 0L else nrow(rows)
    lens <- fr_lens()
    seg <- function(id, label, value) {
      shiny::actionButton(
        ns(id), label,
        class = paste("btn bq2-seg-btn", if (identical(lens, value)) "active")
      )
    }
    htmltools::tagList(
      htmltools::div(
        class = "pr-ttl-row",
        pr_title("Distinguishing statements",
                 sprintf("%d across %d factors", n_d, res@n_factors)),
        htmltools::div(style = "flex: 1;"),
        htmltools::div(
          class = "bq2-seg small",
          seg("fr_lens_05", "p < 0.05", 0.05),
          seg("fr_lens_01", "p < 0.01", 0.01),
          seg("fr_lens_001", "p < 0.001", 0.001)
        ),
        shiny::selectInput(
          ns("fr_dist_from"), NULL,
          choices = c("every other factor" = "all", "any pair" = "any"),
          selected = shiny::isolate(input$fr_dist_from) %||% "all",
          width = "170px", selectize = FALSE
        )
      ),
      htmltools::div(
        class = "pr-ttl-sub",
        htmltools::span(
          class = "fr-tip",
          title = "differences tested against the SED · p is BH-adjusted across every pairwise test",
          "* p < 0.05 · ** p < 0.01"
        )
      )
    )
  })

  output$fr_dist <- DT::renderDT({
    res <- fr_res()
    qd <- rv$qdata
    shiny::req(res, qd)
    rows <- fr_dist_rows()
    shiny::req(!is.null(rows))
    d <- fr_dist_obj()
    fs <- res@factor_scores
    k <- res@n_factors
    pairs <- colnames(d$pairwise_p_adj)

    q_vals <- vapply(seq_len(nrow(rows)), function(r) {
      as.numeric(fs[[paste0("F", rows$factor[r], "_score")]][rows$i[r]])
    }, numeric(1))
    z_vals <- vapply(seq_len(nrow(rows)), function(r) {
      as.numeric(fs[[paste0("F", rows$factor[r], "_zscore")]][rows$i[r]])
    }, numeric(1))

    df <- data.frame(
      Statement = paste0("S", fs$statement_num[rows$i]),
      Text = as.character(qd@statements)[fs$statement_num[rows$i]],
      Factor = sprintf('<span style="color:%s;font-weight:700;">F%d</span>',
                       vapply(rows$factor, fcol, character(1)), rows$factor),
      q = fmt_q(q_vals),
      z = fmt_m2(z_vals),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    diffs <- d$pairwise_differences
    for (pn in pairs) {
      col <- paste0("diff_", sub("-", "_", pn))
      sig <- diffs[[paste0(col, "_sig")]][rows$i]
      df[[pn]] <- paste0(fmt_m2(diffs[[col]][rows$i]), sig)
    }
    pair_members <- strsplit(pairs, "-", fixed = TRUE)
    p_rel <- vapply(seq_len(nrow(rows)), function(r) {
      fname <- paste0("F", rows$factor[r])
      rel <- vapply(pair_members, function(p) fname %in% p, logical(1))
      max(d$pairwise_p_adj[rows$i[r], rel], na.rm = TRUE)
    }, numeric(1))
    df[["p (BH)"]] <- fmt_p_cell(p_rel)
    df <- df[order(p_rel), , drop = FALSE]

    DT::datatable(
      df, escape = FALSE, rownames = FALSE,
      options = list(pageLength = 15, dom = "ftp", scrollX = TRUE),
      class = "compact stripe"
    )
  })

  output$fr_cons_head <- shiny::renderUI({
    res <- fr_res()
    shiny::req(res, res@n_factors >= 2)
    d <- fr_dist_obj()
    n_c <- sum(d$consensus_level != "")
    htmltools::div(
      class = "pr-ttl",
      htmltools::tags$b(
        class = "fr-tip",
        title = "strict: no pairwise difference at p < 0.05 · broad: none at p < 0.01",
        "Consensus statements"
      ),
      htmltools::span(sprintf(" · %d of %d", n_c, nrow(res@factor_scores)))
    )
  })

  output$fr_cons <- DT::renderDT({
    res <- fr_res()
    qd <- rv$qdata
    shiny::req(res, qd)
    d <- fr_dist_obj()
    fs <- res@factor_scores
    keep <- which(d$consensus_level != "")
    shiny::req(length(keep) > 0)

    zr <- d$z_range[keep, , drop = FALSE]
    df <- data.frame(
      Statement = paste0("S", fs$statement_num[keep]),
      Text = as.character(qd@statements)[fs$statement_num[keep]],
      `z range` = sprintf("%s to %s", fmt_m2(zr[, "min"]), fmt_m2(zr[, "max"])),
      `Max difference` = sprintf("%.2f", zr[, "max"] - zr[, "min"]),
      Level = unname(d$consensus_level[keep]),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    df <- df[order(df$Level, decreasing = TRUE), , drop = FALSE]

    DT::datatable(
      df, escape = TRUE, rownames = FALSE,
      options = list(pageLength = 15, dom = "ftp", scrollX = TRUE),
      class = "compact stripe"
    )
  })

  # Crib sheets ----

  output$fr_crib_head <- shiny::renderUI({
    res <- fr_res()
    shiny::req(res)
    k <- res@n_factors
    f <- as.integer(input$fr_crib_factor %||% 1)
    f <- min(max(f, 1L), k)
    cs <- tryCatch(generate_crib_sheet(res, factor = f),
                   error = function(e) NULL)
    n_items <- if (is.null(cs)) 0L else {
      sum(vapply(list(cs$most_agree, cs$higher_than_all,
                      cs$lower_than_all, cs$most_disagree),
                 function(x) if (is.data.frame(x)) nrow(x) else 0L,
                 integer(1)))
    }
    htmltools::tagList(
      htmltools::div(
        class = "pr-ttl-row",
        fr_title(sprintf("Crib sheet · Factor %d", f),
                 sprintf("%d defining sorts", sum(res@flagging[, f])),
                 sprintf("%d statements", n_items),
                 tip = "Watts and Stenner's four sections"),
        htmltools::div(style = "flex: 1;"),
        shiny::selectInput(
          ns("fr_crib_factor"), NULL,
          choices = stats::setNames(seq_len(k), paste("Factor", seq_len(k))),
          selected = f, width = "140px", selectize = FALSE
        )
      ),
      htmltools::div(class = "pr-ttl-sub", "D distinguishing · C consensus")
    )
  })

  output$fr_crib_body <- shiny::renderUI({
    res <- fr_res()
    qd <- rv$qdata
    shiny::req(res, qd)
    k <- res@n_factors
    f <- min(max(as.integer(input$fr_crib_factor %||% 1), 1L), k)
    cs <- tryCatch(generate_crib_sheet(res, factor = f),
                   error = function(e) NULL)
    shiny::req(cs)
    deck_max <- max(bq_grid_labels(length(qd@distribution)))
    others <- setdiff(seq_len(k), f)

    crib_table <- function(df) {
      if (!is.data.frame(df) || nrow(df) == 0) {
        return(htmltools::div(class = "pr-ttl-sub", "none"))
      }
      other_cols <- paste0("F", others, "_score")
      other_cols <- other_cols[other_cols %in% names(df)]
      other_labels <- paste0("F", sub("_score", "", sub("^F", "", other_cols)),
                             " q")
      # Numeric headers right-align over their right-aligned values
      header_cells <- c(
        list(htmltools::tags$th("Statement"), htmltools::tags$th("Text")),
        lapply(c("q", "z", other_labels), function(h) {
          htmltools::tags$th(class = "r", h)
        }),
        list(htmltools::tags$th("Mark"))
      )
      rows <- lapply(seq_len(nrow(df)), function(r) {
        htmltools::tags$tr(
          htmltools::tags$td(paste0("S", df$statement_num[r])),
          htmltools::tags$td(df$statement[r]),
          htmltools::tags$td(class = "r", fmt_q(df$score[r])),
          htmltools::tags$td(class = "r", fmt_m2(df$zscore[r])),
          lapply(other_cols, function(col) {
            htmltools::tags$td(class = "r", fmt_q(df[[col]][r]))
          }),
          htmltools::tags$td(
            ifelse(is.na(df$marker[r]) | df$marker[r] == "", "",
                   df$marker[r])
          )
        )
      })
      htmltools::div(
        class = "fr-tbl-scroll",
        htmltools::tags$table(
          class = "fr-tbl",
          htmltools::tags$thead(htmltools::tags$tr(header_cells)),
          htmltools::tags$tbody(rows)
        )
      )
    }

    htmltools::tagList(
      pr_title(sprintf("Highest (+%d)", deck_max)),
      crib_table(cs$most_agree),
      pr_title("Higher than in any other array"),
      crib_table(cs$higher_than_all),
      pr_title("Lower than in any other array"),
      crib_table(cs$lower_than_all),
      pr_title(sprintf("Lowest (−%d)", deck_max)),
      crib_table(cs$most_disagree)
    )
  })

  # Participants: who holds each view ----

  fr_membership <- shiny::reactive({
    res <- fr_res()
    shiny::req(res)
    flags <- res@flagging
    k <- ncol(flags)
    m <- ifelse(rowSums(flags) > 0,
                paste0("F", max.col(flags + 0, ties.method = "first")),
                "None")
    factor(m, levels = c(paste0("F", seq_len(k)), "None"))
  })

  output$fr_part_head <- shiny::renderUI({
    res <- fr_res()
    qd <- rv$qdata
    shiny::req(res, qd)
    if (!has_participant_attributes(qd)) {
      return(htmltools::div(
        class = "bq2-tab-note",
        "Add demographics on the Priorities view."
      ))
    }
    groups <- attribute_groups(qd)
    fg <- groups[vapply(groups, is.factor, logical(1))]
    shiny::req(length(fg) > 0)
    a <- input$fr_part_attr %||% names(fg)[1]
    if (!a %in% names(fg)) a <- names(fg)[1]

    v <- fg[[a]]
    memb <- fr_membership()
    ok <- !is.na(v)
    tab <- table(droplevels(v[ok]), memb[ok])
    test_line <- tryCatch({
      exp_ok <- all(suppressWarnings(stats::chisq.test(tab))$expected >= 5)
      if (exp_ok) {
        ct <- suppressWarnings(stats::chisq.test(tab))
        sprintf("X²(%d) = %.1f · p = %.3f · row %%",
                ct$parameter, ct$statistic, ct$p.value)
      } else {
        ft <- stats::fisher.test(tab, simulate.p.value = TRUE, B = 5000)
        sprintf("Fisher's exact p = %.3f · row %%", ft$p.value)
      }
    }, error = function(e) "row %")

    htmltools::tagList(
      htmltools::div(
        class = "pr-ttl-row",
        pr_title("Who holds each view",
                 sprintf("%d of %d with demographics",
                         sum(ok), nrow(res@flagging))),
        htmltools::div(style = "flex: 1;"),
        shiny::selectInput(
          ns("fr_part_attr"), NULL,
          choices = stats::setNames(names(fg),
                                    vapply(names(fg), pr_attr_label,
                                           character(1))),
          selected = a, width = "180px", selectize = FALSE
        )
      ),
      htmltools::div(class = "pr-ttl-sub", test_line)
    )
  })

  output$fr_part <- DT::renderDT({
    res <- fr_res()
    qd <- rv$qdata
    shiny::req(res, qd, has_participant_attributes(qd))
    groups <- attribute_groups(qd)
    fg <- groups[vapply(groups, is.factor, logical(1))]
    shiny::req(length(fg) > 0)
    a <- input$fr_part_attr %||% names(fg)[1]
    if (!a %in% names(fg)) a <- names(fg)[1]
    v <- fg[[a]]
    memb <- fr_membership()
    ok <- !is.na(v)
    tab <- table(droplevels(v[ok]), memb[ok])

    df <- data.frame(Group = rownames(tab),
                     n = as.integer(rowSums(tab)),
                     check.names = FALSE, stringsAsFactors = FALSE)
    for (cn in colnames(tab)) {
      df[[cn]] <- sprintf("%d (%.0f%%)", tab[, cn],
                          100 * tab[, cn] / pmax(1, rowSums(tab)))
    }

    DT::datatable(
      df, escape = TRUE, rownames = FALSE,
      options = list(dom = "t", ordering = FALSE, pageLength = nrow(df)),
      class = "compact stripe"
    )
  })

  # Step 4: Export: bootstrap verdict, audit trail, workbook ----

  output$fr_stab_head <- shiny::renderUI({
    res <- fr_res()
    shiny::req(res)
    boot <- rv$fr_boot

    if (is.null(boot)) {
      return(fr_title(
        "Bootstrap stability", "not yet run",
        "about a minute per 500 resamples",
        tip = "resamples sorts with replacement and re-fits the full solution"
      ))
    }

    bs <- boot$bs
    lo <- bs@loading_ci
    w_med <- stats::median(lo$ci_upper - lo$ci_lower, na.rm = TRUE)
    res_l <- res@rotation$loadings
    boot_mean <- matrix(lo$boot_mean, nrow = nrow(res_l))
    tc <- diag(tucker_congruence(res_l, boot_mean))
    flags <- res@flagging
    def_i <- which(rowSums(flags) > 0)
    agree <- if (length(def_i) > 0) {
      mean(bs@flag_frequency[cbind(def_i,
                                   max.col(flags[def_i, , drop = FALSE] + 0,
                                           ties.method = "first"))])
    } else {
      NA_real_
    }
    weak <- which(tc < 0.90)

    headline <- if (length(weak) == 0) {
      "Solution is stable"
    } else if (length(weak) >= length(tc)) {
      "Solution is unstable"
    } else if (length(weak) == 1) {
      sprintf("Stable except Factor %d", weak)
    } else {
      paste0("Stable except Factors ",
             paste(weak[-length(weak)], collapse = ", "),
             " and ", weak[length(weak)])
    }
    status <- paste(c(
      sprintf("loading CI width %.2f", w_med),
      if (!is.na(agree)) sprintf("flag agreement %.0f%%", 100 * agree),
      paste0("congruence ", paste(sprintf("%.2f", tc), collapse = " · ")),
      sprintf("%d resamples", bs@n_bootstrap)
    ), collapse = " · ")

    htmltools::div(
      class = "bq2-verdict pr-verdict",
      htmltools::div(
        class = "bq2-verdict-main",
        htmltools::div(class = "bq2-verdict-headline", headline),
        htmltools::div(
          class = "bq2-verdict-status",
          htmltools::span(class = "bq2-status-rest", status)
        )
      )
    )
  })

  output$fr_stale_chip <- shiny::renderUI({
    boot <- rv$fr_boot
    if (is.null(boot) || !isTRUE(boot$stale)) return(NULL)
    htmltools::span(class = "fr-chip", "Stale · flags changed")
  })

  shiny::observeEvent(input$fr_run_boot, {
    res <- rv$results
    shiny::req(res, rv$qdata)
    B <- max(100, min(2000, as.integer(input$fr_boot_n %||% 500)))
    shiny::withProgress(message = sprintf("Bootstrapping, %d resamples", B),
                        value = 0.3, {
      bs <- tryCatch(
        suppressMessages(qsort_bootstrap(
          rv$qdata, n_bootstrap = B, nfactors = res@n_factors,
          extraction = res@method_details$extraction,
          rotation = {
            # After a manual rotation the stored string carries a
            # "+manual(..deg)" suffix qsort_analyze would reject
            rot <- sub("\\+manual\\(.*$", "", res@method_details$rotation)
            if (identical(rot, "none")) "varimax" else rot
          },
          seed = 42, parallel = FALSE, progress = FALSE
        )),
        error = function(e) e
      )
      shiny::incProgress(0.7)
    })
    if (inherits(bs, "error")) {
      session$sendCustomMessage("showToast", list(
        message = paste("Bootstrap failed:", conditionMessage(bs)),
        type = "error", duration = 6000
      ))
    } else {
      rv$fr_boot <- list(bs = bs, stale = FALSE)
      session$sendCustomMessage("showToast", list(
        message = sprintf("Bootstrap finished · %d resamples", B),
        type = "success"
      ))
    }
  })

  output$fr_boot_load <- DT::renderDT({
    res <- fr_res()
    boot <- rv$fr_boot
    shiny::req(res, boot)
    bs <- boot$bs
    lo <- bs@loading_ci
    k <- res@n_factors
    ids <- unique(lo$item)

    df <- data.frame(Sort = ids, check.names = FALSE,
                     stringsAsFactors = FALSE)
    for (f in seq_len(k)) {
      sub <- lo[lo$factor == paste0("F", f), ]
      sub <- sub[match(ids, sub$item), ]
      df[[paste0("F", f)]] <- sprintf(
        "%s [%s, %s]",
        fmt_z(sub$original), fmt_z(sub$ci_lower), fmt_z(sub$ci_upper)
      )
    }
    ff <- bs@flag_frequency[match(ids, rownames(bs@flag_frequency)), ,
                            drop = FALSE]
    best <- max.col(ff, ties.method = "first")
    df[["Flag frequency"]] <- sprintf(
      "F%d · %.0f%%", best, 100 * ff[cbind(seq_along(ids), best)]
    )

    DT::datatable(
      df, escape = TRUE, rownames = FALSE,
      options = list(pageLength = 15, dom = "ftp", scrollX = TRUE),
      class = "compact stripe"
    )
  })

  output$fr_boot_scores_head <- shiny::renderUI({
    boot <- rv$fr_boot
    res <- fr_res()
    shiny::req(res, boot)
    f <- min(max(as.integer(input$fr_boot_factor %||% 1), 1L), res@n_factors)
    htmltools::div(
      class = "pr-ttl-row",
      pr_title("Score intervals", sprintf("Factor %d", f)),
      htmltools::div(style = "flex: 1;"),
      shiny::selectInput(
        ns("fr_boot_factor"), NULL,
        choices = stats::setNames(seq_len(res@n_factors),
                                  paste("Factor", seq_len(res@n_factors))),
        selected = f, width = "140px", selectize = FALSE
      )
    )
  })

  output$fr_boot_scores <- DT::renderDT({
    res <- fr_res()
    qd <- rv$qdata
    boot <- rv$fr_boot
    shiny::req(res, qd, boot)
    bs <- boot$bs
    f <- min(max(as.integer(input$fr_boot_factor %||% 1), 1L), res@n_factors)
    sc <- bs@score_ci
    sub <- sc[sc$factor == paste0("F", f), ]
    fs <- res@factor_scores
    idx <- match(sub$item, fs$statement)
    rk <- fs[[paste0("F", f, "_rank")]][idx]
    n_st <- nrow(fs)
    rank_lo <- rank(-sub$ci_upper, ties.method = "min")
    rank_hi <- rank(-sub$ci_lower, ties.method = "min")

    df <- data.frame(
      Statement = paste0("S", fs$statement_num[idx]),
      Text = as.character(qd@statements)[fs$statement_num[idx]],
      z = fmt_m2(sub$original),
      `95% CI` = sprintf("[%s, %s]", fmt_z(sub$ci_lower), fmt_z(sub$ci_upper)),
      Rank = rk,
      `Rank range` = sprintf("%d to %d", pmin(rank_lo, rank_hi),
                             pmax(rank_lo, rank_hi)),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    df <- df[order(df$Rank), , drop = FALSE]

    DT::datatable(
      df, escape = TRUE, rownames = FALSE,
      options = list(pageLength = 15, dom = "ftp", scrollX = TRUE),
      class = "compact stripe"
    )
  })

  # The audit trail under the step-4 Details

  output$fr_m_chars_head <- shiny::renderUI({
    res <- fr_res()
    shiny::req(res)
    md <- res@method_details
    r <- md$av_rel_coef %||% 0.8
    fr_title("Factor characteristics",
             md$rotation,
             toupper(md$extraction),
             tools::toTitleCase(md$cor_method),
             tip = sprintf("reliability = %.2fn/(1+(n-1)%.2f) with n = the defining sorts you actually flagged",
                           r, r))
  })

  output$fr_m_chars <- DT::renderDT({
    res <- fr_res()
    shiny::req(res)
    ch <- res@factor_characteristics$characteristics
    fs <- res@factor_scores
    w <- attr(fs, "factor_weights")
    df <- data.frame(
      Factor = paste("Factor", seq_len(nrow(ch))),
      `Defining sorts` = ch$n_flagged,
      `Weights sum` = vapply(seq_len(nrow(ch)), function(f) {
        wf <- w[[paste0("F", f)]]
        if (is.null(wf)) NA_real_ else round(sum(abs(wf)), 2)
      }, numeric(1)),
      `Composite reliability` = sprintf("%.3f", ch$composite_reliability),
      `SE of scores` = sprintf("%.3f", ch$se_factor),
      `Variance %` = sprintf("%.1f", ch$variance_pct),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    DT::datatable(df, escape = TRUE, rownames = FALSE,
                  options = list(dom = "t", ordering = FALSE),
                  class = "compact stripe")
  })

  output$fr_m_sed_head <- shiny::renderUI({
    fr_title("Standard errors of differences",
             tip = "the SED matrix behind every distinguishing test")
  })

  output$fr_m_sed <- DT::renderDT({
    res <- fr_res()
    shiny::req(res)
    sed <- res@factor_characteristics$sed_matrix
    k <- nrow(sed)
    m <- round(sed, 3)
    df <- data.frame(Factor = paste("Factor", seq_len(k)), m,
                     check.names = FALSE)
    names(df)[-1] <- paste("Factor", seq_len(k))
    DT::datatable(df, escape = TRUE, rownames = FALSE,
                  options = list(dom = "t", ordering = FALSE),
                  class = "compact stripe")
  })

  output$fr_m_fsc_head <- shiny::renderUI({
    res <- fr_res()
    shiny::req(res)
    md <- res@method_details
    oblique <- md$rotation %in% c("promax", "oblimin")
    fr_title("Factor score correlations",
             sprintf("%d x %d", res@n_factors, res@n_factors),
             tip = if (oblique) {
               sprintf("correlated by construction under %s", md$rotation)
             })
  })

  output$fr_m_fsc <- DT::renderDT({
    res <- fr_res()
    shiny::req(res)
    k <- res@n_factors
    zmat <- as.matrix(
      res@factor_scores[, paste0("F", seq_len(k), "_zscore"), drop = FALSE]
    )
    fsc <- round(stats::cor(zmat, use = "pairwise.complete.obs"), 3)
    df <- data.frame(Factor = paste("Factor", seq_len(k)), fsc,
                     check.names = FALSE)
    names(df)[-1] <- paste("Factor", seq_len(k))
    DT::datatable(df, escape = TRUE, rownames = FALSE,
                  options = list(dom = "t", ordering = FALSE),
                  class = "compact stripe")
  })

  output$fr_m_unrot_head <- shiny::renderUI({
    res <- fr_res()
    shiny::req(res)
    uw <- res@extraction$unrotated_wide
    shiny::req(!is.null(uw))
    pr_title("Unrotated loadings",
             sprintf("first %d components", ncol(uw)),
             "with cumulative communality")
  })

  output$fr_m_unrot <- DT::renderDT({
    res <- fr_res()
    shiny::req(res)
    uw <- res@extraction$unrotated_wide
    cc <- res@extraction$cumulative_communalities
    shiny::req(!is.null(uw))
    df <- data.frame(Sort = rownames(uw), round(uw, 3),
                     `Cumulative h²` = round(cc[, ncol(cc)], 3),
                     check.names = FALSE)
    DT::datatable(df, escape = TRUE, rownames = FALSE,
                  options = list(pageLength = 15, dom = "ftp",
                                 scrollX = TRUE),
                  class = "compact stripe")
  })

  output$fr_m_cor_head <- shiny::renderUI({
    res <- fr_res()
    qd <- rv$qdata
    shiny::req(res, qd)
    n <- nrow(qd@sorts)
    pr_title("Correlation matrix",
             tools::toTitleCase(res@method_details$cor_method),
             sprintf("%d x %d", n, n))
  })

  output$fr_m_cor <- DT::renderDT({
    res <- fr_res()
    qd <- rv$qdata
    shiny::req(res, qd)
    cm <- round(res@correlation, 2)
    fd <- sort_descriptives(qd)
    forced <- length(unique(apply(qd@sorts, 1, function(r) {
      paste(sort(table(r)), collapse = ",")
    }))) == 1
    df <- data.frame(Sort = rownames(cm), check.names = FALSE,
                     stringsAsFactors = FALSE)
    if (!forced) {
      df$Mean <- round(fd$mean, 2)
      df$SD <- round(fd$sd, 2)
    }
    df <- cbind(df, cm)
    DT::datatable(df, escape = TRUE, rownames = FALSE,
                  options = list(pageLength = 10, dom = "ftp",
                                 scrollX = TRUE),
                  class = "compact stripe")
  })

  output$fr_m_defcor <- shiny::renderUI({
    res <- fr_res()
    shiny::req(res)
    dc <- defining_sort_correlations(res)
    line <- paste(vapply(dc, function(x) {
      sprintf("F%d %s", x$factor,
              if (is.na(x$mean_r)) "n/a" else sprintf("%.2f", x$mean_r))
    }, character(1)), collapse = " · ")
    htmltools::tagList(
      pr_title("Defining-sort correlations", "mean r within each factor"),
      htmltools::div(class = "pr-ttl-sub", line)
    )
  })

  output$fr_workbook <- shiny::downloadHandler(
    filename = function() {
      paste0("canhrqsort_factors_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      shiny::req(rv$results)
      shiny::withProgress(message = "Building workbook", value = 0.4, {
        export_factor_workbook(rv$results, file)
        shiny::incProgress(0.6)
      })
    }
  )

  # Everything lives in step panels or Details blocks that start hidden;
  # without this the outputs stay suspended and never render on reveal.
  for (o in c("fr_step_sync", "fr_pre_verdict", "fr_scree", "fr_ret_head",
              "fr_ret_tbl", "fr_k_input", "fr_extract_btn",
              "fr_step1_settings",
              "fr_verdict", "fr_rotation_input", "fr_flag_settings",
              "fr_flag_chip", "fr_load_head", "fr_loadings",
              "fr_bipolar_line", "fr_rot_controls", "fr_rot_plot",
              "fr_int_tabbar",
              "fr_arrays", "fr_array_pane", "fr_scores_head", "fr_scores",
              "fr_dist_head", "fr_dist", "fr_cons_head", "fr_cons",
              "fr_crib_head", "fr_crib_body",
              "fr_part_head", "fr_part",
              "fr_stab_head", "fr_stale_chip", "fr_boot_load",
              "fr_boot_scores_head", "fr_boot_scores",
              "fr_m_chars_head", "fr_m_chars",
              "fr_m_sed_head", "fr_m_sed", "fr_m_fsc_head", "fr_m_fsc",
              "fr_m_unrot_head", "fr_m_unrot", "fr_m_cor_head", "fr_m_cor",
              "fr_m_defcor", "fr_workbook")) {
    shiny::outputOptions(output, o, suspendWhenHidden = FALSE)
  }

  invisible(NULL)
}
