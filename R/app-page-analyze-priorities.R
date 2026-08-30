#' @title Priorities Panel (Frequentist page, quick layer)
#' @description The descriptive layer of the Frequentist page: overall
#' statement rankings the moment data loads, participant-attribute upload
#' with an explicit match report, grouping controls, subgroup comparisons
#' behind per-attribute gates, the rank-flow chart, and a workbook export.
#' Pure UI and server binding; every computation lives in priorities.R and
#' attributes.R.
#' @name app-page-analyze-priorities
#' @keywords internal
NULL

#' Priorities body UI (rendered inside the analyze page)
#' @keywords internal
# Human label for an attribute name ("agegroup" -> "age group")
pr_attr_label <- function(x) {
  x <- gsub("[._]+", " ", x)
  gsub("agegroup", "age group", x, ignore.case = TRUE)
}

# Statistical title line above a tab's content: bold name, muted facts
pr_title <- function(main, ...) {
  facts <- c(...)
  htmltools::div(
    class = "pr-ttl",
    htmltools::tags$b(main),
    if (length(facts) > 0) {
      htmltools::span(paste0(" · ", paste(facts, collapse = " · ")))
    }
  )
}

pr_tab_ids <- c("pr_tab_ranked", "pr_tab_part", "pr_tab_flow",
                "pr_tab_diff", "pr_tab_stats")
pr_panel_ids <- c("panel_ranked", "panel_part", "panel_flow",
                  "panel_diff", "panel_stats")

pr_js_array <- function(ids) paste0("['", paste(ids, collapse = "','"), "']")

# Tabs switch client-side so they respond even while R is mid-computation
pr_tab_link <- function(ns, id, label, active = FALSE) {
  htmltools::tags$a(
    id = ns(id), href = "#",
    class = paste(c("ov2-tab", if (active) "active"), collapse = " "),
    onclick = sprintf("return canhrPrTab(this, %s, %s);",
                      pr_js_array(vapply(pr_tab_ids, ns, character(1))),
                      pr_js_array(vapply(pr_panel_ids, ns, character(1)))),
    label
  )
}

priorities_body_ui <- function(ns) {
  htmltools::tagList(
    shiny::uiOutput(ns("pr_verdict")),

    # Attributes panel: upload, match report, grouping controls
    htmltools::div(
      id = ns("pr_attr_panel"),
      class = "bq2-settings collapsed",
      htmltools::div(
        class = "pr-attr-grid",
        htmltools::div(
          class = "pr-attr-upload",
          htmltools::div(class = "bq2-label", "Demographics file"),
          shiny::fileInput(
            ns("pr_attr_file"), NULL,
            accept = c(".csv", ".txt", ".xlsx", ".xls"),
            buttonLabel = "Browse", placeholder = "CSV or Excel with an id column"
          ),
          htmltools::div(
            class = "pr-attr-hint",
            "One row per participant: an id column plus any demographic ",
            "columns (gender, age). The id column is found by matching its ",
            "values against the sort ids."
          )
        ),
        htmltools::div(
          class = "pr-attr-report",
          shiny::uiOutput(ns("pr_match_report"))
        )
      )
    ),

    # Results card: Ranked / Participants / Flow / Differences
    htmltools::div(
      class = "ov2-card pr-card",
      htmltools::div(
        class = "ov2-tabbar",
        pr_tab_link(ns, "pr_tab_ranked", "Ranking", active = TRUE),
        pr_tab_link(ns, "pr_tab_part", "Participants"),
        pr_tab_link(ns, "pr_tab_flow", "Flow"),
        pr_tab_link(ns, "pr_tab_diff", "Group ranking"),
        pr_tab_link(ns, "pr_tab_stats", "Tests")
      ),
      htmltools::div(
        class = "ov2-card-body",
        htmltools::div(
          id = ns("panel_ranked"),
          shiny::uiOutput(ns("pr_rank_title")),
          DT::DTOutput(ns("pr_ranked"))
        ),
        htmltools::div(
          id = ns("panel_part"),
          style = "display: none;",
          shiny::uiOutput(ns("pr_part_head")),
          shiny::uiOutput(ns("pr_part_plot_ui")),
          shiny::uiOutput(ns("pr_grouping_editor"))
        ),
        htmltools::div(
          id = ns("panel_flow"),
          style = "display: none;",
          shiny::uiOutput(ns("pr_flow_title")),
          shiny::uiOutput(ns("pr_flow_controls")),
          shiny::plotOutput(ns("pr_flow_plot"), height = "560px",
                            click = ns("pr_flow_click")),
          shiny::uiOutput(ns("pr_flow_pane"))
        ),
        htmltools::div(
          id = ns("panel_diff"),
          style = "display: none;",
          shiny::uiOutput(ns("pr_diff_controls")),
          DT::DTOutput(ns("pr_diff"))
        ),
        htmltools::div(
          id = ns("panel_stats"),
          style = "display: none;",
          shiny::uiOutput(ns("pr_stats_controls")),
          DT::DTOutput(ns("pr_stats"))
        )
      )
    )
  )
}

#' Priorities control-bar actions (right side of the page bar)
#' @keywords internal
priorities_bar_ui <- function(ns) {
  htmltools::tagList(
    shiny::uiOutput(ns("pr_attr_button"), inline = TRUE),
    shiny::uiOutput(ns("pr_workbook_area"), inline = TRUE)
  )
}

#' Bind the Priorities server logic inside the analyze module server
#' @keywords internal
priorities_server_bind <- function(input, output, session, rv, ns) {

  pr_stage <- shiny::reactiveValues(attrs = NULL, match = NULL, name = NULL)

  pr_groups <- shiny::reactive({
    qd <- rv$qdata
    shiny::req(qd)
    if (has_participant_attributes(qd)) attribute_groups(qd) else NULL
  })

  pr_res <- shiny::reactive({
    qd <- rv$qdata
    shiny::req(qd)
    compute_priorities(qd, groups = pr_groups(), q = 0.05)
  })

  # Tabs switch client-side (see pr_tab_link). The heavy computation after a
  # data change is pulled through here, inside the observer that caused it,
  # so the user sees a progress bar instead of a frozen page.
  pr_precompute <- function(message = "Running the group tests") {
    shiny::withProgress(message = message, value = 0.5, {
      tryCatch(pr_res(), error = function(e) NULL)
    })
    invisible(NULL)
  }

  # Verdict ----
  output$pr_verdict <- shiny::renderUI({
    pr <- pr_res()
    tab <- pr$table
    n_flag <- sum(tab$neutral_label %in% c("Prioritized", "Deprioritized"))
    J <- nrow(tab)
    has_attrs <- has_participant_attributes(rv$qdata)
    conc <- pr$concordance

    diff_counts <- if (!is.null(pr$tests) && nrow(pr$tests) > 0) {
      fl <- pr$tests[pr$tests$flagged & pr$tests$test != "wilcoxon_pratt", , drop = FALSE]
      if (nrow(fl)) table(fl$attribute) else NULL
    }

    headline <- if (!has_attrs) {
      sprintf("%d of %d statements %s from neutral", n_flag, J,
              if (n_flag == 1) "departs" else "depart")
    } else if (is.null(diff_counts)) {
      "No group differences flagged"
    } else if (length(diff_counts) == 1) {
      k <- diff_counts[[1]]
      sprintf("%d %s by %s", k,
              if (k == 1) "priority differs" else "priorities differ",
              pr_attr_label(names(diff_counts)[1]))
    } else {
      sprintf("%d priorities differ across groups", sum(diff_counts))
    }

    top_row <- tab[1, ]
    status_bits <- c(
      sprintf("Panel agreement W = %.2f, %s", conc$W, conc$band),
      sprintf("top statement %s at %+.2f", top_row$stmt, top_row$mean),
      "flags at q = 0.05"
    )

    gate_note <- if (has_attrs && !is.null(pr$gates) && nrow(pr$gates) > 0) {
      failed <- pr$gates$attribute[!pr$gates$pass]
      if (length(failed)) {
        sprintf("No overall group difference for %s; group rankings stay descriptive.",
                paste(pr_attr_label(failed), collapse = ", "))
      }
    }

    htmltools::div(
      class = "bq2-verdict pr-verdict",
      htmltools::div(
        class = "bq2-verdict-main",
        htmltools::div(class = "bq2-verdict-headline", headline),
        htmltools::div(
          class = "bq2-verdict-status",
          htmltools::span(class = "bq2-status-rest",
                          paste(status_bits, collapse = " · "))
        ),
        if (!is.null(gate_note)) {
          htmltools::div(class = "bq2-verdict-body pr-gate-note", gate_note)
        }
      )
    )
  })

  # Attributes: bar button, upload, match report, attach ----
  output$pr_attr_button <- shiny::renderUI({
    qd <- rv$qdata
    shiny::req(qd)
    label <- if (has_participant_attributes(qd)) {
      attrs <- participant_attributes(qd)
      n_attr <- ncol(attrs) - 1
      n_match <- sum(stats::complete.cases(attrs[, -1, drop = FALSE]))
      paste0("Demographics: ", n_attr, " · ", n_match, " of ",
             nrow(attrs), " matched")
    } else {
      "Add demographics"
    }
    shiny::actionButton(ns("pr_toggle_attrs"), label, class = "btn-ov2-blue bq2-run")
  })

  toggle_attr_panel <- function() {
    session$sendCustomMessage("toggleClass", list(
      id = ns("pr_attr_panel"), className = "collapsed"
    ))
  }
  shiny::observeEvent(input$pr_toggle_attrs, toggle_attr_panel())

  shiny::observeEvent(input$pr_attr_file, {
    shiny::req(input$pr_attr_file, rv$qdata)
    tryCatch({
      attrs <- read_participant_attributes(input$pr_attr_file$datapath)
      m <- match_participant_attributes(rv$qdata@participants, attrs)
      pr_stage$attrs <- attrs
      pr_stage$match <- m
      pr_stage$name <- input$pr_attr_file$name
    }, error = function(e) {
      pr_stage$attrs <- NULL
      pr_stage$match <- NULL
      session$sendCustomMessage("showToast", list(
        message = paste("Demographics error:", conditionMessage(e)),
        type = "error", duration = 6000
      ))
    })
  })

  output$pr_match_report <- shiny::renderUI({
    qd <- rv$qdata
    shiny::req(qd)

    if (!is.null(pr_stage$match)) {
      return(htmltools::div(
        htmltools::div(class = "bq2-label", paste0("Match report: ", pr_stage$name)),
        htmltools::div(
          class = "pr-report-lines",
          lapply(pr_stage$match$messages, function(m) {
            htmltools::div(class = "pr-report-line", m)
          })
        ),
        htmltools::div(
          class = "pr-report-actions",
          shiny::actionButton(ns("pr_attach"), "Attach demographics",
                              class = "btn-ov2-blue bq2-run"),
          shiny::actionButton(ns("pr_discard"), "Discard",
                              class = "btn-ov2-blue bq2-run")
        )
      ))
    }

    if (has_participant_attributes(qd)) {
      rep <- qd@metadata$attribute_match
      htmltools::div(
        htmltools::div(class = "bq2-label", "Attached demographics"),
        if (!is.null(rep$messages)) {
          htmltools::div(
            class = "pr-report-lines",
            lapply(rep$messages, function(m) {
              htmltools::div(class = "pr-report-line", m)
            })
          )
        } else {
          htmltools::div(class = "pr-report-line",
                         "Demographics are attached to this dataset.")
        },
        htmltools::div(
          class = "pr-report-actions",
          shiny::actionButton(ns("pr_remove_attrs"), "Remove demographics",
                              class = "btn-ov2-blue bq2-run")
        )
      )
    } else {
      htmltools::div(class = "pr-report-line pr-report-empty",
                     "No demographics attached yet. Upload a file to begin.")
    }
  })

  shiny::observeEvent(input$pr_attach, {
    shiny::req(pr_stage$attrs, rv$qdata)
    tryCatch({
      rv$qdata <- attach_participant_attributes(
        rv$qdata, pr_stage$attrs, id_col = pr_stage$match$id_col
      )
      pr_precompute("Running the group tests")
      session$sendCustomMessage("showToast", list(
        message = sprintf("Demographics attached: matched %d of %d sorts.",
                          pr_stage$match$matched,
                          length(rv$qdata@participants)),
        type = "success"
      ))
      pr_stage$attrs <- NULL
      pr_stage$match <- NULL
    }, error = function(e) {
      session$sendCustomMessage("showToast", list(
        message = paste("Attach failed:", conditionMessage(e)),
        type = "error", duration = 6000
      ))
    })
  })

  shiny::observeEvent(input$pr_discard, {
    pr_stage$attrs <- NULL
    pr_stage$match <- NULL
  })

  shiny::observeEvent(input$pr_remove_attrs, {
    qd <- rv$qdata
    shiny::req(qd)
    qd@metadata$participant_attributes <- NULL
    qd@metadata$attribute_spec <- NULL
    qd@metadata$attribute_match <- NULL
    qd@metadata$demographics <- NULL
    rv$qdata <- qd
    pr_precompute("Updating")
    session$sendCustomMessage("showToast", list(
      message = "Demographics removed.", type = "info"
    ))
  })

  # Grouping editor ----
  # Raw levels of a grouped attribute, mirroring set_attribute_binning
  pr_raw_levels <- function(sp, attrs, a) {
    lv <- sp$levels
    if (is.null(lv)) {
      lv <- unique(trimws(as.character(attrs[[a]][!is.na(attrs[[a]])])))
    }
    lv
  }

  # Parse range-band labels: "18-30" -> c(18, 30), "71+" -> c(71, Inf).
  # Non-range labels come back NA and never take part in absorption.
  pr_parse_range <- function(x) {
    x <- trimws(as.character(x))
    lo <- rep(NA_real_, length(x))
    hi <- rep(NA_real_, length(x))
    m1 <- regmatches(x, regexec(
      "^([0-9]+(?:\\.[0-9]+)?)\\s*-\\s*([0-9]+(?:\\.[0-9]+)?)$", x))
    m2 <- regmatches(x, regexec("^([0-9]+(?:\\.[0-9]+)?)\\s*\\+$", x))
    for (i in seq_along(x)) {
      if (length(m1[[i]]) == 3) {
        lo[i] <- as.numeric(m1[[i]][2])
        hi[i] <- as.numeric(m1[[i]][3])
      } else if (length(m2[[i]]) == 2) {
        lo[i] <- as.numeric(m2[[i]][2])
        hi[i] <- Inf
      }
    }
    cbind(lo, hi)
  }

  output$pr_grouping_editor <- shiny::renderUI({
    qd <- rv$qdata
    shiny::req(qd, has_participant_attributes(qd))
    spec <- qd@metadata$attribute_spec
    attrs <- participant_attributes(qd)
    groups_all <- attribute_groups(qd)
    fcolors <- get_theme_colors()$factor_colors
    a_names <- setdiff(names(attrs), "participant")

    editors <- lapply(seq_along(a_names), function(ai) {
      a <- a_names[ai]
      sp <- spec[[a]]
      if (is.null(sp)) return(NULL)

      if (identical(sp$type, "numeric")) {
        binned <- !is.null(sp$breaks)

        # Read-only chips previewing the groups the break points produce
        chips <- NULL
        if (binned && is.factor(groups_all[[a]])) {
          f <- groups_all[[a]]
          cnt <- table(f)
          chips <- htmltools::div(
            class = "pr-grp-chips",
            lapply(seq_along(levels(f)), function(g) {
              htmltools::span(
                class = "pr-grp-chip",
                htmltools::span(
                  class = "pr-grp-dot",
                  style = paste0(
                    "background:",
                    fcolors[((g - 1) %% length(fcolors)) + 1], ";")
                ),
                htmltools::span(levels(f)[g]),
                htmltools::span(class = "pr-grp-count",
                                sprintf("%d", cnt[[levels(f)[g]]]))
              )
            })
          )
        }

        htmltools::div(
          class = "pr-attr-block",
          htmltools::div(
            class = "pr-attr-head",
            htmltools::span(class = "pr-attr-name", tools::toTitleCase(pr_attr_label(a))),
            htmltools::span(
              class = "pr-attr-note",
              if (binned) "numeric, grouped at your break points"
              else "numeric; set break points to make groups"
            ),
            if (binned) {
              shiny::actionLink(ns(paste0("pr_reset_", ai)),
                                "Reset to original", class = "pr-reset")
            }
          ),
          shiny::selectInput(
            ns(paste0("pr_bin_mode_", ai)), NULL,
            choices = c("Use as a number (correlation)" = "raw",
                        "Median split" = "median",
                        "Custom break points" = "custom"),
            selected = if (binned) "custom" else "raw",
            width = "260px", selectize = FALSE
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'custom'",
                                ns(paste0("pr_bin_mode_", ai))),
            htmltools::div(class = "bq2-label",
                           "Break points (lowest, cuts, highest)"),
            shiny::textInput(
              ns(paste0("pr_bin_breaks_", ai)), NULL,
              value = if (binned) paste(sp$breaks, collapse = ", ") else "",
              placeholder = "e.g. 18, 40, 75", width = "260px"
            ),
            htmltools::div(class = "bq2-label", "Group labels (optional)"),
            shiny::textInput(
              ns(paste0("pr_bin_labels_", ai)), NULL,
              value = if (!is.null(sp$labels)) {
                paste(sp$labels, collapse = ", ")
              } else "",
              placeholder = "e.g. 18-40, 41-75", width = "260px"
            )
          ),
          chips
        )
      } else {
        f <- groups_all[[a]]
        if (!is.factor(f)) return(NULL)
        cnt <- table(f)
        cur <- levels(f)
        lv_raw <- pr_raw_levels(sp, attrs, a)
        cur_of <- vapply(lv_raw, function(l) {
          if (!is.null(sp$map)) (sp$map[[l]] %||% l) else l
        }, character(1))
        modified <- !identical(unname(cur_of), unname(lv_raw))

        rows <- lapply(seq_along(cur), function(g) {
          raws <- lv_raw[!is.na(cur_of) & cur_of == cur[g]]
          combined <- !(length(raws) == 1 && identical(raws[1], cur[g]))
          n_g <- if (cur[g] %in% names(cnt)) cnt[[cur[g]]] else 0L
          htmltools::div(
            class = "pr-grp-row",
            title = if (combined) {
              paste("Combines", paste(raws, collapse = " + "))
            },
            htmltools::span(
              class = "pr-grp-dot",
              style = paste0(
                "background:",
                if (n_g > 0) fcolors[((g - 1) %% length(fcolors)) + 1]
                else "#cbd5e1", ";")
            ),
            shiny::textInput(ns(paste0("pr_grp_", ai, "_", g)), NULL,
                             value = cur[g], width = "140px"),
            htmltools::span(class = "pr-grp-count", sprintf("%d", n_g)),
            if (length(cur) > 1) {
              htmltools::tags$a(
                href = "#", class = "pr-grp-x",
                title = sprintf("Set %s aside", cur[g]),
                onclick = sprintf(
                  "Shiny.setInputValue('%s', {ai: %d, g: %d}, {priority: 'event'}); return false;",
                  ns("pr_grp_del"), ai, g),
                "×"
              )
            }
          )
        })

        dropped <- lv_raw[is.na(cur_of)]
        aside_line <- if (length(dropped) > 0) {
          raw_chr <- trimws(as.character(attrs[[a]]))
          cnts <- vapply(dropped, function(d) {
            sum(raw_chr == d, na.rm = TRUE)
          }, integer(1))
          htmltools::div(
            class = "pr-grp-aside",
            paste0("Set aside: ",
                   paste(sprintf("%s (%d)", dropped, cnts), collapse = ", "),
                   ". These sorts still count in the overall ranking. ",
                   "A new range that covers them, or Reset to original, ",
                   "brings them back.")
          )
        }

        htmltools::div(
          class = "pr-attr-block",
          htmltools::div(
            class = "pr-attr-head",
            htmltools::span(class = "pr-attr-name", tools::toTitleCase(pr_attr_label(a))),
            htmltools::span(
              class = "pr-attr-note",
              if (identical(sp$type, "ordinal")) {
                "arrived grouped; type a wider range like 18-40 to combine bands, or give two groups the same name"
              } else {
                "rename a group, or give two the same name to combine them"
              }
            ),
            if (modified) {
              shiny::actionLink(ns(paste0("pr_reset_", ai)),
                                "Reset to original", class = "pr-reset")
            }
          ),
          htmltools::div(class = "pr-grp-rows", rows),
          aside_line
        )
      }
    })

    htmltools::div(
      class = "pr-grouping",
      htmltools::div(class = "pr-grouping-title", "Groupings"),
      htmltools::div(
        class = "pr-grouping-hint",
        paste0("Edits take effect on Apply and carry through every chart, ",
               "ranking, and test. The × excludes a group immediately.")
      ),
      editors,
      shiny::actionButton(ns("pr_apply_groups"), "Apply groupings",
                          class = "btn-ov2-blue bq2-run")
    )
  })

  # Reset one attribute's grouping to what the file arrived with
  lapply(1:24, function(ai) {
    shiny::observeEvent(input[[paste0("pr_reset_", ai)]], {
      qd <- rv$qdata
      shiny::req(qd, has_participant_attributes(qd))
      a_names <- setdiff(names(participant_attributes(qd)), "participant")
      shiny::req(ai <= length(a_names))
      a <- a_names[ai]
      rv$qdata <- reset_attribute_binning(qd, a)
      pr_precompute("Restoring the original groups")
      session$sendCustomMessage("showToast", list(
        message = sprintf("Groups for '%s' reset to the original.", a),
        type = "info"
      ))
    })
  })

  # The x on an editor row: set that group aside immediately. Its sorts
  # stay in the overall ranking and drop out of this demographic's panels.
  shiny::observeEvent(input$pr_grp_del, {
    qd <- rv$qdata
    shiny::req(qd, has_participant_attributes(qd))
    info <- input$pr_grp_del
    ai <- suppressWarnings(as.integer(info$ai))
    g <- suppressWarnings(as.integer(info$g))
    attrs <- participant_attributes(qd)
    a_names <- setdiff(names(attrs), "participant")
    shiny::req(!is.na(ai), ai >= 1, ai <= length(a_names))
    a <- a_names[ai]
    sp <- qd@metadata$attribute_spec[[a]]
    shiny::req(!is.null(sp), !identical(sp$type, "numeric"))
    f <- attribute_groups(qd)[[a]]
    shiny::req(is.factor(f))
    cur_levels <- levels(f)
    shiny::req(!is.na(g), g >= 1, g <= length(cur_levels),
               length(cur_levels) > 1)
    target <- cur_levels[g]
    lv_raw <- pr_raw_levels(sp, attrs, a)
    cur_of <- vapply(lv_raw, function(l) {
      if (!is.null(sp$map)) (sp$map[[l]] %||% l) else l
    }, character(1))
    new_vals <- ifelse(!is.na(cur_of) & cur_of == target,
                       NA_character_, cur_of)
    rv$qdata <- set_attribute_binning(
      qd, a, map = stats::setNames(new_vals, lv_raw)
    )
    pr_precompute("Updating groups and tests")
    session$sendCustomMessage("showToast", list(
      message = sprintf("%s set aside for %s. Reset to original restores it.",
                        target, a),
      type = "info"
    ))
  })

  shiny::observeEvent(input$pr_apply_groups, {
    qd <- rv$qdata
    shiny::req(qd, has_participant_attributes(qd))
    spec <- qd@metadata$attribute_spec
    attrs <- participant_attributes(qd)
    a_names <- setdiff(names(attrs), "participant")
    ok <- TRUE

    for (ai in seq_along(a_names)) {
      a <- a_names[ai]
      sp <- spec[[a]]
      if (is.null(sp)) next
      tryCatch({
        if (identical(sp$type, "numeric")) {
          mode <- input[[paste0("pr_bin_mode_", ai)]] %||% "raw"
          if (identical(mode, "raw")) {
            if (!is.null(sp$breaks)) qd <- reset_attribute_binning(qd, a)
          } else if (identical(mode, "median")) {
            v <- suppressWarnings(as.numeric(attrs[[a]]))
            md <- stats::median(v, na.rm = TRUE)
            qd <- set_attribute_binning(
              qd, a,
              breaks = c(min(v, na.rm = TRUE), md, max(v, na.rm = TRUE))
            )
          } else {
            txt <- input[[paste0("pr_bin_breaks_", ai)]] %||% ""
            br <- suppressWarnings(as.numeric(trimws(strsplit(txt, ",")[[1]])))
            br <- sort(unique(br[!is.na(br)]))
            if (length(br) >= 3) {
              lab_txt <- trimws(input[[paste0("pr_bin_labels_", ai)]] %||% "")
              labs <- if (nzchar(lab_txt)) {
                trimws(strsplit(lab_txt, ",")[[1]])
              } else {
                paste0(utils::head(br, -1), "-", utils::tail(br, -1))
              }
              if (length(labs) != length(br) - 1) {
                labs <- paste0(utils::head(br, -1), "-", utils::tail(br, -1))
              }
              qd <- set_attribute_binning(qd, a, breaks = br, labels = labs)
            }
          }
        } else {
          # Rows are keyed by the CURRENT groups; compose the edits back
          # onto the raw levels so a rename replaces the shown name instead
          # of hanging beside it
          f <- attribute_groups(qd)[[a]]
          shiny::req(is.factor(f))
          cur_levels <- levels(f)
          lv_raw <- pr_raw_levels(sp, attrs, a)
          cur_of <- vapply(lv_raw, function(l) {
            if (!is.null(sp$map)) (sp$map[[l]] %||% l) else l
          }, character(1))
          edits <- vapply(seq_along(cur_levels), function(g) {
            val <- input[[paste0("pr_grp_", ai, "_", g)]]
            if (is.null(val) || !nzchar(trimws(val))) cur_levels[g]
            else trimws(val)
          }, character(1))
          idx <- match(cur_of, cur_levels)
          new_vals <- ifelse(is.na(idx), cur_of, edits[idx])

          # Range absorption: typing a wider band (18-40 over an 18-30 box)
          # pulls in every band that fits inside it, so the combined count
          # is the total. Groups whose own box was edited keep that edit.
          # Set-aside bands rejoin when a new range covers their ages: the
          # typed range is an explicit claim over those people.
          changed <- edits != cur_levels
          if (any(changed)) {
            edit_rng <- pr_parse_range(edits)
            raw_rng <- pr_parse_range(lv_raw)
            for (g in which(changed)) {
              if (is.na(edit_rng[g, 1])) next
              for (r in seq_along(lv_raw)) {
                if (is.na(raw_rng[r, 1])) next
                if (!is.na(cur_of[r])) {
                  src <- match(cur_of[r], cur_levels)
                  if (!is.na(src) && changed[src]) next
                }
                if (raw_rng[r, 1] >= edit_rng[g, 1] &&
                    raw_rng[r, 2] <= edit_rng[g, 2]) {
                  new_vals[r] <- edits[g]
                }
              }
            }
          }

          if (identical(unname(new_vals), unname(lv_raw))) {
            # Typed back to the originals: drop the map instead of storing
            # an identity one
            if (!is.null(sp$map)) qd <- reset_attribute_binning(qd, a)
          } else {
            qd <- set_attribute_binning(
              qd, a, map = stats::setNames(new_vals, lv_raw)
            )
          }
        }
      }, error = function(e) {
        ok <<- FALSE
        session$sendCustomMessage("showToast", list(
          message = paste0("Grouping for '", a, "' failed: ",
                           conditionMessage(e)),
          type = "error", duration = 6000
        ))
      })
    }

    rv$qdata <- qd
    pr_precompute("Applying groupings")
    if (ok) {
      session$sendCustomMessage("showToast", list(
        message = "Groupings applied. Tables and charts updated.",
        type = "success"
      ))
    }
  })

  # Ranking: the plain overall ranking, demographics-free ----
  output$pr_rank_title <- shiny::renderUI({
    pr <- pr_res()
    pr_title("Mean placement ranking",
             sprintf("%d statements", nrow(pr$table)),
             sprintf("%d sorts", pr$meta$n))
  })

  output$pr_ranked <- DT::renderDT({
    pr <- pr_res()
    tab <- pr$table
    fmt_m <- function(x) gsub("-", "−", sprintf("%+.2f", x), fixed = TRUE)

    df <- data.frame(
      Rank = ifelse(tab$tied, paste0(tab$rank, "="), as.character(tab$rank)),
      Statement = tab$stmt,
      Text = tab$statement,
      `Average placement` = fmt_m(tab$mean),
      SD = sprintf("%.2f", tab$sd),
      check.names = FALSE, stringsAsFactors = FALSE
    )

    DT::datatable(
      df, escape = TRUE, rownames = FALSE,
      options = list(pageLength = 15, dom = "ftp", scrollX = TRUE),
      class = "compact stripe"
    )
  })

  # Participants: who is in the panel, under the current groupings ----
  output$pr_part_head <- shiny::renderUI({
    qd <- rv$qdata
    shiny::req(qd)
    if (!has_participant_attributes(qd)) {
      return(htmltools::div(
        class = "bq2-tab-note",
        "Add demographics to see who is in the panel."
      ))
    }
    attrs <- participant_attributes(qd)
    n_all <- nrow(attrs)
    n_match <- sum(stats::complete.cases(attrs[, -1, drop = FALSE]))
    pr_title("Panel composition",
             sprintf("%d sorts", n_all),
             sprintf("%d with demographics", n_match))
  })

  output$pr_part_plot_ui <- shiny::renderUI({
    qd <- rv$qdata
    shiny::req(qd, has_participant_attributes(qd))
    k <- length(attribute_groups(qd))
    shiny::plotOutput(ns("pr_part_plot"),
                      height = paste0(80 + 380 * ceiling(k / 2), "px"))
  })

  output$pr_part_plot <- shiny::renderPlot({
    qd <- rv$qdata
    shiny::req(qd, has_participant_attributes(qd))
    g <- create_plot_panel_composition(qd)
    shiny::req(!is.null(g))
    grid::grid.draw(g)
  }, res = 96)

  # Flow chart ----
  output$pr_flow_title <- shiny::renderUI({
    qd <- rv$qdata
    shiny::req(qd)
    groups <- pr_groups()
    shiny::req(groups)
    fg <- groups[vapply(groups, is.factor, logical(1))]
    sel <- intersect(input$pr_flow_attrs %||% names(fg), names(fg))
    shiny::req(length(sel) >= 1)
    n <- max(3, min(10, input$pr_flow_n %||% 5))
    n_groups <- sum(vapply(fg[sel], nlevels, integer(1)))
    pr_title("Statement flow",
             sprintf("top %d statements", n),
             sprintf("%d groups", n_groups))
  })

  output$pr_flow_controls <- shiny::renderUI({
    qd <- rv$qdata
    shiny::req(qd)
    groups <- pr_groups()
    factor_groups <- if (!is.null(groups)) {
      names(groups)[vapply(groups, is.factor, logical(1))]
    } else character(0)

    if (!length(factor_groups)) {
      return(htmltools::div(
        class = "bq2-tab-note",
        "Add demographics to draw the flow of top statements across groups."
      ))
    }

    htmltools::div(
      class = "pr-flow-controls",
      htmltools::div(
        class = "pr-flow-pick",
        htmltools::div(class = "bq2-label", "Split by"),
        shiny::checkboxGroupInput(
          ns("pr_flow_attrs"), NULL,
          choices = stats::setNames(
            factor_groups, vapply(factor_groups, pr_attr_label, character(1))
          ),
          selected = shiny::isolate(input$pr_flow_attrs) %||% factor_groups,
          inline = TRUE
        )
      ),
      htmltools::div(
        class = "pr-flow-topn",
        htmltools::div(class = "bq2-label", "Top statements"),
        shiny::numericInput(ns("pr_flow_n"), NULL,
                            value = shiny::isolate(input$pr_flow_n) %||% 5,
                            min = 3, max = 10, width = "72px")
      ),
      htmltools::div(style = "flex: 1;"),
      shiny::downloadButton(ns("pr_flow_png"), "Download figure",
                            class = "btn-ov2-blue bq2-run", icon = NULL)
    )
  })

  pr_flow_gg <- shiny::reactive({
    qd <- rv$qdata
    shiny::req(qd)
    groups <- pr_groups()
    shiny::req(groups)
    sel <- input$pr_flow_attrs %||%
      names(groups)[vapply(groups, is.factor, logical(1))]
    sel <- intersect(sel, names(groups))
    shiny::req(length(sel) >= 1)
    n <- max(3, min(10, input$pr_flow_n %||% 5))

    pr <- pr_res()
    top_tbl <- priorities_top_n(qd, groups = groups[sel], n = n)
    overall <- stats::setNames(pr$table$mean, pr$table$stmt)
    stmt_named <- stats::setNames(qd@statements,
                                  paste0("S", seq_along(qd@statements)))
    p <- plot_priorities_flow(
      top_tbl,
      statements = stmt_named,
      overall_means = overall,
      gates = pr$gates,
      palette = "ramp",
      show_legend = FALSE
    )
    attr(p, "priorities_top") <- top_tbl
    p
  })

  output$pr_flow_plot <- shiny::renderPlot({
    pr_flow_gg()
  }, res = 96)

  # Click a block to read its statement (the flow carries no legend)
  pr_flow_sel <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$pr_flow_click, {
    click <- input$pr_flow_click
    p <- tryCatch(pr_flow_gg(), error = function(e) NULL)
    shiny::req(p, click$x, click$y)
    levels_x <- attr(p, "priorities_levels")
    n_max <- attr(p, "priorities_nmax")
    top_tbl <- attr(p, "priorities_top")
    gi <- round(click$x)
    shiny::req(gi >= 1, gi <= length(levels_x))
    rank <- n_max - floor(click$y)
    shiny::req(rank >= 1, rank <= n_max)

    pairs <- unique(top_tbl[, c("attribute", "level")])
    lvl_key <- if (anyDuplicated(pairs$level) > 0) {
      paste(top_tbl$attribute, top_tbl$level, sep = ": ")
    } else {
      as.character(top_tbl$level)
    }
    hit <- which(lvl_key == levels_x[gi] & top_tbl$rank == rank)
    shiny::req(length(hit) == 1)
    pr_flow_sel(list(code = as.character(top_tbl$stmt[hit]),
                     level = levels_x[gi], rank = rank))
  })

  shiny::observeEvent(list(input$pr_flow_attrs, input$pr_flow_n),
                      pr_flow_sel(NULL), ignoreInit = TRUE)

  output$pr_flow_pane <- shiny::renderUI({
    qd <- rv$qdata
    shiny::req(qd)
    sel <- pr_flow_sel()
    if (is.null(sel)) {
      return(htmltools::div(class = "ov2-qs-pane empty",
                            "Click a block to read its statement."))
    }
    p <- tryCatch(pr_flow_gg(), error = function(e) NULL)
    shiny::req(p)
    top_tbl <- attr(p, "priorities_top")
    colors <- attr(p, "priorities_colors")
    pr <- pr_res()

    code <- sel$code
    s_num <- suppressWarnings(as.integer(sub("^S", "", code)))
    txt <- if (!is.na(s_num) && s_num <= length(qd@statements)) {
      qd@statements[s_num]
    } else code
    badge_bg <- unname(colors[code]) %||% "#236192"

    overall_row <- pr$table[pr$table$stmt == code, , drop = FALSE]
    fmt_m <- function(x) gsub("-", "−", sprintf("%+.2f", x), fixed = TRUE)

    rows <- top_tbl[top_tbl$stmt == code, , drop = FALSE]
    per_level <- lapply(seq_len(nrow(rows)), function(i) {
      htmltools::div(
        class = "bq2-pane-fact",
        htmltools::span(class = "bq2-comp-name", rows$level[i]),
        htmltools::span(
          class = "bq2-comp-facts",
          paste0("rank ", rows$rank[i], " · mean ", fmt_m(rows$mean[i]))
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
                        code)
      ),
      htmltools::div(
        class = "bq2-pane-body",
        htmltools::div(class = "ov2-qs-pane-text", txt),
        htmltools::div(
          class = "bq2-pane-facts",
          if (nrow(overall_row) == 1) {
            htmltools::div(
              class = "bq2-pane-fact",
              htmltools::span(class = "bq2-comp-name", "Overall"),
              htmltools::span(
                class = "bq2-comp-facts",
                paste0("rank ", overall_row$rank, " of ", nrow(pr$table),
                       " · mean ", fmt_m(overall_row$mean))
              )
            )
          },
          per_level
        )
      )
    )
  })

  output$pr_flow_png <- shiny::downloadHandler(
    filename = function() paste0("priorities_flow_", format(Sys.Date(), "%Y%m%d"), ".png"),
    content = function(file) {
      ggplot2::ggsave(file, plot = pr_flow_gg(), width = 16, height = 9,
                      dpi = 300, bg = "white")
    }
  )

  # Group ranking: each group's own overall ranking ----
  output$pr_diff_controls <- shiny::renderUI({
    qd <- rv$qdata
    shiny::req(qd)
    groups <- pr_groups()
    fg <- if (!is.null(groups)) {
      groups[vapply(groups, is.factor, logical(1))]
    } else list()
    if (!length(fg)) {
      return(htmltools::div(
        class = "bq2-tab-note",
        "Add demographics to rank statements by group."
      ))
    }

    choices <- list()
    for (a in names(fg)) {
      lv <- levels(fg[[a]])
      choices[[tools::toTitleCase(pr_attr_label(a))]] <-
        stats::setNames(paste(a, lv, sep = "||"), lv)
    }
    sel <- shiny::isolate(input$pr_diff_group)
    if (is.null(sel) || !(sel %in% unlist(choices))) sel <- unlist(choices)[[1]]

    htmltools::tagList(
      htmltools::div(
        class = "pr-ttl-row",
        shiny::uiOutput(ns("pr_diff_title")),
        htmltools::div(style = "flex: 1;"),
        shiny::selectInput(ns("pr_diff_group"), NULL, choices = choices,
                           selected = sel, width = "220px", selectize = FALSE)
      ),
      shiny::uiOutput(ns("pr_diff_note"))
    )
  })

  output$pr_diff_title <- shiny::renderUI({
    sel <- input$pr_diff_group
    shiny::req(sel)
    parts <- strsplit(sel, "||", fixed = TRUE)[[1]]
    groups <- pr_groups()
    shiny::req(groups)
    v <- groups[[parts[1]]]
    shiny::req(v)
    n_g <- sum(!is.na(v) & v == parts[2])
    pr_title("Group ranking", parts[2],
             sprintf("%d of %d sorts", n_g, nrow(rv$qdata@sorts)))
  })

  output$pr_diff_note <- shiny::renderUI({
    qd <- rv$qdata
    shiny::req(qd)
    htmltools::div(
      class = "pr-ttl-sub",
      sprintf("Overall rank is computed from all %d sorts.", nrow(qd@sorts))
    )
  })

  output$pr_diff <- DT::renderDT({
    qd <- rv$qdata
    shiny::req(qd)
    groups <- pr_groups()
    shiny::req(groups)
    sel <- input$pr_diff_group
    shiny::req(sel)
    parts <- strsplit(sel, "||", fixed = TRUE)[[1]]
    v <- groups[[parts[1]]]
    shiny::req(is.factor(v))

    pr <- pr_res()
    tab <- pr$table
    fmt_m <- function(x) gsub("-", "−", sprintf("%+.2f", x), fixed = TRUE)

    X <- qd@sorts
    idx <- which(!is.na(v) & v == parts[2])
    shiny::req(length(idx) > 0)
    m <- colMeans(X[idx, , drop = FALSE], na.rm = TRUE)
    s <- apply(X[idx, , drop = FALSE], 2, stats::sd, na.rm = TRUE)
    rk <- rank(-m, ties.method = "min")
    tied <- duplicated(m) | duplicated(m, fromLast = TRUE)
    code <- paste0("S", seq_along(m))
    ord <- order(rk, seq_along(m))

    df <- data.frame(
      Rank = ifelse(tied, paste0(rk, "="), as.character(rk))[ord],
      Statement = code[ord],
      Text = tab$statement[match(code[ord], tab$stmt)],
      `Average placement` = fmt_m(m[ord]),
      SD = sprintf("%.2f", s[ord]),
      `Overall rank` = tab$rank[match(code[ord], tab$stmt)],
      check.names = FALSE, stringsAsFactors = FALSE
    )

    DT::datatable(
      df, escape = TRUE, rownames = FALSE,
      options = list(pageLength = 15, dom = "ftp", scrollX = TRUE),
      class = "compact stripe"
    )
  })

  # Tests: the tests behind the flags, one family at a time ----
  pr_test_names <- c(
    wilcoxon_pratt = "Wilcoxon signed-rank against the deck's neutral column",
    mann_whitney = "Mann-Whitney between the two groups",
    kruskal_wallis = "Kruskal-Wallis across the groups",
    linear_by_linear = "Linear-by-linear trend across the ordered groups",
    spearman = "Spearman correlation with the raw values"
  )

  output$pr_stats_controls <- shiny::renderUI({
    pr <- pr_res()
    fams <- unique(pr$tests$attribute)
    choices <- c("Versus neutral" = "neutral")
    for (a in setdiff(fams, "neutral")) {
      choices[[paste("By", pr_attr_label(a))]] <- a
    }
    sel <- shiny::isolate(input$pr_stats_family)
    if (is.null(sel) || !(sel %in% unname(choices))) sel <- "neutral"

    htmltools::tagList(
      htmltools::div(
        class = "pr-ttl-row",
        shiny::uiOutput(ns("pr_stats_title")),
        htmltools::div(style = "flex: 1;"),
        shiny::selectInput(ns("pr_stats_family"), NULL, choices = choices,
                           selected = sel, width = "220px", selectize = FALSE)
      ),
      shiny::uiOutput(ns("pr_stats_note"))
    )
  })

  output$pr_stats_title <- shiny::renderUI({
    pr <- pr_res()
    fam <- input$pr_stats_family %||% "neutral"
    J <- nrow(pr$table)
    if (identical(fam, "neutral")) {
      return(pr_title("Departure from neutral",
                      sprintf("%d statements", J),
                      sprintf("%d sorts", pr$meta$n)))
    }
    groups <- pr_groups()
    shiny::req(groups, fam %in% names(groups))
    v <- groups[[fam]]
    n_with <- sum(!is.na(v))
    if (is.factor(v)) {
      pr_title(sprintf("Placement by %s", pr_attr_label(fam)),
               sprintf("%d statements", J),
               sprintf("%d groups", nlevels(v)),
               sprintf("%d sorts", n_with))
    } else {
      pr_title(sprintf("Placement by %s", pr_attr_label(fam)),
               sprintf("%d statements", J),
               sprintf("%d sorts", n_with))
    }
  })

  output$pr_stats_note <- shiny::renderUI({
    pr <- pr_res()
    fam <- input$pr_stats_family %||% "neutral"
    tt <- pr$tests[pr$tests$attribute == fam, , drop = FALSE]
    shiny::req(nrow(tt) > 0)
    used <- pr_test_names[unique(tt$test)]
    used <- used[!is.na(used)]
    used <- if (length(used) > 0) {
      paste(used, collapse = "; ")
    } else {
      "Per-statement tests"
    }

    line <- if (identical(fam, "neutral")) {
      sprintf("%s · BH within family · flags at q = 0.05", used)
    } else {
      gate <- pr$gates[pr$gates$attribute == fam, , drop = FALSE]
      gate_seg <- if (nrow(gate) == 1 && !is.na(gate$p)) {
        if (isTRUE(gate$pass)) {
          sprintf("gate passed · PERMANOVA R² = %.2f, p = %.3f",
                  gate$r2, gate$p)
        } else {
          sprintf("gate not passed · PERMANOVA p = %.3f · screening only",
                  gate$p)
        }
      } else {
        "gate not computable"
      }
      sprintf("%s · %s · BH within family · flags at q = 0.05",
              used, gate_seg)
    }
    htmltools::div(class = "pr-ttl-sub", line)
  })

  output$pr_stats <- DT::renderDT({
    pr <- pr_res()
    fam <- input$pr_stats_family %||% "neutral"
    tt <- pr$tests[pr$tests$attribute == fam, , drop = FALSE]
    shiny::req(nrow(tt) > 0)

    stmt_text <- pr$table$statement[match(tt$stmt, pr$table$stmt)]
    ord <- order(!tt$flagged, tt$q_adj, tt$stmt)
    tt <- tt[ord, , drop = FALSE]
    stmt_text <- stmt_text[ord]

    fmt_p <- function(p) {
      ifelse(is.na(p), "",
             ifelse(p < 0.001, "&lt;0.001", sprintf("%.3f", p)))
    }
    fmt_m <- function(x) gsub("-", "−", sprintf("%+.2f", x), fixed = TRUE)

    finding <- if (identical(fam, "neutral")) {
      ifelse(
        tt$flagged,
        sprintf('<span class="pr-cell-flag">%s</span>',
                ifelse(tt$direction == "above neutral",
                       "Prioritized", "Deprioritized")),
        '<span class="pr-cell-muted">No evidence</span>'
      )
    } else {
      ifelse(
        tt$flagged,
        sprintf('<span class="pr-cell-flag">%s</span>',
                ifelse(is.na(tt$direction), "Differs across groups",
                       tt$direction)),
        ifelse(!is.na(tt$note) & nzchar(tt$note),
               sprintf('<span class="pr-cell-muted">%s</span>', tt$note),
               '<span class="pr-cell-muted">No evidence</span>')
      )
    }

    df <- data.frame(
      Statement = tt$stmt,
      Text = stmt_text,
      Finding = finding,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    if (identical(fam, "neutral")) {
      df$Shift <- ifelse(
        is.na(tt$hl_shift), "",
        paste0(fmt_m(tt$hl_shift), " [", fmt_m(tt$hl_lower), ", ",
               fmt_m(tt$hl_upper), "]")
      )
    }
    df$Effect <- ifelse(is.na(tt$effect_label), "", tt$effect_label)
    df[["p value"]] <- fmt_p(tt$p_raw)
    df[["q value"]] <- fmt_p(tt$q_adj)
    df$n <- tt$n_used

    DT::datatable(
      df, escape = FALSE, rownames = FALSE,
      options = list(pageLength = 15, dom = "ftp", scrollX = TRUE),
      class = "compact stripe"
    )
  })

  # Workbook ----
  output$pr_workbook_area <- shiny::renderUI({
    shiny::req(rv$qdata)
    shiny::downloadButton(ns("pr_workbook"), "Workbook",
                          class = "btn-ov2-blue bq2-run", icon = NULL,
                          title = "Ranked tables, group comparisons, and the flow figure in one Excel file")
  })

  output$pr_workbook <- shiny::downloadHandler(
    filename = function() {
      paste0("canhrqsort_priorities_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      shiny::req(rv$qdata)
      shiny::withProgress(message = "Building workbook", value = 0.4, {
        write_priorities_workbook(
          rv$qdata, file,
          priorities = pr_res(),
          groups = pr_groups(),
          top_n = max(3, min(10, input$pr_flow_n %||% 5))
        )
        shiny::incProgress(0.6)
      })
    }
  )

  # These outputs live inside collapsed panels and hidden tabs; without this
  # they stay suspended and never render after the panel opens.
  for (o in c("pr_match_report", "pr_grouping_editor", "pr_flow_controls",
              "pr_flow_plot", "pr_flow_pane", "pr_flow_title",
              "pr_diff_controls", "pr_diff", "pr_diff_note", "pr_diff_title",
              "pr_stats_controls", "pr_stats", "pr_stats_note",
              "pr_stats_title", "pr_rank_title",
              "pr_part_head", "pr_part_plot_ui", "pr_part_plot",
              "pr_flow_png")) {
    shiny::outputOptions(output, o, suspendWhenHidden = FALSE)
  }

  invisible(NULL)
}
