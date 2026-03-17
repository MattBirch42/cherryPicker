#' Cherry Picker Basic Server
#'
#' Internal server function for the basic Cherry Picker app.
#' Accepts a pre-loaded data frame from the R session. Raw data is stored
#' in DuckDB and never mutated. Selections and removals are tracked as
#' persistent reactive UID vectors.
#'
#' @param preloaded_data A data frame passed in from the R session.
#'
#' @return A Shiny server function.
#' @keywords internal
cherry_picker_basic_server <- function(preloaded_data) {
  function(input, output, session) {

    # ----------------------------------------------------------
    # Initialise global export list if not already present
    # ----------------------------------------------------------
    if (!exists("cherry_picker_export_list", envir = .GlobalEnv)) {
      assign("cherry_picker_export_list", list(), envir = .GlobalEnv)
    }

    # ----------------------------------------------------------
    # Load raw data into DuckDB once — never mutated
    # ----------------------------------------------------------
    db <- convert_to_tbl(preloaded_data)
    con <- db$con.app

    # In-memory data frame with .row_uid (used for plotting)
    raw_df <- as.data.frame(preloaded_data)
    raw_df$.row_uid <- seq_len(nrow(raw_df))

    # ----------------------------------------------------------
    # Persistent reactive state
    # Three independent vectors — no duplicates enforced on write
    # selected_ids: currently highlighted on the plot (includes unsaved)
    # saved_ids:    explicitly committed by user via "Save Selections"
    # removed_ids:  marked for removal on the Export tab
    # ----------------------------------------------------------
    selected_ids <- shiny::reactiveVal(integer(0))
    saved_ids    <- shiny::reactiveVal(integer(0))
    removed_ids  <- shiny::reactiveVal(integer(0))

    # ----------------------------------------------------------
    # Downstream reactive: full data with all three flags attached
    # ----------------------------------------------------------
    data_with_flags <- shiny::reactive({
      df <- raw_df
      df$selected <- df$.row_uid %in% selected_ids()
      df$saved    <- df$.row_uid %in% saved_ids()
      df$removed  <- df$.row_uid %in% removed_ids()
      df
    })

    # ----------------------------------------------------------
    # Variable selectors — preserve last user choice on update
    # ----------------------------------------------------------
    last_xvar <- shiny::reactiveVal(NULL)
    last_yvar <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$xvar, { last_xvar(input$xvar) })
    shiny::observeEvent(input$yvar, { last_yvar(input$yvar) })

    shiny::observe({
      choices <- setdiff(names(raw_df), ".row_uid")
      shiny::updateSelectInput(
        session, "xvar",
        choices = choices,
        selected = if (!is.null(last_xvar()) && last_xvar() %in% choices) {
          last_xvar()
        } else if (length(choices) > 0) choices[1] else NULL
      )
      shiny::updateSelectInput(
        session, "yvar",
        choices = choices,
        selected = if (!is.null(last_yvar()) && last_yvar() %in% choices) {
          last_yvar()
        } else if (length(choices) > 1) choices[2] else choices[1]
      )
    })

    # ----------------------------------------------------------
    # Color variable
    # ----------------------------------------------------------
    color_var <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$add_color, {
      choices <- setdiff(names(raw_df), c(".row_uid", "selected", "saved", "removed"))
      meta_info <- lapply(choices, function(v) {
        vals <- raw_df[[v]]
        if (is.numeric(vals)) {
          rng <- range(vals, na.rm = TRUE)
          sprintf("%s (numeric, range: %.2f \u2013 %.2f)", v, rng[1], rng[2])
        } else if (is.factor(vals) || is.character(vals)) {
          sprintf("%s (categorical, %d levels)", v, length(unique(vals)))
        } else {
          sprintf("%s (other)", v)
        }
      })
      shiny::showModal(shiny::modalDialog(
        title = "Choose a Color Variable",
        shiny::selectInput(
          "colorvar_choice", "Available variables",
          choices = stats::setNames(choices, meta_info),
          width = "100%"
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("confirm_color", "Apply Color", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })

    shiny::observeEvent(input$confirm_color, {
      shiny::removeModal()
      color_var(input$colorvar_choice)
    })

    # ----------------------------------------------------------
    # Scatter plot
    # ----------------------------------------------------------
    output$scatter <- plotly::renderPlotly({
      shiny::req(input$xvar, input$yvar)
      make_marginal_scatter(
        df          = raw_df,
        xvar        = input$xvar,
        yvar        = input$yvar,
        nbins_x       = input$x_bins,
        nbins_y       = input$y_bins,
        highlight_ids = selected_ids(),
        remove_ids    = removed_ids(),
        colorvar      = color_var()
      )
    })

    # ----------------------------------------------------------
    # Plotly selection events -> selected_ids (additive, no dupes)
    # ----------------------------------------------------------
    shiny::observeEvent(
      plotly::event_data("plotly_click", source = "scatterplot"), {
        ed <- plotly::event_data("plotly_click", source = "scatterplot")
        if (!is.null(ed$customdata)) {
          ids <- as.integer(unlist(ed$customdata))
          selected_ids(unique(union(selected_ids(), ids)))
        }
      }
    )

    shiny::observeEvent(
      plotly::event_data("plotly_selected", source = "scatterplot"), {
        ed <- plotly::event_data("plotly_selected", source = "scatterplot")
        if (!is.null(ed$customdata)) {
          ids <- as.integer(unlist(ed$customdata))
          selected_ids(unique(union(selected_ids(), ids)))
        }
      }
    )

    # ----------------------------------------------------------
    # Save Selections: commit selected_ids into saved_ids
    # ----------------------------------------------------------
    shiny::observeEvent(input$save_selected, {
      saved_ids(unique(union(saved_ids(), selected_ids())))
      shiny::showNotification(
        paste0(length(saved_ids()), " rows saved."),
        type = "message", duration = 3
      )
    })

    # ----------------------------------------------------------
    # Clear Unsaved: revert selected_ids back to saved_ids only
    # ----------------------------------------------------------
    shiny::observeEvent(input$clear_unsaved, {
      selected_ids(saved_ids())
    })

    # ----------------------------------------------------------
    # Clear All: wipe both selected_ids and saved_ids
    # ----------------------------------------------------------
    shiny::observeEvent(input$clear_all_selected, {
      selected_ids(integer(0))
      saved_ids(integer(0))
    })

    # ----------------------------------------------------------
    # Selection counter (Visualization tab)
    # ----------------------------------------------------------
    output$selection_counter <- shiny::renderUI({
      total    <- nrow(raw_df)
      n_saved  <- length(saved_ids())
      n_sel    <- length(selected_ids())
      n_unsaved <- length(setdiff(selected_ids(), saved_ids()))
      n_rem    <- length(removed_ids())
      pct      <- if (total > 0) (n_saved / total) * 100 else 0
      color    <- if (pct < 1) "green" else if (pct < 5) "orange" else "red"

      shiny::tagList(
        shiny::tags$p(
          paste0("Saved: ", n_saved, " / ", total,
                 " (", sprintf("%.2f", pct), "%)"),
          style = paste0("font-weight: bold; color: ", color, "; margin: 0;")
        ),
        shiny::tags$p(
          paste0("Unsaved (pending): ", n_unsaved),
          style = "font-weight: bold; color: steelblue; margin: 0;"
        ),
        shiny::tags$p(
          paste0("Removed: ", n_rem, " / ", total),
          style = "font-weight: bold; color: #555; margin: 0;"
        )
      )
    })

    # ----------------------------------------------------------
    # Export tab: counter
    # ----------------------------------------------------------
    output$export_counter <- shiny::renderUI({
      total <- nrow(raw_df)
      shiny::tagList(
        shiny::tags$p(paste0("Total rows: ",   total)),
        shiny::tags$p(paste0("Saved:       ",  length(saved_ids()))),
        shiny::tags$p(paste0("Selected:    ",  length(selected_ids()))),
        shiny::tags$p(paste0("Removed:     ",  length(removed_ids())))
      )
    })

    # ----------------------------------------------------------
    # Mark selected as removed (persistent)
    # ----------------------------------------------------------
    shiny::observeEvent(input$mark_removed, {
      removed_ids(unique(union(removed_ids(), selected_ids())))
    })

    shiny::observeEvent(input$clear_removed, {
      removed_ids(integer(0))
    })

    # ----------------------------------------------------------
    # Helper: show comment modal, then run callback with comment
    # ----------------------------------------------------------
    export_callback <- shiny::reactiveVal(NULL)

    show_comment_modal <- function(input_id, confirm_id, callback) {
      export_callback(callback)
      shiny::showModal(shiny::modalDialog(
        title = "Add a comment (optional)",
        shiny::textAreaInput(
          input_id, "Comment", "",
          width = "100%", rows = 3,
          placeholder = "Optional description for this export"
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(confirm_id, "Export", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    }

    # ----------------------------------------------------------
    # Export selected to R — keep
    # ----------------------------------------------------------
    shiny::observeEvent(input$export_to_r_keep, {
      sel <- selected_ids()
      df_out <- raw_df[raw_df$.row_uid %in% sel, , drop = FALSE]
      show_comment_modal(
        "comment_keep", "confirm_export_keep",
        function(comment) {
          .append_to_export_list(df_out, comment, "selected_keep")
          shiny::showNotification("Exported to cherry_picker_export_list", type = "message")
        }
      )
    })

    shiny::observeEvent(input$confirm_export_keep, {
      shiny::removeModal()
      fn <- export_callback()
      if (!is.null(fn)) fn(input$comment_keep)
    }, ignoreInit = TRUE)

    # ----------------------------------------------------------
    # Export selected to R — mark as removed
    # ----------------------------------------------------------
    shiny::observeEvent(input$export_to_r_remove, {
      sel <- selected_ids()
      df_out <- raw_df[raw_df$.row_uid %in% sel, , drop = FALSE]
      show_comment_modal(
        "comment_remove", "confirm_export_remove",
        function(comment) {
          .append_to_export_list(df_out, comment, "selected_remove")
          removed_ids(unique(union(removed_ids(), sel)))
          shiny::showNotification(
            "Exported and marked as removed", type = "message"
          )
        }
      )
    })

    shiny::observeEvent(input$confirm_export_remove, {
      shiny::removeModal()
      fn <- export_callback()
      if (!is.null(fn)) fn(input$comment_remove)
    }, ignoreInit = TRUE)

    # ----------------------------------------------------------
    # Export full data with flags to R
    # ----------------------------------------------------------
    shiny::observeEvent(input$export_full_r, {
      df_out <- data_with_flags()
      show_comment_modal(
        "comment_full_r", "confirm_export_full_r",
        function(comment) {
          .append_to_export_list(df_out, comment, "full_with_flags")
          shiny::showNotification("Full data exported to cherry_picker_export_list", type = "message")
        }
      )
    })

    shiny::observeEvent(input$confirm_export_full_r, {
      shiny::removeModal()
      fn <- export_callback()
      if (!is.null(fn)) fn(input$comment_full_r)
    }, ignoreInit = TRUE)

    # ----------------------------------------------------------
    # CSV: selected rows
    # ----------------------------------------------------------
    output$export_csv_selected <- shiny::downloadHandler(
      filename = function() "cherry_picker_selected.csv",
      content = function(file) {
        sel <- selected_ids()
        df_out <- raw_df[raw_df$.row_uid %in% sel, , drop = FALSE]
        utils::write.csv(df_out, file, row.names = FALSE)
      }
    )

    # ----------------------------------------------------------
    # CSV: remaining rows (excluding removed)
    # ----------------------------------------------------------
    output$export_csv_remaining <- shiny::downloadHandler(
      filename = function() "cherry_picker_remaining.csv",
      content = function(file) {
        rem <- removed_ids()
        df_out <- raw_df[!(raw_df$.row_uid %in% rem), , drop = FALSE]
        utils::write.csv(df_out, file, row.names = FALSE)
      }
    )

    # ----------------------------------------------------------
    # CSV: full data with flags
    # ----------------------------------------------------------
    output$export_csv_full <- shiny::downloadHandler(
      filename = function() "cherry_picker_full_flags.csv",
      content = function(file) {
        utils::write.csv(data_with_flags(), file, row.names = FALSE)
      }
    )

    # ----------------------------------------------------------
    # Footer
    # ----------------------------------------------------------
    output$app_footer <- shiny::renderUI({
      shiny::tags$div(
        style = "margin-top: 20px;",
        shiny::tags$p(
          "Cherry Picker \u00a9 2025 Matt Birch",
          style = "font-size: 0.7em; color: gray; margin: 0;"
        ),
        shiny::tags$p(
          "github.com/MattBirch42/cherryPicker",
          style = "font-size: 0.7em; color: gray; margin: 0;"
        )
      )
    })
  }
}

# ----------------------------------------------------------
# Internal helper: append an entry to the global export list
# ----------------------------------------------------------
.append_to_export_list <- function(data, comment, type) {
  lst <- get("cherry_picker_export_list", envir = .GlobalEnv)
  lst[[length(lst) + 1]] <- list(
    data    = data,
    comment = substr(comment, 1, 256),
    type    = type,
    time    = Sys.time()
  )
  assign("cherry_picker_export_list", lst, envir = .GlobalEnv)
}
