#' Cherry Picker Server
#'
#' Internal function that defines the server logic for the Cherry Picker app.
#'
#' @param preloaded_data Optional data frame. If provided, the app starts
#'   immediately with this dataset. If `NULL`, the user must upload a file.
#'
#' @return A Shiny server function.
#' @keywords internal
#' 
cherry_picker_server <- function(preloaded_data = NULL) {
  function(input, output, session) {
    
    if (!exists("cherry_picker_export_list", envir = .GlobalEnv)) {
      assign("cherry_picker_export_list", list(), envir = .GlobalEnv)
    }
    
    # whether we are in preloaded mode
    output$preloadedMode <- shiny::reactive({ !is.null(preloaded_data) })
    shiny::outputOptions(output, "preloadedMode", suspendWhenHidden = FALSE)
    
    # stores
    uploaded_data  <- shiny::reactiveVal(NULL)
    filtered_data  <- shiny::reactiveVal(NULL)
    filter_mode    <- shiny::reactiveVal(FALSE)
    
    # ====== File upload ======
    shiny::observeEvent(input$file, {
      if (is.null(preloaded_data)) {
        ext <- tools::file_ext(input$file$name)
        
        if (tolower(ext) == "csv") {
          df <- utils::read.csv(input$file$datapath, header = input$header, stringsAsFactors = FALSE)
        } else if (tolower(ext) == "parquet") {
          df <- arrow::read_parquet(input$file$datapath) %>% as.data.frame()
        } else {
          shiny::showNotification("Unsupported file type. Please upload a CSV or Parquet file.", type = "error")
          return(NULL)
        }
        df$.row_uid <- seq_len(nrow(df))
        df <- detect_and_convert_dates(df, session)
        df <- detect_and_convert_timestamps(df, session)
        uploaded_data(df)
        
        if (nrow(df) > 20000) {
          shiny::showModal(
            shiny::modalDialog(
              title = "Large Data Warning",
              shiny::p(
                paste0("Your dataset has ", nrow(df), " rows and ", ncol(df),
                       " columns. Larger datasets may slow down the app. ",
                       "Do you want to filter the data first?")
              ),
              footer = shiny::tagList(
                shiny::actionButton("proceed_no_filter", "Proceed without filtering"),
                shiny::actionButton("do_filter", "Filter data")
              ),
              easyClose = FALSE
            )
          )
        } else {
          filter_mode(FALSE)
          filtered_data(df)
        }
      }
    })
    
    shiny::observeEvent(input$proceed_no_filter, {
      shiny::removeModal()
      filter_mode(FALSE)
      filtered_data(uploaded_data())
    })
    
    shiny::observeEvent(input$do_filter, {
      shiny::removeModal()
      filter_mode(TRUE)
      df <- uploaded_data()
      shiny::showModal(
        shiny::modalDialog(
          title = "Filter Data",
          shiny::uiOutput("filter_ui"),
          footer = shiny::tagList(
            shiny::div(
              style = "flex-grow: 1; text-align: left;",
              shiny::uiOutput("filter_counter", inline = TRUE)
            ),
            shiny::div(
              style = "flex-shrink: 0;",
              shiny::actionButton("apply_filters", "Apply Filters"),
              shiny::actionButton("cancel_filters", "Cancel Filters")
            )
          ),
          size = "l",
          easyClose = FALSE
        )
      )
      output$filter_ui <- shiny::renderUI({
        build_filter_ui(df)
      })
    })
    
    shiny::observeEvent(input$apply_filters, {
      shiny::removeModal()
      df <- apply_filters(uploaded_data(), input)
      filter_mode(TRUE)
      filtered_data(df)
    })
    
    shiny::observeEvent(input$cancel_filters, {
      shiny::removeModal()
      filter_mode(FALSE)
      filtered_data(uploaded_data())
    })
    
    shiny::observeEvent(input$clear_filters, {
      filter_mode(FALSE)
      filtered_data(uploaded_data())
    })
    
    shiny::observe({
      if (!is.null(preloaded_data)) {
        df <- as.data.frame(preloaded_data)
        df$.row_uid <- seq_len(nrow(df))
        df <- detect_and_convert_dates(df, session)
        df <- detect_and_convert_timestamps(df, session)
        uploaded_data(df)
        filter_mode(FALSE)
        filtered_data(df)
      }
    })
    
   
    raw_data <- shiny::reactive({
      shiny::req(filtered_data())
      filtered_data()
    })
    
    # update variable choices
    # Track most recent user selections
    last_xvar <- shiny::reactiveVal(NULL)
    last_yvar <- shiny::reactiveVal(NULL)
    
    # Update when user changes dropdowns
    shiny::observeEvent(input$xvar, {
      last_xvar(input$xvar)
    })
    shiny::observeEvent(input$yvar, {
      last_yvar(input$yvar)
    })
    
    # Update choices but preserve selections if still valid
    shiny::observe({
      df <- raw_data()
      choices <- setdiff(names(df), ".row_uid")
      
      shiny::updateSelectInput(
        session,
        "xvar",
        choices = choices,
        selected = if (!is.null(last_xvar()) && last_xvar() %in% choices) {
          last_xvar()
        } else if (length(choices) > 0) {
          choices[1]
        } else NULL
      )
      
      shiny::updateSelectInput(
        session,
        "yvar",
        choices = choices,
        selected = if (!is.null(last_yvar()) && last_yvar() %in% choices) {
          last_yvar()
        } else if (length(choices) > 1) {
          choices[2]
        } else if (length(choices) == 1) {
          choices[1]
        } else NULL
      )
    })
    
    
    # ====== Selections ======
    highlight_ids <- shiny::reactiveVal(c())
    
    output$scatter <- plotly::renderPlotly({
      shiny::req(input$xvar, input$yvar)
      make_marginal_scatter(raw_data(), input$xvar, input$yvar,
                            nbins_x = input$x_bins,
                            nbins_y = input$y_bins, highlight_ids())
    })
    
    shiny::observeEvent(plotly::event_data("plotly_click", source = "scatterplot"), {
      ed <- plotly::event_data("plotly_click", source = "scatterplot")
      if (!is.null(ed) && !is.null(ed$customdata)) {
        ids <- unlist(ed$customdata)   # works for scatter clicks (single) or bar clicks (many)
        highlight_ids(union(highlight_ids(), ids))
      }
    })
    
    
    shiny::observeEvent(plotly::event_data("plotly_selected", source = "scatterplot"), {
      ed <- plotly::event_data("plotly_selected", source = "scatterplot")
      if (!is.null(ed) && !is.null(ed$customdata)) {
        highlight_ids(union(highlight_ids(), ed$customdata))
      }
    })
    
    shiny::observeEvent(input$clear, { highlight_ids(c()) })
    
    # ====== Visualize without selected points ======
    shiny::observeEvent(input$viz_without, {
      df <- raw_data()
      removed <- highlight_ids()
      filtered_df <- df[!(df$.row_uid %in% removed), , drop = FALSE]
      
      shiny::showModal(
        shiny::modalDialog(
          title = "Scatter Plot Without Selected Points",
          shiny::selectInput("modal_xvar", "X-axis variable",
                             choices = setdiff(names(filtered_df), ".row_uid")),
          shiny::selectInput("modal_yvar", "Y-axis variable",
                             choices = setdiff(names(filtered_df), ".row_uid")),
          plotly::plotlyOutput("scatter_without"),
          easyClose = TRUE,
          size = "l"
        )
      )
      
      # --- preserve last_xvar / last_yvar if valid ---
      choices <- setdiff(names(filtered_df), ".row_uid")
      
      shiny::updateSelectInput(
        session,
        "modal_xvar",
        choices = choices,
        selected = if (!is.null(last_xvar()) && last_xvar() %in% choices) {
          last_xvar()
        } else if (length(choices) > 0) {
          choices[1]
        } else NULL
      )
      
      shiny::updateSelectInput(
        session,
        "modal_yvar",
        choices = choices,
        selected = if (!is.null(last_yvar()) && last_yvar() %in% choices) {
          last_yvar()
        } else if (length(choices) > 1) {
          choices[2]
        } else if (length(choices) == 1) {
          choices[1]
        } else NULL
      )
    })
    
    output$scatter_without <- plotly::renderPlotly({
      shiny::req(input$modal_xvar, input$modal_yvar)
      df <- raw_data()
      removed <- highlight_ids()
      filtered_df <- df[!(df$.row_uid %in% removed), , drop = FALSE]
      make_marginal_scatter(filtered_df, input$modal_xvar, input$modal_yvar,
                            nbins_x = input$x_bins,
                            nbins_y = input$y_bins)
    })
    
    # ====== Selection counter ======
    output$selection_counter <- shiny::renderUI({
      df <- raw_data()
      total <- nrow(df)
      selected <- length(highlight_ids())
      pct <- if (total > 0) (selected / total) * 100 else 0
      
      color <- if (pct < 1) {
        "green"
      } else if (pct < 5) {
        "orange"
      } else {
        "red"
      }
      
      shiny::tags$p(
        paste0("Selected: ", selected, " / ", total, " (", sprintf("%.2f", pct), "%)"),
        style = paste0("font-weight: bold; color: ", color, ";")
      )
    })
    
    # ====== Filter row counter (popup only) ======
    output$filter_counter <- shiny::renderUI({
      if (!is.null(uploaded_data())) {
        total <- nrow(uploaded_data())
        preview <- apply_filters(uploaded_data(), input)
        current <- nrow(preview)
        shiny::tags$p(
          paste0("Filtered rows: ", current, " / ", total),
          style = "margin: 0; font-weight: bold;"
        )
      }
    })
    
    # ====== Download selected rows ======
    output$export_to_csv_keep <- shiny::downloadHandler(
      filename = function() "cherry_picker_output.csv",
      content = function(file) {
        df <- raw_data()
        selected <- highlight_ids()
        if (length(selected) > 0) {
          selected_df <- df[df$.row_uid %in% selected, , drop = FALSE]
        } else {
          selected_df <- df[0, , drop = FALSE]
        }
        utils::write.csv(selected_df, file, row.names = FALSE)
      }
    )
    
    # ====== Export to CSV and Remove ======
    output$export_to_csv_remove <- shiny::downloadHandler(
      filename = function() "cherry_picker_output.csv",
      content = function(file) {
        df <- raw_data()
        selected <- highlight_ids()
        if (length(selected) > 0) {
          selected_df <- df[df$.row_uid %in% selected, , drop = FALSE]
        } else {
          selected_df <- df[0, , drop = FALSE]
        }
        utils::write.csv(selected_df, file, row.names = FALSE)
        
        df_remaining <- df[!(df$.row_uid %in% selected), , drop = FALSE]
        filtered_data(df_remaining)
        highlight_ids(c())  # clear highlights
      }
    )
    
    # ====== Export to R and keep ======
    shiny::observeEvent(input$export_to_r, {
      df <- raw_data()
      selected <- highlight_ids()
      if (length(selected) > 0) {
        selected_df <- df[df$.row_uid %in% selected, , drop = FALSE]
      } else {
        selected_df <- df[0, , drop = FALSE]
      }
      
      # Show modal to collect comment
      shiny::showModal(
        shiny::modalDialog(
          title = "Add comment or description",
          shiny::textAreaInput(
            "export_comment",
            "Comment",
            "",
            width = "100%",
            rows = 3,
            placeholder = "Enter an optional comment for this export"
          ),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton("confirm_export_to_r", "Export")
          ),
          easyClose = TRUE
        )
      )
      # Handle confirm button
      shiny::observeEvent(input$confirm_export_to_r, {
        shiny::removeModal()
        comment <- substr(input$export_comment, 1, 256) # enforce max length
        export_entry <- list(
          data = selected_df,
          comment = comment
        )
        
        # Append to global list
        cherry_picker_export_list <- get("cherry_picker_export_list", envir = .GlobalEnv)
        cherry_picker_export_list[[length(cherry_picker_export_list) + 1]] <- export_entry
        assign("cherry_picker_export_list", cherry_picker_export_list, envir = .GlobalEnv)
        
        shiny::showNotification("Export added to R session", type = "message")
      }, ignoreInit = TRUE, once = TRUE)
    })
    
    # ====== Export to R and Remove ======
    shiny::observeEvent(input$export_to_r_remove, {
      df <- raw_data()
      selected <- highlight_ids()
      if (length(selected) > 0) {
        selected_df <- df[df$.row_uid %in% selected, , drop = FALSE]
      } else {
        selected_df <- df[0, , drop = FALSE]
      }
      
      # Show modal to collect comment
      shiny::showModal(
        shiny::modalDialog(
          title = "Add comment or description",
          shiny::textAreaInput(
            "export_comment_remove",
            "Comment",
            "",
            width = "100%",
            rows = 3,
            placeholder = "Enter an optional comment for this export"
          ),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton("confirm_export_to_r_remove", "Export and Remove")
          ),
          easyClose = TRUE
        )
      )
      
      # Handle confirm button
      shiny::observeEvent(input$confirm_export_to_r_remove, {
        shiny::removeModal()
        comment <- input$export_comment_remove
        export_entry <- list(
          data = selected_df,
          comment = comment
        )
        
        # Append to global list
        cherry_picker_export_list <- get("cherry_picker_export_list", envir = .GlobalEnv)
        cherry_picker_export_list[[length(cherry_picker_export_list) + 1]] <- export_entry
        assign("cherry_picker_export_list", cherry_picker_export_list, envir = .GlobalEnv)
        
        # Remove exported rows from current working dataset
        df_remaining <- df[!(df$.row_uid %in% selected), , drop = FALSE]
        filtered_data(df_remaining)
        highlight_ids(c())  # clear highlights
        
        shiny::showNotification("Export added to R session and selections removed from data", type = "message")
      }, ignoreInit = TRUE, once = TRUE)
    })
    
    # ====== Footer ======
    output$app_footer <- shiny::renderUI({
      if (is.null(preloaded_data)) {
        shiny::tags$div(
          style = "margin-top:20px;",
          shiny::tags$p(
            "Cherry Picker (Powered by cherryPicker Package) © 2025 Matt Birch",
            style = "font-size: 0.7em; color: gray; margin: 0;"
          ),
          shiny::tags$p(
            "github.com/MattBirch42/cherryPicker",
            style = "font-size: 0.7em; color: gray; margin: 0;"
          )
        )
      } else {
        shiny::tags$div(
          style = "margin-top:20px;",
          shiny::tags$p(
            "Cherry Picker © 2025 Matt Birch",
            style = "font-size: 0.7em; color: gray; margin: 0;"
          ),
          shiny::tags$p(
            "github.com/MattBirch42/cherryPicker",
            style = "font-size: 0.7em; color: gray; margin: 0;"
          )
        )
      }
    })
  }
}
