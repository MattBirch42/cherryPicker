#' Cherry Picker Server
#'
#' Internal function that defines the server logic for the Cherry Picker app.
#'
#' @param preloaded_data Optional data frame. If provided, the app starts
#'   immediately with this dataset. If `NULL`, the user must upload a file.
#'
#' @return A Shiny server function.
#' @keywords internal
cherry_picker_server <- function(preloaded_data = NULL) {
  function(input, output, session) {
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
        df <- utils::read.csv(input$file$datapath, header = input$header, stringsAsFactors = FALSE)
        df$.row_uid <- seq_len(nrow(df))
        df <- detect_and_convert_dates(df, session)
        uploaded_data(df)
        
        if (nrow(df) > 200) {
          shiny::showModal(
            shiny::modalDialog(
              title = "Large Data Warning",
              shiny::p(
                paste0("Your dataset has ", nrow(df), " rows and ", ncol(df),
                       " columns. Larger datasets may slow down the app. ",
                       "Do you want to filter first?")
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
        uploaded_data(df)
        filter_mode(FALSE)
        filtered_data(df)
      }
    })
    
    # app’s dataset
    raw_data <- shiny::reactive({
      shiny::req(filtered_data())
      filtered_data()
    })
    
    # update variable choices
    shiny::observe({
      df <- raw_data()
      choices <- setdiff(names(df), ".row_uid")
      if (length(choices) >= 2) {
        shiny::updateSelectInput(session, "xvar", choices = choices, selected = choices[1])
        shiny::updateSelectInput(session, "yvar", choices = choices, selected = choices[2])
      } else if (length(choices) == 1) {
        shiny::updateSelectInput(session, "xvar", choices = choices, selected = choices[1])
      }
    })
    
    # ====== Selections ======
    highlight_ids <- shiny::reactiveVal(c())
    
    output$scatter <- plotly::renderPlotly({
      shiny::req(input$xvar, input$yvar)
      make_marginal_scatter(raw_data(), input$xvar, input$yvar, highlight_ids())
    })
    
    shiny::observeEvent(plotly::event_data("plotly_click", source = "scatterplot"), {
      ed <- plotly::event_data("plotly_click", source = "scatterplot")
      if (!is.null(ed) && !is.null(ed$customdata)) {
        highlight_ids(union(highlight_ids(), ed$customdata))
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
      
      shiny::updateSelectInput(session, "modal_xvar",
                               selected = setdiff(names(filtered_df), ".row_uid")[1])
      shiny::updateSelectInput(session, "modal_yvar",
                               selected = setdiff(names(filtered_df), ".row_uid")[2])
    })
    
    output$scatter_without <- plotly::renderPlotly({
      shiny::req(input$modal_xvar, input$modal_yvar)
      df <- raw_data()
      removed <- highlight_ids()
      filtered_df <- df[!(df$.row_uid %in% removed), , drop = FALSE]
      make_marginal_scatter(filtered_df, input$modal_xvar, input$modal_yvar)
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
    output$download_selected <- shiny::downloadHandler(
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
    
    # ====== Footer ======
    output$app_footer <- shiny::renderUI({
      if (is.null(preloaded_data)) {
        shiny::tags$p(
          "Cherry Picker (Shiny App mode) · © 2025 Matt Birch · github.com/MattBirch42/coolstuff",
          style = "font-size: 0.8em; color: gray;"
        )
      } else {
        shiny::tags$p(
          "Cherry Picker (R Function mode) · © 2025 Matt Birch · github.com/MattBirch42/coolstuff",
          style = "font-size: 0.8em; color: gray;"
        )
      }
    })
  }
}
