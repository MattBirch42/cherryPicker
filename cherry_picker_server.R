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
    # detect whether preloaded
    output$preloadedMode <- shiny::reactive({ !is.null(preloaded_data) })
    shiny::outputOptions(output, "preloadedMode", suspendWhenHidden = FALSE)
    
    # choose raw data
    raw_data <- shiny::reactive({
      if (!is.null(preloaded_data)) {
        df <- as.data.frame(preloaded_data)
      } else {
        shiny::req(input$file)
        df <- utils::read.csv(input$file$datapath, header = input$header, stringsAsFactors = FALSE)
      }
      df$.row_uid <- seq_len(nrow(df))
      detect_and_convert_dates(df, session)
    })
    
    # update select inputs when data changes
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
    
    # persistent highlights
    highlight_ids <- shiny::reactiveVal(c())
    
    # main scatter plot
    output$scatter <- plotly::renderPlotly({
      shiny::req(input$xvar, input$yvar)
      make_marginal_scatter(raw_data(), input$xvar, input$yvar, highlight_ids())
    })
    
    # click handler
    shiny::observeEvent(plotly::event_data("plotly_click", source = "scatterplot"), {
      ed <- plotly::event_data("plotly_click", source = "scatterplot")
      if (!is.null(ed) && !is.null(ed$customdata)) {
        highlight_ids(union(highlight_ids(), ed$customdata))
      }
    })
    
    # lasso handler
    shiny::observeEvent(plotly::event_data("plotly_selected", source = "scatterplot"), {
      ed <- plotly::event_data("plotly_selected", source = "scatterplot")
      if (!is.null(ed) && !is.null(ed$customdata)) {
        highlight_ids(union(highlight_ids(), ed$customdata))
      }
    })
    
    # clear button
    shiny::observeEvent(input$clear, { highlight_ids(c()) })
    
    # visualize without selected points (popup modal)
    shiny::observeEvent(input$viz_without, {
      df <- raw_data()
      removed <- highlight_ids()
      filtered_df <- df[!(df$.row_uid %in% removed), , drop = FALSE]
      
      shiny::showModal(
        shiny::modalDialog(
          title = "Scatter Plot Without Selected Points",
          shiny::selectInput("modal_xvar", "X-axis variable", choices = setdiff(names(filtered_df), ".row_uid")),
          shiny::selectInput("modal_yvar", "Y-axis variable", choices = setdiff(names(filtered_df), ".row_uid")),
          plotly::plotlyOutput("scatter_without"),
          easyClose = TRUE,
          size = "l"
        )
      )
      
      # default modal selections
      shiny::updateSelectInput(session, "modal_xvar", selected = setdiff(names(filtered_df), ".row_uid")[1])
      shiny::updateSelectInput(session, "modal_yvar", selected = setdiff(names(filtered_df), ".row_uid")[2])
    })
    
    # render scatter in modal
    output$scatter_without <- plotly::renderPlotly({
      shiny::req(input$modal_xvar, input$modal_yvar)
      df <- raw_data()
      removed <- highlight_ids()
      filtered_df <- df[!(df$.row_uid %in% removed), , drop = FALSE]
      make_marginal_scatter(filtered_df, input$modal_xvar, input$modal_yvar)
    })
    
    # download selected data
    output$download_selected <- shiny::downloadHandler(
      filename = function() {
        "cherry_picker_output.csv"
      },
      content = function(file) {
        df <- raw_data()
        selected <- highlight_ids()
        if (length(selected) > 0) {
          selected_df <- df[df$.row_uid %in% selected, , drop = FALSE]
        } else {
          selected_df <- df[0, , drop = FALSE] # empty if none selected
        }
        utils::write.csv(selected_df, file, row.names = FALSE)
      }
    )
    
    # footer
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
