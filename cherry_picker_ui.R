#' Cherry Picker UI
#'
#' Internal function that builds the UI for the Cherry Picker app.
#'
#' @return A Shiny UI definition.
#' @keywords internal
cherry_picker_ui <- function() {
  shiny::fluidPage(
    shiny::titlePanel("Cherry Picker"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::conditionalPanel(
          condition = "output.preloadedMode == false",
          shiny::fileInput("file", "Upload CSV"),
          shiny::checkboxInput("header", "Header", TRUE),
          shiny::actionButton("do_filter", "Apply Filters to Data Set"),
          shiny::actionButton("clear_filters", "Clear Filters")
        ),
        shiny::conditionalPanel(
          condition = "output.preloadedMode == true",
          shiny::helpText("Using preloaded dataset")
        ),
        # --- Selection counter always shown on main tab ---
        shiny::uiOutput("selection_counter"),
        shiny::selectInput("xvar", "X-axis variable", choices = NULL),
        shiny::selectInput("yvar", "Y-axis variable", choices = NULL),
        shiny::actionButton("clear", "Clear Highlights"),
        shiny::actionButton("viz_without", "Visualize Dataset Without Selected Points"),
        shiny::downloadButton("download_selected", "Download Selected Data")
      ),
      shiny::mainPanel(
        plotly::plotlyOutput("scatter"),
        shiny::uiOutput("app_footer")
      )
    ),
    # --- Styling for filter modal ---
    shiny::tags$head(
      shiny::tags$style(HTML("
        /* Make filter modal body scrollable */
        .modal-body {
          max-height: 60vh;
          overflow-y: auto;
        }
        /* Keep footer pinned with counter + buttons */
        .modal-footer {
          position: sticky;
          bottom: 0;
          background: white;
          border-top: 1px solid #ccc;
          padding: 10px;
          display: flex;
          justify-content: space-between;
          align-items: center;
        }
      "))
    )
  )
}
