#' Cherry Picker UI
#'
#' Internal function that builds the UI for the Cherry Picker app.
#'
#' @return A Shiny UI definition.
#' @keywords internal
cherry_picker_ui <- function() {
  shiny::fluidPage(
    theme = shinythemes::shinytheme("flatly"), 
    shiny::titlePanel("Cherry Picker"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::conditionalPanel(
          condition = "output.preloadedMode == false",
          shiny::fluidRow(
            shiny::column(
              width = 8,
              shiny::fileInput("file", "Upload csv or parquet file")
            ),
            shiny::column(
              width = 4,
              shiny::checkboxInput("header", "Data has Header", TRUE)
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::actionButton("do_filter", shiny::HTML("Optional:<br>Filter Data"), width = "100%")
            ),
            shiny::column(
              width = 6,
              shiny::actionButton("clear_filters",shiny::HTML("Optional:<br>Clear Filters"), width = "100%")
            )
          )
        ),
        shiny::conditionalPanel(
          condition = "output.preloadedMode == true",
          shiny::helpText("Using preloaded dataset"),
          shiny::actionButton("do_filter", shiny::HTML("Optional:<br>Filter Data")),
          shiny::actionButton("clear_filters", shiny::HTML("Optional:<br>Clear Filters"))
        ),
        # --- Selection counter always shown on main tab ---
        shiny::div(
          style = "margin: 20px 0;",
          shiny::uiOutput("selection_counter")
        ),
        shiny::selectInput("xvar", "X-axis variable", choices = NULL),
        shiny::selectInput("yvar", "Y-axis variable", choices = NULL),
        shiny::sliderInput("x_bins", "X histogram bins:", min = 5, max = 100, value = 30, step = 1),
        shiny::sliderInput("y_bins", "Y histogram bins:", min = 5, max = 100, value = 30, step = 1),
        shiny::actionButton("clear", "Clear Selected Points"),
        shiny::div(
          style = "margin-top: 5px;",
          shiny::actionButton("viz_without",  shiny::HTML("Visualize Dataset<br>Without Selected Points"))
        ),
        shiny::div(
          style = "margin-top: 20px;",
          shiny::downloadButton("download_selected", "Download Selected Data")
        )
      ),
      shiny::mainPanel(
        plotly::plotlyOutput("scatter"),
        shiny::uiOutput("app_footer")
      )
    ),
    # --- Styling for filter modal ---
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
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
