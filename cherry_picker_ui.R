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
          shiny::checkboxInput("header", "Header", TRUE)
        ),
        shiny::conditionalPanel(
          condition = "output.preloadedMode == true",
          shiny::helpText("Using preloaded dataset")
        ),
        shiny::selectInput("xvar", "X-axis variable", choices = NULL),
        shiny::selectInput("yvar", "Y-axis variable", choices = NULL),
        shiny::actionButton("clear", "Clear Highlights"),
        shiny::actionButton("viz_without", "Visualize Without Selected Points"),
        shiny::downloadButton("download_selected", "Download Selected Data"),
        shiny::br(), shiny::br(),
        shiny::uiOutput("selection_counter")
      ),
      shiny::mainPanel(
        plotly::plotlyOutput("scatter"),
        shiny::uiOutput("app_footer")
      )
    )
  )
}
