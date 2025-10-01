#' Cherry Picker UI
#'
#' Internal function that builds the UI for the Cherry Picker app.
#'
#' @return A Shiny UI definition.
#' @keywords internal
cherry_picker_ui <- function() {
  shiny::fluidPage(
    theme = shinythemes::shinytheme("yeti"), 
    shiny::titlePanel("Cherry Picker"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # Always-visible selection counter
        shiny::div(
          style = "margin: 20px 0;",
          shiny::uiOutput("selection_counter")
        ),
        
        # Collapsible groups
        shinyBS::bsCollapse(
          id = "sidebar_panels", 
          open = c("Graph Axis Options"), 
          multiple = TRUE,  
          
          # 1) Data and Filtering Options
          shinyBS::bsCollapsePanel(
            "Data and Filtering Options",
            # Preloaded == false
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
              # <-- stack buttons vertically instead of side-by-side
              shiny::actionButton("do_filter", shiny::HTML("Optional:<br>Filter Data"), width = "100%"),
              shiny::actionButton("clear_filters", shiny::HTML("Optional:<br>Clear Filters"), width = "100%")
            ),
            # Preloaded == true
            shiny::conditionalPanel(
              condition = "output.preloadedMode == true",
              shiny::helpText("Using preloaded dataset"),
              shiny::actionButton("do_filter", shiny::HTML("Optional:<br>Filter Data"), width = "100%"),
              shiny::actionButton("clear_filters", shiny::HTML("Optional:<br>Clear Filters"), width = "100%")
            ),
            style = "primary"
          ),
          
          # 2) Graph Axis Options (default open)
          shinyBS::bsCollapsePanel(
            "Graph Options",
            shiny::selectInput("xvar", "X-axis variable", choices = NULL),
            shiny::selectInput("yvar", "Y-axis variable", choices = NULL),
            shiny::sliderInput("x_bins", "X histogram bins:", min = 5, max = 100, value = 30, step = 1),
            shiny::sliderInput("y_bins", "Y histogram bins:", min = 5, max = 100, value = 30, step = 1),
            shiny::actionButton("clear", "Clear Selected Points"),
            shiny::div(style = "margin-top: 5px;",
                       shiny::actionButton("viz_without", shiny::HTML("Visualize Dataset<br>Without Selected Points"))),
            style = "primary"
          ),
          
          # 3) Data Selection and Export Options
          shinyBS::bsCollapsePanel(
            "Data Selection and Export Options",
            
            shiny::actionButton(
              "export_to_r",
              shiny::HTML("Export Selections to R<br>and Keep Them"),
              width = "100%"),
            
            shiny::actionButton(
              "export_to_r_remove",
              shiny::HTML("Export Selections to R<br>and Remove Them"),
              width = "100%"
            ),
            
            shiny::downloadButton(
              "export_to_csv_keep",
              shiny::HTML("Download and Keep<br>Selected Data"),
              width = "100%"
            ),
            
            shiny::downloadButton(
              "export_to_csv_remove",
              shiny::HTML("Download and Remove<br>Selected Data"),
              width = "100%"
            ),
            
            style = "primary"
          )
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
