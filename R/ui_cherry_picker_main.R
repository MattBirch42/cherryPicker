#' Cherry Picker Main UI (Modular)
#'
#' Constructs the main Shiny user interface for the Cherry Picker application.
#' Each tab is implemented as a Shiny module, providing:
#' - Data Import and Filter
#' - Cherry Picker
#' - Aggregations
#' - Linear Modeling
#' - Additional Modeling
#'
#' @return A Shiny UI definition for the Cherry Picker main application.
#' @keywords internal
#' @import shiny
#' @import shinythemes
#'
ui_cherry_picker_main <- function(preloaded_data = NULL) {
  shiny::fluidPage(
    theme = shinythemes::shinytheme("yeti"),
    shiny::titlePanel("Cherry Picker"),
    
    shiny::navbarPage(
      title = NULL,  # Title already shown above
      id = "main_tabs",
      
      # --- Tabs (now modular) ---
      shiny::tabPanel(
        "Data Import and Filter",
        ui_data_import("import")
      ),
      
      shiny::tabPanel(
        "Cherry Picker",
        ui_cherry_picker("picker")
      ),
      
      shiny::tabPanel(
        "Aggregations",
        ui_aggregation("agg")
      ),
      
      shiny::tabPanel(
        "Linear Modeling",
        ui_linear_modeling("lm")
      ),
      
      shiny::tabPanel(
        "Additional Modeling",
        ui_additional_modeling("extra")
      )
    ),
    
    # --- Optional global styling ---
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        /* Style tweaks for a clean modern layout */
        .navbar-nav > li > a { 
          font-weight: 500;
          font-size: 15px;
        }
        .tab-content {
          padding-top: 15px;
        }
      "))
    )
  )
}
