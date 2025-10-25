#' Cherry Picker Main UI
#'
#' Constructs the main Shiny user interface for the Cherry Picker application.
#' This tab-based layout provides a clean top navigation bar with the following tabs:
#' \itemize{
#'   \item \strong{Data Import and Filter:} Handles file upload and data filtering.
#'   \item \strong{Cherry Picker:} The main interactive data selection interface.
#'   \item \strong{Aggregations:} Displays aggregation and summary statistics.
#'   \item \strong{Linear Modeling:} Provides tools for fitting and visualizing linear models.
#'   \item \strong{Additional Modeling:} Offers advanced or experimental modeling tools.
#' }
#'
#' Each tab references a corresponding modular UI function:
#' \code{ui_data_import()}, \code{ui_cherry_picker()}, \code{ui_aggregation()},
#' \code{ui_linear_modeling()}, and \code{ui_additional_modeling()}.
#'
#' @return A Shiny UI definition for the Cherry Picker main application.
#' @keywords internal
#' @import shiny
#' @import shinythemes
#'
ui_cherry_picker_main <- function(imported_data = NULL) {
  shiny::fluidPage(
    theme = shinythemes::shinytheme("yeti"),
    shiny::titlePanel("Cherry Picker"),
    
    shiny::navbarPage(
      title = NULL,  # Title already shown above
      id = "main_tabs",
      
      # --- Tabs ---
      shiny::tabPanel(
        "Data Import and Filter",
        ui_data_import(imported_data)
      ),
      
      shiny::tabPanel(
        "Cherry Picker",
        ui_cherry_picker(imported_data)
      ),
      
      shiny::tabPanel(
        "Aggregations",
        ui_aggregation(imported_data)
      ),
      
      shiny::tabPanel(
        "Linear Modeling",
        ui_linear_modeling(imported_data)
      ),
      
      shiny::tabPanel(
        "Additional Modeling",
        ui_additional_modeling(imported_data)
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
