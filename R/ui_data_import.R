#' Data Import and Filter UI (Module Version)
#'
#' Provides a full-page layout for uploading and optionally filtering data,
#' including metadata summary after upload and user-selected auto-detection options.
#'
#' @param id A unique module identifier.
#' @return A Shiny UI definition for use in a modular app.
#' @keywords internal
#' @import shiny
ui_data_import <- function(id) {
  ns <- shiny::NS(id) # create namespace for all IDs

  shiny::fluidPage(
    shiny::titlePanel("Data Import and Filtering"),

    # --- Upload controls ---
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::fileInput(
          ns("file"),
          "Upload CSV or Parquet file",
          width = "100%"
        )
      ),
      shiny::column(
        width = 3,
        shiny::checkboxInput(ns("header"), "Data has Header", TRUE)
      )
    ),

    # --- Conversion options ---
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::checkboxInput(
          ns("auto_timestamps"),
          "Autodetect timestamps",
          TRUE
        )
      ),
      shiny::column(
        width = 4,
        shiny::checkboxInput(ns("auto_dates"), "Autodetect dates", TRUE)
      ),
      shiny::column(
        width = 4,
        shiny::checkboxInput(
          ns("convert_characters"),
          "Convert characters to factors",
          TRUE
        )
      )
    ),

    shiny::hr(),

    # --- Metadata summary (appears after upload) ---
    shiny::uiOutput(ns("data_meta_summary")),

    shiny::hr(),

    # --- Filter buttons ---
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::actionButton(
          ns("do_filter"),
          shiny::HTML("Optional:<br>Filter Data"),
          width = "100%"
        )
      ),
      shiny::column(
        width = 6,
        shiny::actionButton(
          ns("clear_filters"),
          shiny::HTML("Optional:<br>Clear Filters"),
          width = "100%"
        )
      )
    ),

    shiny::hr(),

    # --- Data preview section ---
    shiny::uiOutput(ns("data_preview")),

    # --- Styling for modal and meta table ---
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(
        "
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
        .meta-table td, .meta-table th {
          padding: 4px 8px;
          border: 1px solid #ccc;
        }
        .meta-table {
          border-collapse: collapse;
          width: 100%;
          font-size: 0.9em;
        }
      "
      ))
    )
  )
}
