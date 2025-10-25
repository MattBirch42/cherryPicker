#' Cherry Picker Main Server
#'
#' Main server logic for the Cherry Picker application.
#'
#' This function coordinates all tab modules for:
#' - Data Import and Filter: file upload, preprocessing, and filtering.
#' - Cherry Picker: main interactive data selection and visualization.
#' - Aggregations: computation and display of grouped summaries.
#' - Linear Modeling: fitting and diagnostics of linear models.
#' - Additional Modeling: optional advanced or experimental modeling tools.
#'
#' Each tab is expected to have its own modular server function:
#' server_data_import(), server_cherry_picker(),
#' server_aggregation(), server_linear_modeling(),
#' and server_additional_modeling().
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#'
#' @return No return value; called for its side effects in Shiny.
#' @keywords internal
#' @import shiny
#'
server_cherry_picker_main <- function(input, output, session, imported_data = NULL) {
  
  # --- Reactive data store shared across modules ---
  # This holds the dataset currently in use, whether imported or preloaded
  rvals <- shiny::reactiveValues(
    data = NULL,
    filtered_data = NULL,
    selected_points = NULL,
    exported_data = NULL
  )
  
  # --- Data Import and Filtering ---
  # Expected to populate rvals$data and rvals$filtered_data
  if (exists("server_data_import")) {
    server_data_import(input, output, session, rvals)
  } else {
    message("server_data_import() not found - skipping Data Import tab logic.")
  }
  
  # --- Cherry Picker Tab ---
  # Handles scatterplots, selections, color schemes, etc.
  if (exists("server_cherry_picker")) {
    server_cherry_picker(input, output, session, rvals)
  } else {
    message("server_cherry_picker() not found - skipping Cherry Picker tab logic.")
  }
  
  # --- Aggregations Tab ---
  if (exists("server_aggregation")) {
    server_aggregation(input, output, session, rvals)
  } else {
    message("server_aggregation() not found - skipping Aggregation tab logic.")
  }
  
  # --- Linear Modeling Tab ---
  if (exists("server_linear_modeling")) {
    server_linear_modeling(input, output, session, rvals)
  } else {
    message("server_linear_modeling() not found - skipping Linear Modeling tab logic.")
  }
  
  # --- Additional Modeling Tab ---
  if (exists("server_additional_modeling")) {
    server_additional_modeling(input, output, session, rvals)
  } else {
    message("server_additional_modeling() not found - skipping Additional Modeling tab logic.")
  }
  
  # --- Global observers or logging (optional) ---
  shiny::observeEvent(rvals$data, {
    message(paste("Data loaded with", nrow(rvals$data), "rows."))
  }, ignoreInit = TRUE)
  
  shiny::observeEvent(rvals$selected_points, {
    message(paste(length(rvals$selected_points), "points currently selected."))
  }, ignoreInit = TRUE)
}
