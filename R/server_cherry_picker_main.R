#' Cherry Picker Main Server (modular)
#'
#' Coordinates all tab modules for the Cherry Picker application.
#' @keywords internal
#' @import shiny
server_cherry_picker_main <- function(input, output, session, preloaded_data = NULL) {
  
  # --- Shared reactive data store ---
  rvals <- shiny::reactiveValues(
    data = preloaded_data,
    filtered_data = NULL,
    selected_points = NULL,
    exported_data = NULL
  )
  
  # --- Data Import and Filtering ---
  server_data_import("import", rvals, preloaded_data)
  
  # --- Cherry Picker Tab ---
  server_cherry_picker("picker", rvals)
  
  # --- Aggregations Tab ---
  server_aggregation("agg", rvals)
  
  # --- Linear Modeling Tab ---
  server_linear_modeling("lm", rvals)
  
  # --- Additional Modeling Tab ---
  server_additional_modeling("extra", rvals)
  
  # --- Global observers / logging ---
  shiny::observeEvent(rvals$data, {
    message(paste("Data loaded with", nrow(rvals$data), "rows."))
  }, ignoreInit = TRUE)
  
  shiny::observeEvent(rvals$selected_points, {
    message(paste(length(rvals$selected_points), "points currently selected."))
  }, ignoreInit = TRUE)
}
