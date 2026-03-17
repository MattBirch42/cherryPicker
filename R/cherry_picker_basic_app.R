#' Cherry Picker Basic
#'
#' Launches a minimal Cherry Picker app using data passed directly from the
#' R session. No file upload or data import is required. The user's data
#' should already be cleaned and typed before calling this function.
#'
#' Selections and removals are tracked as persistent reactive UID vectors.
#' Raw data is stored in DuckDB and never mutated. Exports are appended to
#' \code{cherry_picker_export_list} in the global environment.
#'
#' @param data A data frame to visualise and interact with.
#'
#' @return A Shiny app object (launched via \code{shiny::runApp()}).
#' @export
#'
#' @examples
#' \dontrun{
#' cherry_picker_basic(mtcars)
#' }
cherry_picker_basic <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (nrow(data) == 0) {
    stop("`data` must have at least one row.")
  }

  shiny::shinyApp(
    ui     = cherry_picker_basic_ui(),
    server = cherry_picker_basic_server(preloaded_data = data)
  )
}
