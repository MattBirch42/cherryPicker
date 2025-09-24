#' Cherry Picker (Local R Use)
#'
#' Launch the Cherry Picker Shiny app directly from R with a preloaded dataset.
#' This skips the upload and filtering workflow and goes straight to the graphing
#' interface. Useful for exploring data frames, tibbles, or tsibbles that you
#' already have in your R session.
#'
#' @param df A dataset (data.frame, tibble, tsibble, etc.) to explore.
#' @return Launches a Shiny app in your R session. Does not return a value.
#' @export 
#' @examples
#' \dontrun{
#' CherryPicker(mtcars)
#' }
#' @export
cherry_picker <- function(df) {
  # force to data.frame for compatibility
  df <- as.data.frame(df)
  
  # launch app with preloaded data
  cherry_picker_app(preloaded_data = df)
}
