#' Cherry Picker (Shiny Upload Mode)
#'
#' Launch the Cherry Picker Shiny app in hosted mode. In this mode the app
#' does not preload data; instead the user uploads a file (CSV) and can apply
#' filters before exploring it. This is the version designed for deployment
#' on RStudio Connect or other Shiny hosting services.
#'
#' @return A running Shiny app.
#' @examples
#' \dontrun{
#' RunCherryPicker()
#' }
#' @export
run_cherry_picker <- function() {
  # launch app without preloaded data
  cherry_picker_app(preloaded_data = NULL)
}
