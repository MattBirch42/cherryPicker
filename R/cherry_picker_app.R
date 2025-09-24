#' Core Cherry Picker App
#'
#' Launches the Shiny app with either uploaded or preloaded data.
#'
#' @param preloaded_data Optional data frame to skip file upload.
#' @return A Shiny app object.
#' @keywords internal
#' 
cherry_picker_app <- function(preloaded_data = NULL) {
  shiny::shinyApp(
    ui = cherry_picker_ui(),
    server = cherry_picker_server(preloaded_data)
  )
}
