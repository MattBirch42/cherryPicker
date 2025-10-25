#' Core Cherry Picker App
#'
#' Launches the Shiny app with either uploaded or preloaded data.
#'
#' @param preloaded_data Optional data frame to skip file upload.
#' @return A Shiny app object.
#' @keywords internal
#' 
# cherry_picker_app <- function(preloaded_data = NULL) {
#   shiny::shinyApp(
#     ui = cherry_picker_ui(),
#     server = cherry_picker_server(preloaded_data)
#   )
# }

# cherry_picker_app <- function(preloaded_data = NULL) {
#   shiny::shinyApp(
#     ui = ui_cherry_picker_main(),
#     server = server_cherry_picker_main
#   )
# }


files <- setdiff(list.files(file.path("R")),"cherry_picker_app.R")
for (i in 1:length(files)) {
  message(i," / ",length(files)," | ",files[i])
  source(file.path("R",files[i]))
}

fake.data <- fake_data(10000,42)

cherry_picker_app <- function(preloaded_data = NULL) {
  shiny::shinyApp(
    ui = ui_cherry_picker_main(preloaded_data = preloaded_data),
    server = function(input, output, session) {
      server_cherry_picker_main(input, output, session, preloaded_data = preloaded_data)
    }
  )
}

cherry_picker_app(fake_data)
