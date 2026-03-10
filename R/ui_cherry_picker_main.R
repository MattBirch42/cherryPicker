ui_cherry_picker_main <- function(preloaded_data = NULL) {
  # Choose default tab name
  default_tab <- if (!is.null(preloaded_data)) {
    "Cherry Picker"
  } else {
    "Data Import and Filter"
  }

  shiny::fluidPage(
    theme = shinythemes::shinytheme("yeti"),
    shiny::titlePanel("Cherry Picker"),

    shiny::navbarPage(
      title = NULL,
      id = "main_tabs",
      selected = default_tab, # <-- set default tab here

      shiny::tabPanel("Data Import and Filter", ui_data_import("import")),
      shiny::tabPanel("Cherry Picker", ui_cherry_picker("picker")),
      shiny::tabPanel("Aggregations", ui_aggregation("agg")),
      shiny::tabPanel("Linear Modeling", ui_linear_modeling("lm")),
      shiny::tabPanel("Additional Modeling", ui_additional_modeling("extra"))
    ),

    shiny::tags$head(
      shiny::tags$style(shiny::HTML(
        "
        .navbar-nav > li > a { 
          font-weight: 500;
          font-size: 15px;
        }
        .tab-content { padding-top: 15px; }
      "
      ))
    )
  )
}
