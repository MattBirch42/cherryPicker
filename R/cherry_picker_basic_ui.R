#' Cherry Picker Basic UI
#'
#' Internal function that builds the UI for the basic Cherry Picker app.
#' Three tabs: Visualization, Statistical Comparisons, Data Export.
#'
#' @return A Shiny UI definition.
#' @keywords internal
cherry_picker_basic_ui <- function() {
  shiny::navbarPage(
    title = "Cherry Picker",
    theme = shinythemes::shinytheme("yeti"),

    # =========================================================
    # TAB 1: Visualization
    # =========================================================
    shiny::tabPanel(
      "Visualization",
      shiny::sidebarLayout(
        shiny::sidebarPanel(

          # Selection counter (always visible)
          shiny::div(
            style = "margin-bottom: 15px;",
            shiny::uiOutput("selection_counter")
          ),

          shinyBS::bsCollapse(
            id = "sidebar_panels",
            open = "Graph Options",
            multiple = TRUE,

            # Graph Options
            shinyBS::bsCollapsePanel(
              "Graph Options",
              shiny::selectInput("xvar", "X-axis variable", choices = NULL),
              shiny::selectInput("yvar", "Y-axis variable", choices = NULL),
              shiny::sliderInput(
                "x_bins",
                "X histogram bins:",
                min = 5, max = 100, value = 30, step = 1
              ),
              shiny::sliderInput(
                "y_bins",
                "Y histogram bins:",
                min = 5, max = 100, value = 30, step = 1
              ),
              shiny::div(
                style = "margin-top: 5px;",
                shiny::actionButton(
                  "add_color",
                  shiny::HTML("Optional:<br>Add Color Scheme"),
                  width = "100%"
                )
              ),
              shiny::div(
                style = "margin-top: 5px;",
                shiny::actionButton(
                  "save_selected",
                  "Save Selections",
                  width = "100%",
                  class = "btn-success"
                )
              ),
              shiny::div(
                style = "margin-top: 5px;",
                shiny::actionButton(
                  "clear_unsaved",
                  "Clear Unsaved Selections",
                  width = "100%"
                )
              ),
              shiny::div(
                style = "margin-top: 5px;",
                shiny::actionButton(
                  "clear_all_selected",
                  "Clear All Selections",
                  width = "100%",
                  class = "btn-danger"
                )
              ),
              style = "primary"
            )
          )
        ),

        shiny::mainPanel(
          plotly::plotlyOutput("scatter", height = "600px"),
          shiny::uiOutput("app_footer")
        )
      )
    ),

    # =========================================================
    # TAB 2: Statistical Comparisons
    # =========================================================
    shiny::tabPanel(
      "Statistical Comparisons",
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::div(
            style = "margin-top: 60px; text-align: center; color: gray;",
            shiny::h4("Statistical Comparisons — Coming Soon"),
            shiny::p(
              "This tab will show summary statistics and regression results ",
              "comparing selected vs. unselected observations."
            )
          )
        )
      )
    ),

    # =========================================================
    # TAB 3: Data Export
    # =========================================================
    shiny::tabPanel(
      "Data Export",
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::wellPanel(
            shiny::h4("Selection Status"),
            shiny::uiOutput("export_counter"),
            shiny::hr(),

            shiny::h4("Mark Rows for Removal"),
            shiny::p(
              shiny::tags$small(
                "Marks currently selected points as removed. ",
                "Removed rows are excluded from exports below."
              )
            ),
            shiny::actionButton(
              "mark_removed",
              shiny::HTML("Mark Selected as Removed"),
              width = "100%",
              class = "btn-warning"
            ),
            shiny::div(
              style = "margin-top: 5px;",
              shiny::actionButton(
                "clear_removed",
                "Clear Removed Flag",
                width = "100%"
              )
            )
          )
        ),

        shiny::column(
          width = 4,
          shiny::wellPanel(
            shiny::h4("Export to R Session"),
            shiny::p(
              shiny::tags$small(
                "Exports are appended to ",
                shiny::tags$code("cherry_picker_export_list"),
                " in your R session."
              )
            ),
            shiny::actionButton(
              "export_to_r_keep",
              shiny::HTML("Export Selected to R<br>(Keep in App)"),
              width = "100%",
              class = "btn-primary"
            ),
            shiny::div(
              style = "margin-top: 5px;",
              shiny::actionButton(
                "export_to_r_remove",
                shiny::HTML("Export Selected to R<br>(Mark as Removed)"),
                width = "100%",
                class = "btn-primary"
              )
            ),
            shiny::div(
              style = "margin-top: 5px;",
              shiny::actionButton(
                "export_full_r",
                shiny::HTML("Export Full Data with Flags<br>to R"),
                width = "100%"
              )
            )
          )
        ),

        shiny::column(
          width = 4,
          shiny::wellPanel(
            shiny::h4("Export to CSV"),
            shiny::downloadButton(
              "export_csv_selected",
              shiny::HTML("Download Selected Rows"),
              width = "100%"
            ),
            shiny::div(
              style = "margin-top: 5px;",
              shiny::downloadButton(
                "export_csv_remaining",
                shiny::HTML("Download Remaining Rows<br>(Excluding Removed)"),
                width = "100%"
              )
            ),
            shiny::div(
              style = "margin-top: 5px;",
              shiny::downloadButton(
                "export_csv_full",
                shiny::HTML("Download Full Data<br>with Flags"),
                width = "100%"
              )
            )
          )
        )
      )
    )
  )
}
