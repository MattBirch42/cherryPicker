#' Build Filter UI with NA Exclusion Toggles
#'
#' Dynamically build filters for each column in the dataset,
#' with an optional "Exclude NA" checkbox to the right.
#'
#' @param df Data frame.
#' @param ns Namespace function from moduleServer.
#' @return Shiny tag list of filter UI elements.
#' @keywords internal
# build_filter_ui <- function(df, ns) {
#   ui_list <- list()
#   
#   for (col in names(df)) {
#     if (col == ".row_uid") next
#     v <- df[[col]]
#     na_pct <- round(mean(is.na(v)) * 100, 1)
#     label <- sprintf("%s (%s, %s%% NA)", col, paste(class(v), collapse = "/"), na_pct)
#     
#     # Construct the filter input itself
#     filter_input <- NULL
#     
#     if (inherits(v, "Date") || inherits(v, "POSIXt")) {
#       rng <- range(as.Date(v), na.rm = TRUE)
#       if (is.finite(rng[1]) && is.finite(rng[2])) {
#         filter_input <- shiny::dateRangeInput(
#           inputId = ns(paste0("filter_", col)),
#           label   = label,
#           start   = rng[1],
#           end     = rng[2],
#           width   = "100%"
#         )
#       }
#       
#     } else if (is.numeric(v)) {
#       vmin <- suppressWarnings(min(v, na.rm = TRUE))
#       vmax <- suppressWarnings(max(v, na.rm = TRUE))
#       if (is.finite(vmin) && is.finite(vmax)) {
#         filter_input <- shiny::sliderInput(
#           inputId = ns(paste0("filter_", col)),
#           label   = label,
#           min = vmin, max = vmax,
#           value = c(vmin, vmax),
#           width = "100%"
#         )
#       }
#       
#     } else {
#       vals <- sort(unique(v))
#       vals <- vals[!is.na(vals)]
#       if (length(vals) > 50) {
#         filter_input <- shiny::tags$p(
#           sprintf("%s: Filtering unavailable (>50 unique values).", col),
#           style = "font-style: italic; color: gray;"
#         )
#       } else {
#         filter_input <- shiny::selectizeInput(
#           inputId = ns(paste0("filter_", col)),
#           label   = label,
#           choices = vals,
#           selected = vals,
#           multiple = TRUE,
#           width = "100%",
#           options = list(plugins = list("remove_button"))
#         )
#       }
#     }
#     
#     # Add the NA toggle checkbox next to it
#     na_checkbox <- shiny::checkboxInput(
#       inputId = ns(paste0("exclude_na_", col)),
#       label = "Exclude NA",
#       value = FALSE,
#       width = "100%"
#     )
#     
#     # Combine into a single row
#     ui_list[[col]] <- shiny::fluidRow(
#       shiny::column(width = 9, filter_input),
#       shiny::column(width = 3, style = "margin-top: 28px;", na_checkbox)
#     )
#   }
#   
#   shiny::tagList(ui_list)
# }
build_filter_ui <- function(con, table, ns) {
  
  preview_df <- tbl(rvals$con.app, "data") %>%
    head(10) %>%
    collect()
  
  tagList(
    tableOutput(ns("data_preview"))
  )
}
