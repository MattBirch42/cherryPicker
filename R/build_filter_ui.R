#' Build Shiny Filter UI
#'
#' Generate filter controls dynamically for a dataset. 
#' - Date/POSIXct columns get a date range input
#' - Numeric get a slider
#' - Character/factor/logical get a multi-select (unless too many uniques)
#' Columns named ".row_uid" are skipped.
#'
#' @param df Data frame.
#' @return A list of shiny UI elements (to be wrapped in tagList()).
#' @keywords internal
build_filter_ui <- function(df) {
  ui_list <- list()
  for (col in names(df)) {
    if (col == ".row_uid") next
    v <- df[[col]]
    na_pct <- round(mean(is.na(v)) * 100, 1)
    label <- sprintf("%s (%s, %s%% NA)", col, paste(class(v), collapse = "/"), na_pct)
    
    # Date or POSIXct -> dateRangeInput
    if (inherits(v, "Date") || inherits(v, "POSIXt")) {
      rng <- range(as.Date(v), na.rm = TRUE)
      if (is.finite(rng[1]) && is.finite(rng[2])) {
        ui_list[[col]] <- shiny::dateRangeInput(
          inputId = paste0("filter_", col),
          label   = label,
          start   = rng[1],
          end     = rng[2]
        )
      }
      
      # Numeric -> slider
    } else if (is.numeric(v)) {
      vmin <- suppressWarnings(min(v, na.rm = TRUE))
      vmax <- suppressWarnings(max(v, na.rm = TRUE))
      if (is.finite(vmin) && is.finite(vmax)) {
        ui_list[[col]] <- shiny::sliderInput(
          inputId = paste0("filter_", col),
          label   = label,
          min = vmin, max = vmax,
          value = c(vmin, vmax)
        )
      }
      
      # Character/factor/logical -> multi-select (with guard on unique count)
    } else {
      vals <- sort(unique(v))
      vals <- vals[!is.na(vals)]
      if (length(vals) > 50) {
        ui_list[[col]] <- shiny::tags$p(
          sprintf("%s: Filtering unavailable for variables with more than 50 unique non-numeric and non-time values.", col),
          style = "font-style: italic; color: gray;"
        )
      } else {
        ui_list[[col]] <- shiny::selectizeInput(
          inputId = paste0("filter_", col),
          label   = label,
          choices = vals,
          selected = vals,
          multiple = TRUE,
          options = list(plugins = list("remove_button"))
        )
      }
    }
  }
  ui_list
}
