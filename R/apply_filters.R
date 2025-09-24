#' Apply Filters to Data
#'
#' Apply user-selected filters from Shiny UI to a dataset.
#' NA values are always retained unless a filter would exclude them
#' (for numeric/date ranges or set filters).
#'
#' @param df Data frame.
#' @param input Shiny input object.
#' @return Filtered data frame.
#' @keywords internal
apply_filters <- function(df, input) {
  out <- df
  for (col in names(df)) {
    if (col == ".row_uid") next
    id <- paste0("filter_", col)
    v <- df[[col]]
    if (!is.null(input[[id]])) {
      if (inherits(v, "Date")) {
        dr <- input[[id]]
        if (length(dr) == 2 && all(!is.na(dr))) {
          out <- out[(is.na(out[[col]]) | (out[[col]] >= dr[1] & out[[col]] <= dr[2])), , drop = FALSE]
        }
      } else if (is.numeric(v)) {
        rng <- input[[id]]
        if (length(rng) == 2 && all(is.finite(rng))) {
          out <- out[(is.na(out[[col]]) | (out[[col]] >= rng[1] & out[[col]] <= rng[2])), , drop = FALSE]
        }
      } else {
        vals <- input[[id]]
        if (!is.null(vals)) {
          out <- out[(is.na(out[[col]]) | out[[col]] %in% vals), , drop = FALSE]
        }
      }
    }
  }
  out
}