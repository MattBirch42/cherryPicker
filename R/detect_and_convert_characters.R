#' Detect and Convert Character Columns to Factors
#'
#' Internal helper function. Scans a data frame for columns of type character
#' and converts them to factors. Skips helper or complex columns (e.g. lists).
#' Optionally warns if a column has more than a threshold of unique values,
#' since large factor sets can be expensive or unhelpful.
#'
#' @param df A data frame.
#' @param unique_warn_threshold Integer. If a character column has more than this
#'   many unique values, a warning will be issued (default 500).
#' @param session Optional Shiny session, used for displaying warnings in Shiny.
#' @return A data frame with character columns converted to factors.
#' @keywords internal
detect_and_convert_characters <- function(
  df,
  unique_warn_threshold = 50,
  session = NULL
) {
  for (col in names(df)) {
    print(col)
    # Skip helper or list-like columns
    if (col == ".row_uid" || is.list(df[[col]])) {
      next
    }

    vec <- df[[col]]

    if (is.character(vec)) {
      unique_vals <- unique(na.omit(vec))
      nlev <- length(unique_vals)

      # Only check threshold if there are any non-missing values
      if (nlev > 0 && nlev > unique_warn_threshold) {
        msg <- sprintf(
          "Column '%s' has %d unique values; converting to factor anyway.",
          col,
          nlev
        )
        if (!is.null(session)) {
          shiny::showNotification(msg, type = "warning")
        } else {
          warning(msg, call. = FALSE)
        }
      }

      df[[col]] <- factor(vec)
    }
  }
  return(df)
}
