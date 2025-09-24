#' Detect and Convert Date Columns
#'
#' Internal helper function. Scans through a data frame, looks for columns that
#' contain date-like strings, and converts them to proper `Date` objects.
#' Supports several common formats and falls back to Excel numeric dates if needed.
#'
#' @param df A data frame.
#' @param session Optional Shiny session, used for displaying warnings if mixed
#'   formats are detected.
#' @importFrom utils head
#' @importFrom stats na.omit
#' @return A data frame with date-like columns converted to `Date`.
#' @keywords internal
detect_and_convert_dates <- function(df, session = NULL) {
  fmt_candidates <- c("ymd", "mdy", "dmy", "Ymd", "Y/m/d", "m/d/Y", "d/m/Y")
  
  for (col in names(df)) {
    if (col == ".row_uid") next
    vec <- df[[col]]
    if (is.character(vec)) {
      sample_vals <- head(na.omit(vec), 10)
      if (length(sample_vals) > 0) {
        for (fmt in fmt_candidates) {
          parsed <- suppressWarnings(lubridate::parse_date_time(sample_vals, orders = fmt))
          if (sum(!is.na(parsed)) >= 5) {
            parsed_full <- suppressWarnings(lubridate::parse_date_time(vec, orders = fmt))
            # Excel fallback if NA remain
            if (any(is.na(parsed_full))) {
              nums <- suppressWarnings(as.numeric(vec))
              excel_dates <- as.Date(nums, origin = "1899-12-30")
              parsed_full[is.na(parsed_full) & !is.na(excel_dates)] <- excel_dates[is.na(parsed_full) & !is.na(excel_dates)]
            }
            df[[col]] <- as.Date(parsed_full)
            break
          }
        }
      }
    }
  }
  df
}
