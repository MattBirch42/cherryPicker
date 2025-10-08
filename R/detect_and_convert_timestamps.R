#' Detect and Convert Timestamp Columns
#'
#' Internal helper function. Scans through a data frame, looks for columns that
#' contain timestamp-like strings, and converts them to proper POSIXct objects.
#' Supports several common formats (ISO, US with AM/PM, etc.) and falls back to
#' Excel numeric dates if needed.
#'
#' @param df A data frame.
#' @param session Optional Shiny session, used for displaying warnings if mixed
#'   formats are detected.
#' @return A data frame with timestamp-like columns converted to POSIXct.
#' @keywords internal
detect_and_convert_timestamps <- function(df, session = NULL) {
  fmt_candidates <- c(
    "Y-m-d H:M:S",     # 2024-12-31 23:59:59
    "m/d/Y I:M:S p",   # 12/31/2024 11:59:59 PM
    "d/m/Y H:M:S",     # 31/12/2024 23:59:59
    "Y/m/d H:M:S",     # 2024/12/31 23:59:59
    "Ymd HMS",         # 20241231 235959
    "Y-m-d"            # plain date, still upgrade to timestamp
  )
  
  for (col in names(df)) {
    # skip technical helper cols
    if (col == ".row_uid") next
    
    vec <- df[[col]]
    
    # Skip if already numeric, Date, or POSIX
    if (is.numeric(vec) || inherits(vec, "Date") || inherits(vec, "POSIXt")) {
      next
    }
    
    # Only check character
    if (is.character(vec)) {
      sample_vals <- head(na.omit(vec), 10)
      if (length(sample_vals) > 0) {
        for (fmt in fmt_candidates) {
          parsed <- suppressWarnings(
            lubridate::parse_date_time(sample_vals, orders = fmt, tz = "UTC")
          )
          
          # Require at least 5 values to parse correctly
          if (sum(!is.na(parsed)) >= 5) {
            parsed_full <- suppressWarnings(
              lubridate::parse_date_time(vec, orders = fmt, tz = "UTC")
            )
            
            # Excel numeric fallback
            if (any(is.na(parsed_full))) {
              nums <- suppressWarnings(as.numeric(vec))
              excel_dates <- as.POSIXct(as.Date(nums, origin = "1899-12-30"))
              parsed_full[is.na(parsed_full) & !is.na(excel_dates)] <- 
                excel_dates[is.na(parsed_full) & !is.na(excel_dates)]
            }
            
            df[[col]] <- parsed_full
            break
          }
        }
      }
    }
  }
  df
}
