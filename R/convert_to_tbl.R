#' Convert Input Data to a Standardized DuckDB Table
#'
#' @description
#' Converts R objects such as `data.frame`, `tibble`, `matrix`, `ts`, `xts`,
#' or `zoo` into a standardized `data.frame` and loads them into a temporary
#' DuckDB table named `"data"`. If a DuckDB connection is not provided, one is
#' created in memory (`:memory:`). This ensures consistent internal structure
#' for subsequent processing within the Cherry Picker app.
#'
#' @param input_data An object containing tabular or time series data. Supported
#'   classes include `data.frame`, `tibble`, `matrix`, `ts`, `xts`, and `zoo`.
#' @param con.app Optional DuckDB connection. If `NULL` or invalid, a new
#'   in-memory connection will be established.
#'
#' @return
#' A list with two elements:
#' \describe{
#'   \item{`con.app`}{The active DuckDB connection.}
#'   \item{`tbl`}{A `dplyr::tbl_dbi` reference to the `"data"` table.}
#' }
#'
#' @details
#' - Ensures valid, unique column names.
#' - Adds an `.row_uid` column if not already present.
#' - Replaces any existing `"data"` table quietly.
#'
#' @examples
#' \dontrun{
#' res <- convert_to_tbl(mtcars)
#' res$tbl %>% dplyr::glimpse()
#' }
#'
#' @importFrom DBI dbConnect dbIsValid dbWriteTable
#' @importFrom duckdb duckdb
#' @importFrom dplyr tbl
#' @export
convert_to_tbl <- function(input_data, con.app = NULL) {
  
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop("Package 'duckdb' is required. Please install it.")
  }
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package 'DBI' is required. Please install it.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Please install it.")
  }
  
  if (is.null(con.app) || !DBI::dbIsValid(con.app)) {
    con.app <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:", read_only = FALSE)
  }
  
  if (inherits(input_data, "data.frame") || inherits(input_data, "tbl_df")) {
    df <- base::as.data.frame(input_data)
  } else if (inherits(input_data, "matrix")) {
    df <- base::as.data.frame(input_data)
  } else if (inherits(input_data, "ts")) {
    df <- data.frame(
      time = as.numeric(stats::time(input_data)),
      value = as.numeric(input_data)
    )
  } else if (inherits(input_data, "xts") || inherits(input_data, "zoo")) {
    df <- data.frame(
      time = as.POSIXct(time(input_data)),
      coredata(input_data)
    )
  } else {
    stop("Unsupported input type: please provide a data.frame, tibble, matrix, ts, xts, or zoo object.")
  }
  
  df <- tryCatch(
    {
      base::as.data.frame(df, stringsAsFactors = FALSE)
    },
    error = function(e) {
      stop("Internal conversion failed: could not coerce input to a base data.frame. ", e$message)
    }
  )
  
  if (!is.data.frame(df) || is.null(nrow(df)) || is.na(nrow(df))) {
    stop("Conversion failed: input object does not behave like a data.frame.")
  }
  
  names(df) <- make.names(names(df), unique = TRUE)
  
  if (!".row_uid" %in% names(df)) {
    df$.row_uid <- seq_len(nrow(df))
  }
  
  DBI::dbWriteTable(con.app, "data", df, overwrite = TRUE)
  
  tbl_ref <- dplyr::tbl(con.app, "data")
  
  message("Data successfully converted and loaded into temporary DuckDB table 'data'.")
  list(con.app = con.app, tbl = tbl_ref)
}
