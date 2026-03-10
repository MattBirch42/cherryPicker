#' Convert Input Data to a Standardized DuckDB Table
#'
#' @description
#' Converts R objects such as `data.frame`, `tibble`, `matrix`, `ts`, `xts`,
#' or `zoo` into a standardized `data.frame` and loads them into a temporary
#' DuckDB table named `"data"`. If a DuckDB connection is not provided, one is
#' created in memory (`:memory:`). Optionally detects and converts date, time,
#' and character columns using helper functions if available.
#'
#' @param input_data An object containing tabular or time series data. Supported
#'   classes include `data.frame`, `tibble`, `matrix`, `ts`, `xts`, and `zoo`.
#' @param con.app Optional DuckDB connection. If `NULL` or invalid, a new
#'   in-memory connection will be established.
#' @param auto_detect_types Logical. If `TRUE` (default), the function will
#'   attempt to detect and convert timestamp, date, and character columns using
#'   `detect_and_convert_timestamps()`, `detect_and_convert_dates()`, and
#'   `detect_and_convert_characters()` if they exist.
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
#' - Adds an `.id` column if not already present.
#' - Replaces any existing `"data"` table quietly.
#' - Optionally auto-detects and converts timestamps, dates, and characters.
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
convert_to_tbl <- function(
  input_data,
  con.app = NULL,
  auto_detect_timestamps = TRUE,
  auto_detect_dates = TRUE,
  auto_detect_characters = TRUE
) {
  # ---- Dependency checks ----------------------------------------------------
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop("Package 'duckdb' is required. Please install it.")
  }
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package 'DBI' is required. Please install it.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Please install it.")
  }

  # ---- Ensure / create DuckDB connection -----------------------------------
  if (is.null(con.app) || !DBI::dbIsValid(con.app)) {
    con.app <- DBI::dbConnect(
      duckdb::duckdb(),
      dbdir = ":memory:",
      read_only = FALSE
    )
  }

  # ---- Normalize input -----------------------------------------------------
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
    stop(
      "Unsupported input type: please provide a data.frame, tibble, matrix, ts, xts, or zoo object."
    )
  }

  # ---- Final safety coercion ----------------------------------------------
  df <- tryCatch(
    base::as.data.frame(df, stringsAsFactors = FALSE),
    error = function(e) {
      stop(
        "Internal conversion failed: could not coerce input to a base data.frame. ",
        e$message
      )
    }
  )

  # ---- Validate ------------------------------------------------------------
  if (!is.data.frame(df) || is.null(nrow(df)) || is.na(nrow(df))) {
    stop("Conversion failed: input object does not behave like a data.frame.")
  }

  # ---- Clean column names --------------------------------------------------
  names(df) <- make.names(names(df), unique = TRUE)

  # ---- Add ID column safely ------------------------------------------------
  if (!".id" %in% names(df)) {
    df$.id <- seq_len(nrow(df))
  }

  # ---- Auto type detection -------------------------------------------------
  if (isTRUE(auto_detect_timestamps)) {
    # Timestamp detection
    if (exists("detect_and_convert_timestamps", mode = "function")) {
      before <- names(df)
      df_new <- detect_and_convert_timestamps(df)
      changed <- setdiff(
        names(df_new)[!sapply(df_new, inherits, what = class(df[[1]]))],
        before
      )
      if (length(changed) > 0) {
        message(
          "Detected and converted ",
          length(changed),
          " timestamp columns:\n  - ",
          paste(changed, collapse = "\n  - ")
        )
      }
      df <- df_new
    }
  }

  if (isTRUE(auto_detect_dates)) {
    # Date detection
    if (exists("detect_and_convert_dates", mode = "function")) {
      before_classes <- sapply(df, class)
      df_new <- detect_and_convert_dates(df)
      converted <- names(which(
        sapply(df_new, inherits, "Date") & !sapply(df, inherits, "Date")
      ))
      if (length(converted) > 0) {
        message(
          "Detected and converted ",
          length(converted),
          " date columns:\n  - ",
          paste(converted, collapse = "\n  - ")
        )
      }
      df <- df_new
    }
  }

  if (isTRUE(auto_detect_characters)) {
    # Character detection
    if (exists("detect_and_convert_characters", mode = "function")) {
      before_classes <- sapply(df, class)
      df_new <- detect_and_convert_characters(df)
      converted <- names(which(
        sapply(df_new, inherits, "character") &
          !sapply(df, inherits, "character")
      ))
      if (length(converted) > 0) {
        message(
          "Detected and converted ",
          length(converted),
          " character columns:\n  - ",
          paste(converted, collapse = "\n  - ")
        )
      }
      df <- df_new
    }
  }

  # ---- Write into DuckDB ---------------------------------------------------
  DBI::dbWriteTable(con.app, "data", df, overwrite = TRUE)

  # ---- Return connection and table reference ------------------------------
  tbl_ref <- dplyr::tbl(con.app, "data")

  message(
    "Data successfully converted and loaded into temporary DuckDB table 'data'."
  )
  list(con.app = con.app, tbl = tbl_ref)
}
