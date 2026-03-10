#' Aggregate Data Dynamically in DuckDB
#'
#' @description
#' Executes an aggregation query inside DuckDB using user-specified grouping
#' variables, target variables, and aggregation functions. Optionally excludes
#' rows by `.id` (or `.row_uid`) before aggregation.
#'
#' @param con.app A valid DuckDB connection (created via `convert_to_tbl()`).
#' @param exclude_set Optional vector of `.id` (or `.row_uid`) values to exclude
#'   from computation. Default is `NULL`, meaning no rows are excluded.
#' @param group_list Character vector of variable names to group by.
#' @param target_list Character vector of variable names to aggregate.
#' @param aggregator_list Character vector of aggregation functions to apply to
#'   each variable in `target_list`. Must be the same length as `target_list`.
#'
#' @return
#' A list with two elements:
#' \describe{
#'   \item{query}{The SQL aggregation query executed in DuckDB.}
#'   \item{tbl}{A `dplyr::tbl_dbi` reference to the new `data_agg` table.}
#' }
#'
#' @details
#' The function runs fully within DuckDB. If `exclude_set` is non-empty, rows
#' matching `.id` in `exclude_set` are filtered out. Each aggregator is applied
#' one-to-one with its corresponding target variable. The resulting column
#' names are formed as `"<target>_<aggregator>"`.
#'
#' Supported aggregators include: `mean`, `count`, `median`, `min`, `max`, `sum`, and `sddev`.
#'
#' @examples
#' \dontrun{
#' res <- convert_to_tbl(mtcars)
# aggregate_duckdb(
#   con.app = res$con.app,
#   exclude_set = NULL,
#   group_list = c("cyl"),
#   target_list = c("mpg", "hp"),
#   aggregator_list = c("mean", "max")
# )
#' }
#'
#' @importFrom DBI dbExecute dbExistsTable
#' @importFrom dplyr tbl
#' @export
aggregate_duckdb <- function(
  con.app,
  exclude_set = NULL,
  group_list = NULL,
  target_list = NULL,
  aggregator_list = NULL
) {
  # ---- Validate input ------------------------------------------------------
  if (is.null(con.app) || !DBI::dbIsValid(con.app)) {
    stop("Please provide a valid DuckDB connection (con.app).")
  }

  if (is.null(target_list) || is.null(aggregator_list)) {
    stop("Both target_list and aggregator_list must be provided.")
  }

  if (length(target_list) != length(aggregator_list)) {
    stop("target_list and aggregator_list must be the same length.")
  }

  # ---- Validate aggregators ------------------------------------------------
  valid_aggs <- c("mean", "min", "max", "sum", "stddev", "count")
  bad_aggs <- setdiff(aggregator_list, valid_aggs)
  if (length(bad_aggs) > 0) {
    stop(
      "Unsupported aggregators: ",
      paste(bad_aggs, collapse = ", "),
      ". Valid options are: ",
      paste(valid_aggs, collapse = ", "),
      "."
    )
  }

  # ---- Construct WHERE clause ----------------------------------------------
  where_clause <- ""
  if (!is.null(exclude_set) && length(exclude_set) > 0) {
    id_values <- paste0(exclude_set, collapse = ", ")
    where_clause <- paste0("WHERE .id NOT IN (", id_values, ")")
  }

  # ---- Construct GROUP BY clause -------------------------------------------
  group_clause <- ""
  if (!is.null(group_list) && length(group_list) > 0) {
    group_clause <- paste("GROUP BY", paste(group_list, collapse = ", "))
  }

  # ---- Build SELECT clause -------------------------------------------------
  group_select <- if (!is.null(group_list) && length(group_list) > 0) {
    paste(group_list, collapse = ", ")
  } else {
    ""
  }

  agg_selects <- vapply(
    seq_along(target_list),
    function(i) {
      target <- target_list[i]
      agg_fun <- aggregator_list[i]
      paste0(agg_fun, "(", target, ") AS ", target, "_", agg_fun)
    },
    FUN.VALUE = character(1)
  )

  select_clause <- paste(c(group_select, agg_selects), collapse = ", ")

  # ---- Final SQL query -----------------------------------------------------
  query <- sprintf(
    "
    CREATE OR REPLACE TABLE data_agg AS
    SELECT %s
    FROM data
    %s
    %s
    ",
    select_clause,
    where_clause,
    group_clause
  )

  # ---- Execute in DuckDB ---------------------------------------------------
  DBI::dbExecute(con.app, query)

  # ---- Return reference ----------------------------------------------------
  tbl_ref <- dplyr::tbl(con.app, "data_agg")

  message("Aggregation completed and saved to DuckDB table 'data_agg'.")
  list(query = query, tbl = tbl_ref)
}

# a = convert_to_tbl(fake_data)
# aggregate_duckdb(con.app = a$con.app,
#                  exclude_set = NULL,
#                  group_list = c("year","weekday"),
#                  target_list = c("temp_avg", "smile","noise"),
#                  aggregator_list = c("stddev","mean","sum"))
