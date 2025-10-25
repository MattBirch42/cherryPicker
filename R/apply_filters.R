#' Apply Filters to Data (global NA removal logic)
#'
#' Applies user-selected filters and drops entire rows
#' containing NA values in any columns where "Exclude NA" is checked.
#'
#' @param df Data frame.
#' @param input Shiny input object.
#' @return Filtered data frame.
#' @keywords internal
apply_filters <- function(df, input) {
  if (is.null(df) || nrow(df) == 0) return(df)
  
  keep <- rep(TRUE, nrow(df))
  
  # 1. Apply value/range filters first
  for (col in names(df)) {
    if (col == ".row_uid") next
    v  <- df[[col]]
    id <- paste0("filter_", col)
    
    if (!is.null(input[[id]])) {
      if (inherits(v, "Date")) {
        dr <- input[[id]]
        if (length(dr) == 2 && all(!is.na(dr))) {
          keep <- keep & (v >= dr[1] & v <= dr[2])
        }
      } else if (is.numeric(v)) {
        rng <- input[[id]]
        if (length(rng) == 2 && all(is.finite(rng))) {
          keep <- keep & (v >= rng[1] & v <= rng[2])
        }
      } else {
        vals <- input[[id]]
        if (!is.null(vals)) {
          keep <- keep & (v %in% vals | is.na(v))
        }
      }
    }
  }
  
  # 2. Identify which columns have "Exclude NA" toggled
  exclude_cols <- names(df)[
    vapply(names(df), function(col) {
      id <- paste0("exclude_na_", col)
      isTRUE(input[[id]])
    }, logical(1))
  ]
  
  print(paste0("exclude_cols = ",exclude_cols))
  
  # 3. Drop any rows with NA in *any* of those columns
  if (length(exclude_cols) > 0) {
    df <- df %>%
      dplyr::filter(
        dplyr::if_all(dplyr::all_of(exclude_cols), ~ !is.na(.))
      )
    keep <- rep(TRUE, nrow(df))
  }
  
  df[keep, , drop = FALSE]
}
