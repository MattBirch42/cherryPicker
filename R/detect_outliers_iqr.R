#' Detect Outlier Cutoffs by IQR Rule
#'
#' @description
#' Identifies outlier cutoff values for a numeric vector using the classic
#' **1.5 × IQR rule**. Returns the *first low* and *first high* data values that
#' would be considered outliers under this rule.
#' These cutoffs can be directly used for flagging with:
#' \eqn{x <= low_cutoff} or \eqn{x >= high_cutoff}.
#'
#' @param x A numeric vector.
#' @param multiplier Numeric. The multiplier applied to the interquartile range
#'   (IQR). Default is `1.5` (Tukey’s rule), but larger values (e.g., 3) yield
#'   more conservative thresholds.
#'
#' @return
#' A one-row tibble with:
#' \describe{
#'   \item{var_name}{The variable name as a character string.}
#'   \item{low_cutoff}{The highest value among the low-side outliers
#'     (\eqn{x < Q1 - multiplier × IQR}).}
#'   \item{high_cutoff}{The lowest value among the high-side outliers
#'     (\eqn{x > Q3 + multiplier × IQR}).}
#' }
#' If no outliers are found, the corresponding cutoff value is `NA`.
#'
#' @examples
#' set.seed(123)
#' x <- c(rnorm(100, mean = 50, sd = 10), 5, 120)
#' detect_outliers_iqr(x)
#'
#' # Example use:
#' res <- detect_outliers_iqr(x)
#' x_flagged <- ifelse(x <= res$low_cutoff | x >= res$high_cutoff, TRUE, FALSE)
#'
#' @importFrom dplyr tibble
#' @export
detect_outliers_iqr <- function(x, multiplier = 1.5) {
  # ---- Validate input ------------------------------------------------------
  if (!is.numeric(x)) {
    stop("Input 'x' must be numeric.")
  }
  if (!is.numeric(multiplier) || length(multiplier) != 1 || multiplier <= 0) {
    stop("Multiplier must be a positive numeric scalar.")
  }

  # ---- Compute quartiles and IQR -------------------------------------------
  q1 <- stats::quantile(x, 0.25, na.rm = TRUE, names = FALSE)
  q3 <- stats::quantile(x, 0.75, na.rm = TRUE, names = FALSE)
  iqr_val <- q3 - q1

  if (is.na(iqr_val) || iqr_val == 0) {
    warning("IQR is zero or NA; returning NA cutoffs.")
    return(dplyr::tibble(
      var_name = deparse(substitute(x)),
      low_cutoff = NA_real_,
      high_cutoff = NA_real_
    ))
  }

  lower_limit <- q1 - multiplier * iqr_val
  upper_limit <- q3 + multiplier * iqr_val

  # ---- Identify actual cutoff values ---------------------------------------
  low_candidates <- x[x < lower_limit]
  high_candidates <- x[x > upper_limit]

  low_value <- if (length(low_candidates)) {
    max(low_candidates, na.rm = TRUE)
  } else {
    NA_real_
  }
  high_value <- if (length(high_candidates)) {
    min(high_candidates, na.rm = TRUE)
  } else {
    NA_real_
  }

  # ---- Return single-row tibble --------------------------------------------
  dplyr::tibble(
    var_name = deparse(substitute(x)),
    low_cutoff = low_value,
    high_cutoff = high_value
  )
}
