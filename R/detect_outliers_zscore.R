#' Detect Outlier Cutoffs by Z-Score
#'
#' @description
#' Given a numeric variable and a z-score threshold, this function computes
#' standardized scores (z-scores) and returns the *data values* that correspond
#' to the low and high cutoffs defined by `-threshold` and `+threshold`.
#' In short, it identifies the first low point and the first high point that can
#' be considered outliers, based on the input threshold as a cutoff
#'
#' @param x A numeric vector.
#' @param threshold Numeric. Z-score threshold for outlier detection.
#'   Default is `3`, corresponding to roughly 99.7% of a normal distribution.
#'
#' @return
#' A one-row tibble with three columns:
#' \describe{
#'   \item{var_name}{Name of the variable as a character string.}
#'   \item{low}{Data value corresponding to `z <= -threshold`.}
#'   \item{high}{Data value corresponding to `z >= threshold`.}
#' }
#' If no outliers are found beyond the specified threshold, the corresponding
#' `low` or `high` value is `NA`.
#'
#' @examples
#' x <- rnorm(1000, mean = 50, sd = 10)
#' detect_outliers_zscore(x, threshold = 2.5)
#'
#' @importFrom dplyr tibble
#' @export
detect_outliers_zscore <- function(x, threshold = 3) {
  # ---- Validate input ------------------------------------------------------
  if (!is.numeric(x)) stop("Input 'x' must be numeric.")
  if (!is.numeric(threshold) || length(threshold) != 1 || threshold <= 0) {
    stop("Threshold must be a positive numeric scalar.")
  }
  
  # ---- Compute z-scores ----------------------------------------------------
  mu <- mean(x, na.rm = TRUE)
  sigma <- sd(x, na.rm = TRUE)
  if (is.na(sigma) || sigma == 0) {
    warning("Standard deviation is zero or NA; returning NA cutoffs.")
    return(dplyr::tibble(var_name = deparse(substitute(x)), low = NA_real_, high = NA_real_))
  }
  
  z_scores <- (x - mu) / sigma
  
  # ---- Identify cutoff values ----------------------------------------------
  low_candidates  <- x[z_scores <= -abs(threshold)]
  high_candidates <- x[z_scores >=  abs(threshold)]
  
  low_value  <- if (length(low_candidates))  max(low_candidates, na.rm = TRUE) else NA_real_
  high_value <- if (length(high_candidates)) min(high_candidates, na.rm = TRUE) else NA_real_
  
  # ---- Return single-row tibble --------------------------------------------
  dplyr::tibble(
    var_name = deparse(substitute(x)),
    low_cutoff = low_value,
    high_cutoff = high_value
  )
}
