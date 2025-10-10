#' Detect Outlier Cutoffs by Z-Score
#'
#' @description
#' Computes z-scores for a numeric vector and identifies the data values that
#' define the first *low* and *high* outlier boundaries according to the given
#' threshold. These cutoff values can be used directly for flagging:
#' \eqn{x <= low_cutoff} or \eqn{x >= high_cutoff}.
#'
#' @param x A numeric vector.
#' @param threshold Numeric. Z-score threshold for outlier detection.
#'   Default is `3`, corresponding to roughly 99.7% of a normal distribution.
#'
#' @return
#' A one-row tibble with:
#' \describe{
#'   \item{var_name}{The variable name as a character string.}
#'   \item{low_cutoff}{The *highest* value among low-side outliers
#'     (\eqn{z <= -threshold}).}
#'   \item{high_cutoff}{The *lowest* value among high-side outliers
#'     (\eqn{z >= threshold}).}
#' }
#' If no outliers exist beyond the given threshold, the corresponding cutoff
#' value is `NA`.
#'
#' @examples
#' set.seed(42)
#' x <- c(rnorm(100, 50, 10), 150, -25)
#' detect_outliers_zscore(x, threshold = 2.5)
#'
#' # Example use:
#' res <- detect_outliers_zscore(x, threshold = 3)
#' x_flagged <- ifelse(x <= res$low_cutoff | x >= res$high_cutoff, TRUE, FALSE)
#'
#' @importFrom dplyr tibble
#' @export
detect_outliers_zscore <- function(x, threshold = 3) {
  if (!is.numeric(x)) stop("Input 'x' must be numeric.")
  if (!is.numeric(threshold) || length(threshold) != 1 || threshold <= 0) {
    stop("Threshold must be a positive numeric scalar.")
  }
  
  mu <- mean(x, na.rm = TRUE)
  sigma <- sd(x, na.rm = TRUE)
  if (is.na(sigma) || sigma == 0) {
    warning("Standard deviation is zero or NA; returning NA cutoffs.")
    return(dplyr::tibble(var_name = deparse(substitute(x)), low_cutoff = NA_real_, high_cutoff = NA_real_))
  }
  
  z_scores <- (x - mu) / sigma
  
  low_candidates  <- x[z_scores <= -abs(threshold)]
  high_candidates <- x[z_scores >=  abs(threshold)]
  
  low_value  <- if (length(low_candidates))  max(low_candidates, na.rm = TRUE) else NA_real_
  high_value <- if (length(high_candidates)) min(high_candidates, na.rm = TRUE) else NA_real_
  
  dplyr::tibble(
    var_name = deparse(substitute(x)),
    low_cutoff = low_value,
    high_cutoff = high_value
  )
}
