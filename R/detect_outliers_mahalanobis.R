#' Detect Multivariate Outliers Using Mahalanobis Distance
#'
#' @description
#' Computes Mahalanobis distances for all numeric columns in a data frame (or matrix)
#' and identifies potential multivariate outliers. The function returns the
#' Mahalanobis cutoff distance (based on the chosen chi-square quantile) and
#' a tibble summarizing the threshold used for detection.
#'
#' @param data A data frame or matrix containing **only numeric columns**.
#'   Non-numeric columns will be dropped automatically with a warning.
#' @param p_value Numeric. The significance level used to determine the cutoff
#'   quantile from the chi-square distribution. Default is `0.001`, which
#'   corresponds to the 99.9th percentile.
#'
#' @return
#' A list with two elements:
#' \describe{
#'   \item{summary}{A one-row tibble with columns:
#'     \code{n_vars}, \code{p_value}, and \code{cutoff_distance}.}
#'   \item{distances}{A tibble with columns:
#'     \code{.id}, \code{mahalanobis_distance}, and \code{is_outlier}.}
#' }
#'
#' @details
#' Mahalanobis distance measures how far each observation is from the
#' multivariate mean, accounting for covariance between variables. Observations
#' with distances greater than the chi-square quantile for `df = ncol(data)`
#' are flagged as outliers.
#'
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   x = rnorm(100, 0, 1),
#'   y = rnorm(100, 0, 1)
#' )
#' df[5, ] <- c(5, 5) # add an outlier
#'
#' res <- detect_outliers_mahalanobis(df)
#' head(res$distances)
#'
#' @importFrom stats cov mahalanobis qchisq
#' @importFrom dplyr tibble
#' @export
detect_outliers_mahalanobis <- function(data, p_value = 0.001) {
  # ---- Validate input ------------------------------------------------------
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("Input must be a data frame or matrix.")
  }
  if (
    !is.numeric(p_value) || length(p_value) != 1 || p_value <= 0 || p_value >= 1
  ) {
    stop("p_value must be between 0 and 1.")
  }

  # ---- Select numeric columns ----------------------------------------------
  num_data <- data[, sapply(data, is.numeric), drop = FALSE]
  if (ncol(num_data) == 0) {
    stop("No numeric columns found in 'data'.")
  }
  if (ncol(num_data) < ncol(data)) {
    warning(
      "Non-numeric columns were dropped before computing Mahalanobis distances."
    )
  }

  # ---- Compute Mahalanobis distances ---------------------------------------
  center <- colMeans(num_data, na.rm = TRUE)
  cov_matrix <- stats::cov(num_data, use = "pairwise.complete.obs")

  # Ensure covariance matrix is invertible
  if (det(cov_matrix) == 0) {
    warning("Covariance matrix is singular; results may be unreliable.")
  }

  d2 <- stats::mahalanobis(num_data, center, cov_matrix)
  cutoff <- stats::qchisq(1 - p_value, df = ncol(num_data))
  is_outlier <- d2 > cutoff

  # ---- Return structured output --------------------------------------------
  list(
    summary = dplyr::tibble(
      n_vars = ncol(num_data),
      p_value = p_value,
      cutoff_distance = cutoff
    ),
    distances = dplyr::tibble(
      .id = seq_along(d2),
      mahalanobis_distance = d2,
      is_outlier = is_outlier
    )
  )
}
