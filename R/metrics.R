#' Evaluation Metrics
#'
#' For summary metrics, compare an estimated distribution against a target or
#' true distribution. For calibration metrics, pointwise error between an
#' estimated age and a target or true age.
#'
#' @param estimate numeric vector of estimates
#' @param target numeric vector of targets
#'
#' @return A non-negative numeric scalar.
#'
#' @details
#' Summary statistics (error in distribution estimates)
#' - `tvd`: total variation distance, half the L1 distance between `estimate`
#'   and `target`.
#' - `kld`: Kullback-Leibler divergence `D(target || estimate)`, the expected
#'   log difference taken with respect to `target`.
#' - `wd`: Wasserstein distance, the L1 distance between the CDFs of
#'   `estimate` and `target`.
#'
#' Calibration statistics (error in age estimates)
#' - `mae`: mean absolute error, the mean of `|estimate - target|`.
#' - `mse`: mean squared error, the mean of `(estimate - target)^2`.
#'
#' @name metrics
tvd <- function(estimate, target) {
  0.5 * sum(abs(target - estimate))
}

#' @rdname metrics
kld <- function(estimate, target) {
  sum(target * (log(target + EPS) - log(estimate + EPS)))
}

#' @rdname metrics
wd <- function(estimate, target) {
  sum(abs(cumsum(target) - cumsum(estimate)))
}

#' @rdname metrics
mae <- function(estimate, target) {
  mean(abs(estimate - target))
}

#' @rdname metrics
mse <- function(estimate, target) {
  mean((estimate - target)^2)
}
