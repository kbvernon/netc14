#' Get MCMC Draws
#'
#' Extract label-invariant MCMC draws of one parameter from a fitted model for
#' convergence diagnostics, including burn-in.
#'
#' @param x a fitted model object
#' @param parameter character, `"density"` (the normalized mixture density at
#'   each calendar year) or, for DPMM only, `"alpha"` (the concentration
#'   parameter) and `"ages"` (the calendar age of each sample)
#' @param ... additional arguments passed to methods
#'
#' @return A matrix with one row per (thinned) iteration.
get_draws <- function(x, parameter, ...) UseMethod("get_draws")

#' @rdname get_draws
#' @param years numeric vector of calendar years at which to evaluate the
#'   mixture density
get_draws.BGMM <- function(x, parameter, years, ...) {
  switch(parameter, density = density_draws(x, years))
}

#' @rdname get_draws
#' @details For DPMM, densities are those stored by
#'   `carbondate::PolyaUrnBivarDirichlet` on its own 100-year grid, found in
#'   `x$density_data$calendar_ages`.
get_draws.DPMM <- function(x, parameter, ...) {
  D <- x[["density_data"]][["densities"]]

  switch(
    parameter,
    alpha = matrix(x[["alpha"]]),
    density = D / rowSums(D),
    ages = x[["calendar_ages"]]
  )
}

#' Potential Scale Reduction Factor
#'
#' Compute rank-normalized split R-hat (Vehtari et al 2021) for one parameter,
#' discarding the first half of every chain as burn-in and splitting the
#' remainder in two. Returns the maximum of bulk R-hat (on rank-normalized
#' draws) and tail R-hat (on rank-normalized draws folded about the median).
#'
#' @param x an `n_iter x n_chains` matrix of draws
#'
#' @return A numeric scalar.
rhat <- function(x) {
  n <- nrow(x) %/% 4
  x <- matrix(x[2 * n + seq_len(2 * n), ], nrow = n)
  max(split_rhat(x), split_rhat(abs(x - median(x))))
}

split_rhat <- function(x) {
  x[] <- qnorm((rank(x) - 3 / 8) / (length(x) + 1 / 4))
  n <- nrow(x)
  W <- mean(apply(x, 2, var))
  B <- n * var(colMeans(x))
  sqrt(((n - 1) / n * W + B / n) / W)
}

#' Kullback-Leibler Trace
#'
#' Compute the KL divergence of each iteration's density from the mean density
#' over the initial iterations, following `carbondate::PlotConvergenceData`.
#'
#' @param density an `n_iter x T` matrix of normalized densities, as returned
#'   by `get_draws()`
#' @param n_initial integer, number of initial iterations used as the
#'   reference density (default is 10% of iterations)
#'
#' @return A data.frame with columns `iter` and `kld`.
kld_trace <- function(density, n_initial = nrow(density) %/% 10) {
  i <- seq_len(n_initial)
  P <- colMeans(density[i, , drop = FALSE])
  Q <- t(density[-i, , drop = FALSE])

  data.frame(
    iter = seq(n_initial + 1, nrow(density)),
    kld = colSums(P * log((P + EPS) / (Q + EPS)))
  )
}
