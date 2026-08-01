#' Get Mixture Parameters
#'
#' Extract mixture parameters from a fitted model.
#'
#' @param x a fitted model object
#' @param ... additional arguments passed to methods
#'
#' @return A data.frame with columns `weights`, `means`, and `errors`, sorted
#'   by weight (descending). Returns `NULL` for non-mixture models.
get_parameters <- function(x, ...) UseMethod("get_parameters")
get_parameters.default <- function(x, ...) NULL

#' @rdname get_parameters
get_parameters.NetC14 <- function(x, ...) {
  x$eval()
  gmm <- x$mixture()

  parameters <- data.frame(
    weights = as_vector(gmm$.mixture_distribution$probs),
    means = as_vector(gmm$.component_distribution$mean),
    errors = as_vector(gmm$.component_distribution$stddev)
  )

  structure(sort_mixtures(parameters), class = c("Parameters", "data.frame"))
}

#' @rdname get_parameters
get_parameters.Chronology <- function(x, ...) {
  parameters <- attr(x, "mixtures")
  structure(sort_mixtures(parameters), class = c("Parameters", "data.frame"))
}

#' @rdname get_parameters
#' @param n_burn integer, number of initial MCMC iterations to discard as
#'   burn-in (BGMM only). If `NULL` (default), uses half of `n_iter`.
get_parameters.BGMM <- function(x, n_burn = NULL, ...) {
  n_iter <- attr(x, "n_iter")
  if (is.null(n_burn)) {
    n_burn <- n_iter %/% 2
  }

  i <- (n_burn + 1):n_iter
  parameters <- data.frame(
    weights = colMeans(x[["weights"]][i, , drop = FALSE]),
    means = colMeans(x[["means"]][i, , drop = FALSE]),
    errors = colMeans(x[["errors"]][i, , drop = FALSE])
  )

  structure(sort_mixtures(parameters), class = c("Parameters", "data.frame"))
}

#' @rdname get_parameters
get_parameters.DPMM <- function(x, ...) {
  n_out <- length(x[["phi"]])
  n_obs <- x[["observations_per_cluster"]][[n_out]]

  parameters <- data.frame(
    weights = n_obs / sum(n_obs),
    means = x[["phi"]][[n_out]],
    errors = 1 / sqrt(x[["tau"]][[n_out]])
  )

  structure(sort_mixtures(parameters), class = c("Parameters", "data.frame"))
}

sort_mixtures <- function(parameters) {
  i <- order(parameters[["weights"]], decreasing = TRUE)
  parameters <- parameters[i, ]
  rownames(parameters) <- NULL
  parameters
}

#' Count Effective Mixture Components
#'
#' @param x a fitted model object
#' @param ... additional arguments passed to methods
#'
#' @return An integer count of effective mixture components, or `NA` for
#'   non-mixture models.
count_components <- function(x, ...) UseMethod("count_components")
count_components.default <- function(x, ...) NA_integer_

#' @rdname count_components
count_components.NetC14 <- function(x, ...) {
  p <- get_parameters(x)
  sum(p[["weights"]] > 0)
}

#' @rdname count_components
count_components.BGMM <- function(x, ...) {
  nrow(get_parameters(x))
}

#' @rdname count_components
count_components.DPMM <- function(x, ...) {
  n_out <- length(x[["n_clust"]])
  x[["n_clust"]][[n_out]]
}

#' Print Parameters
#'
#' @param x a data.frame with class `Parameters`
#' @param ... additional arguments (unused)
print.Parameters <- function(x, ...) {
  cat("\nParameters\n")
  nr <- min(5, nrow(x))
  print.data.frame(x[seq_len(nr), ], digits = 5)
  if (nrow(x) > nr) cat(sprintf(" ... [omitted %d rows]\n", nrow(x) - nr))
}

# K x T matrix of densities for K components over T years
component_densities <- function(means, errors, years) {
  Z <- outer(means, years, `-`) / errors
  exp(-0.5 * Z * Z) / (errors * sqrt(2 * pi))
}
