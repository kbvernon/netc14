#' Simulate Chronology
#'
#' Simulate a probability density over time defined by a mixture of Gaussian,
#' uniform, or skewed (gamma) components.
#'
#' @param start integer scalar, the start date in years before present
#' @param end integer scalar, the end date in years before present
#' @param n_clusters integer scalar, the number of mixture components
#' @param distribution character scalar, the component family, one of
#'   `"gaussian"` (default), `"uniform"`, or `"skewed"`
#'
#' @return A numeric vector with class `Chronology` whose values are the
#'   mixture density evaluated over an annual grid from `start` to `end`. The
#'   start, end, distribution, and simulated mixture parameters are stored as
#'   attributes.
#'
#' @details The simulation attempts to randomly sample mixture locations from
#'   regularly spaced bins over the sequence from `start` to `end`. All
#'   component families are parameterized by their mean and standard deviation.
#'   Skewed components are shifted gamma distributions with shape 2, randomly
#'   reflected to skew left or right.
simulate_chronology <- function(
  start,
  end,
  n_clusters,
  distribution = c("gaussian", "uniform", "skewed")
) {
  distribution <- match.arg(distribution)
  years <- seq(start, end)

  # weights
  weights <- rgamma(n_clusters, shape = 1, rate = 1)
  weights <- weights / sum(weights)

  # means or locations
  year_chunks <- if (n_clusters == 1) {
    list(years)
  } else {
    split(years, cut(seq_along(years), n_clusters, labels = FALSE))
  }

  means <- vapply(
    year_chunks,
    \(.x) sample(.x, size = 1),
    numeric(1),
    USE.NAMES = FALSE
  )

  # errors or standard deviations
  scale <- (end - start) / 20
  errors <- rgamma(n_clusters, shape = 10, rate = (10 - 1) / scale)
  errors <- pmax(errors, round(scale))

  # component densities with mean and standard deviation
  component <- switch(
    distribution,
    gaussian = .gaussian,
    uniform = .uniform,
    skewed = .skewed
  )

  # component likelihoods: w * f(t|u,s)
  mixtures <- Map(
    \(x, w, u, s) w * component(x, u, s),
    w = weights,
    u = means,
    s = errors,
    MoreArgs = list(x = years)
  )
  mixtures <- do.call(rbind, mixtures)

  # density: p(t|w,u,s) = Σ w * f(t|u,s)
  v <- colSums(mixtures)
  # truncate
  v <- v / sum(v)

  # return
  structure(
    v,
    class = c("Chronology", class(v)),
    start = start,
    end = end,
    distribution = distribution,
    mixtures = data.frame(
      weights = weights,
      means = means,
      errors = errors
    )
  )
}

.gaussian <- function(x, mean, sd) {
  dnorm(x, mean, sd)
}

.uniform <- function(x, mean, sd) {
  start <- mean - sqrt(3) * sd
  end <- mean + sqrt(3) * sd
  dunif(x, start, end)
}

.skewed <- function(x, mean, sd) {
  d <- sample(c(-1, 1), size = 1)
  k <- 2
  dgamma(d * (x - mean) + sqrt(k) * sd, shape = k, scale = sd / sqrt(k))
}

#' Print Chronology
#'
#' @param x a numeric vector with class `Chronology`
#' @param ... additional arguments (unused)
print.Chronology <- function(x, ...) {
  parameters <- attr(x, "mixtures")

  i <- order(parameters[["weights"]], decreasing = TRUE)
  parameters <- parameters[i, ]
  rownames(parameters) <- NULL

  template <- paste(
    "Chronology -----",
    "Years: %d-%d",
    "Distribution: %s",
    "N Mixtures: %d\n",
    "Parameters\n",
    sep = "\n"
  )

  txt <- sprintf(
    template,
    attr(x, "start"),
    attr(x, "end"),
    attr(x, "distribution"),
    nrow(parameters)
  )

  cat(txt)
  nr <- min(5, nrow(parameters))
  print.data.frame(parameters[1:nr, , drop = FALSE], digits = 5)
  if (nrow(parameters) > nr) {
    cat(sprintf(" ... [omitted %d rows]\n", nrow(parameters) - nr))
  }
}

#' @rdname density
#' @details For Chronology, returns the true density over the full annual
#'   grid; `years` is ignored.
density.Chronology <- function(x, years, ...) {
  # get years sequence
  years <- seq(attr(x, "start"), attr(x, "end"))

  # strip class and attributes from vector
  v <- unclass(x)
  attributes(v) <- NULL

  # return
  D <- data.frame(
    model = "Chronology",
    cal_age = years,
    density = v,
    conf_lo = NA,
    conf_hi = NA
  )

  structure(D, class = c("Density", "data.frame"))
}

#' @rdname predict
#' @details For Chronology, the oracle prediction: weights each date's
#'   calibrated posterior by the true density.
predict.Chronology <- function(object, calibrated_dates, n_draws = 1000, ...) {
  years <- seq(attr(object, "start"), attr(object, "end"))

  v <- unclass(object)
  q <- t(t(calibrated_dates) * v)
  q <- q / rowSums(q)

  # inverse transform sampling
  ages <- t(apply(
    q,
    MARGIN = 1,
    FUN = inv_transform_sample,
    years = years,
    n = n_draws
  ))

  structure(ages, class = c("Ages", class(ages)))
}
