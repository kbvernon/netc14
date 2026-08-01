#' Sample Radiocarbon Dates
#'
#' Sample radiocarbon dates from a `Chronology`.
#'
#' @param chronology an object of class `Chronology`
#' @param n_samples integer scalar, the number of samples to draw
#' @param calibration a data.frame with class `Calibration`
#'
#' @return A data.frame with class `C14Samples` containing three columns:
#'   `cal_age`, `f14c_mu`, and `f14c_sigma`. Start and end dates are stored as
#'   attributes.
#'
#' @details This function implements the generative model described in Heaton
#'   (2021) and Price et al (2021). The function uses inverse transform sampling
#'   to sample true calendar dates or ages (`cal_age`) from the evaluated
#'   `Chronology` mixture density. Observed C14 measurements `f14c_mu` and
#'   measurement errors `f14c_sigma` are then sampled from the `calibration`
#'   curve at the true calendar ages.
sample_dates <- function(chronology, n_samples, calibration) {
  years <- seq(
    attr(chronology, "start"),
    attr(chronology, "end")
  )

  # sample tᵢ ~ p(t)
  cal_ages <- inv_transform_sample(chronology, years, n_samples)

  # sample c(tᵢ)|tᵢ ~ N(c(tᵢ), zᵢ²)
  i <- match(cal_ages, calibration[["cal_age"]])
  xcalcurve <- rnorm(
    n_samples,
    mean = calibration[["f14c_mu"]][i],
    sd = calibration[["f14c_sigma"]][i]
  )

  # measurement error
  # the right-shifted gamma with shape = 2 and scale = 3 gives the following
  # approximate distribution: min = 21, mean = 26, max = 42+ years
  measurement_error <- 20 + rgamma(n_samples, shape = 2, scale = 3)

  # convert to f14c space
  f14c_sigma <- xcalcurve * measurement_error / LIBBY

  # sample Fᵢ ~ N(c(tᵢ), zᵢ²)
  f14c_mu <- rnorm(
    n_samples,
    mean = xcalcurve,
    sd = f14c_sigma
  )

  # return
  samples <- data.frame(
    cal_age = cal_ages,
    f14c_mu = f14c_mu,
    f14c_sigma = f14c_sigma
  )

  structure(
    samples,
    class = c("C14Samples", class(samples)),
    start = attr(chronology, "start"),
    end = attr(chronology, "end")
  )
}

inv_transform_sample <- function(probs, years, n) {
  cdf <- cumsum(probs)
  p <- runif(n)
  i <- findInterval(p, cdf, all.inside = TRUE)
  sort(years[i])
}

#' Print C14Samples
#'
#' @param x a data.frame with class `C14Samples`
#' @param ... additional arguments (unused)
print.C14Samples <- function(x, ...) {
  template <- paste(
    "C14 Samples -----",
    "Years: %d-%d",
    "N Samples: %d\n",
    "Samples\n",
    sep = "\n"
  )

  txt <- sprintf(
    template,
    attr(x, "start"),
    attr(x, "end"),
    nrow(x)
  )

  cat(txt)
  nr <- min(5, nrow(x))
  print.data.frame(x[1:nr, , drop = FALSE], digits = 5)
  if (nrow(x) > nr) cat(sprintf(" ... [omitted %d rows]\n", nrow(x) - nr))
}
