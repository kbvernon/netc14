#' Dirichlet Process Mixture Model
#'
#' Fit Heaton's (2021) DPMM to radiocarbon samples using Polya Urn sampling.
#'
#' @param samples a data.frame with class `C14Samples`
#' @param n_iter number of MCMC iterations to run (default 10,000)
#' @param n_thin thinning interval; retain every `n_thin`-th iteration
#'   (default 5)
#'
#' @return A list with class `DPMM` containing the fitted model from
#'   `carbondate::PolyaUrnBivarDirichlet`. Attributes include `start`, `end`,
#'   and `elapsed`.
#'
#' @details Uses the IntCal20 calibration curve. Polya Urn sampling is
#'   recommended by Heaton (2021) for DPMM updating.
dpmm <- function(samples, n_iter = 1e4, n_thin = 5) {
  .start <- Sys.time()
  out <- carbondate::PolyaUrnBivarDirichlet(
    rc_determinations = samples[["f14c_mu"]],
    rc_sigmas = samples[["f14c_sigma"]],
    calibration_curve = carbondate::intcal20,
    F14C_inputs = TRUE,
    use_F14C_space = TRUE,
    n_iter = n_iter,
    n_thin = n_thin,
    show_progress = FALSE
  )

  # return
  structure(
    out,
    class = c("DPMM", class(out)),
    start = attr(samples, "start"),
    end = attr(samples, "end"),
    n_iter = n_iter,
    n_thin = n_thin,
    elapsed = as.numeric(Sys.time() - .start, units = "secs")
  )
}

#' @rdname density
#' @details For DPMM, draws 500 posterior samples.
density.DPMM <- function(x, years, ...) {
  years <- sort(as.numeric(years))

  v <- carbondate::FindPredictiveCalendarAgeDensity(
    x,
    years,
    n_posterior_samples = 500
  )
  names(v) <- c("cal_age", "density", "conf_lo", "conf_hi")

  # truncate density estimate (and kinda-sorta fix ci)
  z <- sum(v[["density"]])
  v <- transform(
    v,
    density = density / z,
    conf_lo = conf_lo / z,
    conf_hi = conf_hi / z,
    model = "DPMM"
  )

  # return
  structure(v[c(5, 1:4)], class = c("Density", "data.frame"))
}

#' @rdname predict
#' @details For DPMM, extracts each date's posterior calendar age samples
#'   after burn-in, so the number of draws is fixed by the sampler rather than
#'   `n_draws`.
predict.DPMM <- function(object, calibrated_dates, ...) {
  n_iter <- attr(object, "n_iter")
  n_thin <- attr(object, "n_thin")
  n_burn <- floor(n_iter / (2 * n_thin))

  age_estimates <- object[["calendar_ages"]]
  i <- (n_burn + 1):nrow(age_estimates)
  ages <- t(age_estimates[i, ])

  structure(ages, class = c("Ages", class(ages)))
}

#' Print DPMM
#'
#' @param x a fitted `DPMM` model
#' @param ... additional arguments (unused)
print.DPMM <- function(x, ...) {
  template <- paste(
    "Dirichlet Process Mixture Model -----",
    "Years: %d-%d",
    "N Samples: %d",
    "N Iter: %d",
    "N Mixtures: %d\n",
    sep = "\n"
  )

  cat(sprintf(
    template,
    attr(x, "start"),
    attr(x, "end"),
    ncol(x[["calendar_ages"]]),
    x[["input_parameters"]][["n_iter"]],
    count_components(x)
  ))

  print(get_parameters(x))
}
