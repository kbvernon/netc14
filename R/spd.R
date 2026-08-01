#' Summed Probability Distribution
#'
#' Sum the posteriors of individually calibrated radiocarbon dates.
#'
#' @param calibrated_dates a `CalGrid` matrix of calibrated dates
#'
#' @return A numeric vector of summed posterior probabilities with class `SPD`.
#'   Attributes include `n_samples`, `start`, `end`, and `elapsed`.
#'
#' @details The distribution is normalized within the range of `start` and
#'   `end`.
spd <- function(calibrated_dates) {
  .start <- Sys.time()
  # row normalize before taking column means
  density <- colMeans(calibrated_dates / rowSums(calibrated_dates))
  structure(
    density,
    class = c("SPD", class(density)),
    n_samples = nrow(calibrated_dates),
    start = attr(calibrated_dates, "start"),
    end = attr(calibrated_dates, "end"),
    elapsed = as.numeric(Sys.time() - .start, units = "secs")
  )
}

#' Estimate Chronology
#'
#' @param x a fitted SPD object
#' @param years numeric vector of calendar years for density estimation
#' @param ... additional arguments (unused)
#'
#' @return A data.frame with class `Density` containing columns: model,
#'   cal_age, density, conf_lo, and conf_hi.
#'
#' @details For SPD, confidence intervals are NA.
#'
#' @name density
density.SPD <- function(x, years, ...) {
  cal_age <- seq(attr(x, "start"), attr(x, "end"))
  i <- which(cal_age %in% years)
  d <- unclass(x)[i]

  # return
  D <- data.frame(
    model = "SPD",
    cal_age = years,
    density = d,
    conf_lo = NA,
    conf_hi = NA
  )

  structure(D, class = c("Density", "data.frame"))
}

#' Estimate Radiocarbon Ages
#'
#' @param object a fitted SPD object
#' @param calibrated_dates a `CalGrid` matrix of calibrated dates
#' @param n_draws integer, number of calendar age samples to draw from each
#'   date's calibrated posterior (default 1000)
#' @param ... additional arguments (unused)
#'
#' @return An `n_samples x n_draws` matrix with class `Ages` containing
#'   calendar age samples for each date. See details for how each model
#'   weights the calibrated posteriors it samples from.
#'
#' @details For SPD, draws directly from each date's calibrated posterior via
#'   inverse transform sampling.
#'
#' @name predict
predict.SPD <- function(object, calibrated_dates, n_draws = 1000, ...) {
  years <- seq(attr(calibrated_dates, "start"), attr(calibrated_dates, "end"))

  # row normalize measurement matrix
  calibrated_dates <- calibrated_dates / rowSums(calibrated_dates)

  ages <- t(apply(
    calibrated_dates,
    MARGIN = 1,
    FUN = inv_transform_sample,
    years = years,
    n = n_draws
  ))

  structure(ages, class = c("Ages", class(ages)))
}

#' Print SPD
#'
#' @param x a fitted `SPD` object
#' @param ... additional arguments (unused)
print.SPD <- function(x, ...) {
  n <- min(35, length(x))
  d <- round(unclass(x)[1:n], 3)
  names(d) <- seq(attr(x, "start"), attr(x, "end"))[1:n]

  template <- paste(
    "Summed Probability Distribution -----",
    "Years: %d-%d",
    "N Samples: %d\n",
    "Density\n",
    sep = "\n"
  )

  txt <- sprintf(
    template,
    attr(x, "start"),
    attr(x, "end"),
    attr(x, "n_samples")
  )

  cat(txt)
  out <- capture.output(print(d))
  if (length(x) > n) {
    omitted <- sprintf(" ... [omitted %d values]", length(x) - n)
    out[length(out)] <- paste0(out[length(out)], omitted)
  }
  cat(out, sep = "\n")
}
