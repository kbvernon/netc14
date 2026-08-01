#' Calibrate Radiocarbon Dates
#'
#' Revise uncertainty about the true ages of radiocarbon samples using a
#' calibration curve.
#'
#' @param samples a data.frame with class `C14Samples`
#' @param calibration a data.frame with class `Calibration`
#'
#' @return An n x m matrix with class `CalGrid` containing likelihoods for the
#'   n radiocarbon samples evaluated over m years from `start` to `end`.
#'
#' @details Rows are not normalized; consumers normalize as needed.
calibrate <- function(samples, calibration) {
  # avoid calibrating over the entire length of the curve
  start <- attr(samples, "start")
  end <- attr(samples, "end")
  calibration <- subset(
    calibration,
    cal_age >= start & cal_age <= end
  )

  # do calibration
  calibrated_dates <- Map(
    calibrate_sample,
    f14c_mu = samples[["f14c_mu"]],
    f14c_sigma = samples[["f14c_sigma"]],
    MoreArgs = list(calibration = calibration)
  )
  calibrated_dates <- do.call(rbind, calibrated_dates)

  # return
  structure(
    calibrated_dates,
    class = c("CalGrid", class(calibrated_dates)),
    start = start,
    end = end
  )
}

calibrate_sample <- function(f14c_mu, f14c_sigma, calibration) {
  total_error <- sqrt(calibration[["f14c_sigma"]]^2 + f14c_sigma^2)
  dnorm(
    f14c_mu,
    mean = calibration[["f14c_mu"]],
    sd = total_error
  )
}

#' Print CalGrid
#'
#' @param x a matrix with class `CalGrid`
#' @param ... additional arguments (unused)
print.CalGrid <- function(x, ...) {
  nr <- min(5, nrow(x))
  nc <- min(5, ncol(x))
  preview <- unclass(x)[1:nr, 1:nc, drop = FALSE]
  colnames(preview) <- seq(attr(x, "start"), attr(x, "start") + nc - 1)
  rownames(preview) <- seq_len(nr)

  template <- paste(
    "Calibrated Radiocarbon Dates -----",
    "Years: %d-%d",
    "N Samples: %d\n",
    "Posteriors\n",
    sep = "\n"
  )

  cat(sprintf(
    template,
    attr(x, "start"),
    attr(x, "end"),
    nrow(x)
  ))
  print(preview)
  if (nrow(x) > nr || ncol(x) > nc) {
    parts <- character(0)
    if (nrow(x) > nr) {
      parts <- c(parts, sprintf("%d rows", nrow(x) - nr))
    }
    if (ncol(x) > nc) {
      parts <- c(parts, sprintf("%d columns", ncol(x) - nc))
    }
    cat(sprintf(" ... [omitted %s]\n", paste(parts, collapse = " and ")))
  }
  invisible(x)
}
