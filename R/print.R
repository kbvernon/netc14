#' Print Calibration
#'
#' @param x a data.frame with class `Calibration` containing columns `cal_age`,
#'   `f14c_mu`, and `f14c_sigma`
#' @param ... additional arguments (unused)
print.Calibration <- function(x, ...) {
  year_range <- range(x[["cal_age"]])

  template <- paste(
    "Calibration Curve -----",
    "Name: %s",
    "Years: %d-%d\n",
    "Curve\n",
    sep = "\n"
  )

  txt <- sprintf(
    template,
    attr(x, "curve"),
    year_range[1],
    year_range[2]
  )

  cat(txt)
  nr <- min(5, nrow(x))
  print.data.frame(x[1:nr, ], digits = 5)
  if (nrow(x) > nr) cat(sprintf(" ... [omitted %d rows]\n", nrow(x) - nr))
}

#' Print Ages
#'
#' @param x an `Ages` matrix (n_samples x n_draws) of calendar age samples
#' @param ... additional arguments (unused)
print.Ages <- function(x, ...) {
  n_samples <- nrow(x)
  n_draws <- ncol(x)

  template <- paste(
    "Calendar Age Estimate -----",
    "N Samples: %d",
    "N Draws: %d\n",
    "Summary\n",
    sep = "\n"
  )

  txt <- sprintf(template, n_samples, n_draws)
  cat(txt)

  nr <- min(5, n_samples)
  nc <- min(5, n_draws)
  preview <- unclass(x)[1:nr, 1:nc, drop = FALSE]
  rownames(preview) <- seq_len(nr)
  colnames(preview) <- seq_len(nc)

  print(preview)
  if (n_samples > nr || n_draws > nc) {
    parts <- character(0)
    if (n_samples > nr) {
      parts <- c(parts, sprintf("%d rows", n_samples - nr))
    }
    if (n_draws > nc) {
      parts <- c(parts, sprintf("%d columns", n_draws - nc))
    }
    cat(sprintf(" ... [omitted %s]\n", paste(parts, collapse = " and ")))
  }
}

#' Print Density
#'
#' @param x a data.frame with class `Density` containing columns `model`,
#'   `cal_age`, `density`, `conf_lo`, and `conf_hi`
#' @param ... additional arguments (unused)
print.Density <- function(x, ...) {
  year_range <- range(x[["cal_age"]])

  template <- paste(
    "Density Estimate -----",
    "Years: %d-%d",
    "N Years: %d\n",
    "Density\n",
    sep = "\n"
  )

  txt <- sprintf(
    template,
    year_range[1],
    year_range[2],
    nrow(x)
  )

  cat(txt)
  nr <- min(5, nrow(x))
  print.data.frame(x[1:nr, , drop = FALSE], digits = 5)
  if (nrow(x) > nr) cat(sprintf(" ... [omitted %d rows]\n", nrow(x) - nr))
}
