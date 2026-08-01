#' Composite Kernel Density Estimation
#'
#' Estimate a chronology using composite kernel density estimation (CKDE).
#'
#' @param calibrated_dates a `CalGrid` matrix of calibrated dates
#' @param n_iter integer scalar, the number of calendar years to draw from each
#'   calibrated posterior. Default is 500.
#'
#' @return A matrix with class `CKDE` containing `n_iter` rows and one column
#'   per calendar year. Each row is a kernel density estimate of the chronology
#'   based on samples drawn from each calibrated posterior. Attributes include
#'   `n_samples`, `start`, `end`, and `elapsed`.
#'
#' @details CKDE works by drawing `n_iter` calendar years from each calibrated
#'   posterior using inverse transform sampling, then estimating kernel
#'   densities for each iteration. It produces a distribution of density
#'   estimates over years that can be summarized with quantiles to get
#'   confidence intervals.
ckde <- function(calibrated_dates, n_iter = 500) {
  .start <- Sys.time()
  start <- attr(calibrated_dates, "start")
  end <- attr(calibrated_dates, "end")
  years <- seq(start, end)
  n_years <- ncol(calibrated_dates)
  n_samples <- nrow(calibrated_dates)

  # row normalize measurement matrix before sampling
  calibrated_dates <- calibrated_dates / rowSums(calibrated_dates)

  # this creates an n_iter x n_samples matrix
  samples <- apply(
    calibrated_dates,
    MARGIN = 1,
    FUN = inv_transform_sample,
    years = years,
    n = n_iter
  )

  # this creates an n_years x n_iter matrix
  out <- apply(
    samples,
    MARGIN = 1,
    FUN = \(.x) density(.x, from = start, to = end, n = n_years)[["y"]]
  )

  # transpose to n_iter x n_years
  out <- t(out)

  # return
  structure(
    out,
    class = c("CKDE", class(out)),
    n_samples = n_samples,
    start = start,
    end = end,
    elapsed = as.numeric(Sys.time() - .start, units = "secs")
  )
}

#' @rdname density
density.CKDE <- function(x, years, ...) {
  cal_age <- seq(attr(x, "start"), attr(x, "end"))
  i <- sort(which(cal_age %in% years))

  # estimate median density and confidence intervals
  D <- apply(
    x[, i],
    MARGIN = 2,
    FUN = \(.x) quantile(.x, prob = c(0.025, 0.5, 0.975))
  )

  # collect it all into a data.frame with standard format
  D <- as.data.frame(t(D))
  names(D) <- c("conf_lo", "density", "conf_hi")

  D[["model"]] <- "CKDE"
  D[["cal_age"]] <- years

  D <- D[c("model", "cal_age", "density", "conf_lo", "conf_hi")]

  # return
  structure(D, class = c("Density", "data.frame"))
}

#' @rdname predict
#' @details For CKDE, weights each date's calibrated posterior by the median
#'   kernel density estimate and draws calendar age samples via inverse
#'   transform sampling.
predict.CKDE <- function(object, calibrated_dates, n_draws = 1000, ...) {
  years <- seq(
    attr(calibrated_dates, "start"),
    attr(calibrated_dates, "end")
  )

  v <- density(object, years)[["density"]]
  q <- t(t(calibrated_dates) * v)
  q <- q / rowSums(q)

  ages <- t(apply(
    q,
    MARGIN = 1,
    FUN = inv_transform_sample,
    years = years,
    n = n_draws
  ))

  structure(ages, class = c("Ages", class(ages)))
}

#' Print CKDE
#'
#' @param x a fitted CKDE object
#' @param ... additional arguments (unused)
print.CKDE <- function(x, ...) {
  nr <- min(5, nrow(x))
  nc <- min(5, ncol(x))
  preview <- unclass(x)[1:nr, 1:nc, drop = FALSE]
  colnames(preview) <- seq(attr(x, "start"), attr(x, "start") + nc - 1)
  rownames(preview) <- seq_len(nr)

  template <- paste(
    "Composite Kernel Density Estimation -----",
    "Years: %d-%d",
    "N Iter: %d\n",
    "Densities\n",
    sep = "\n"
  )

  txt <- sprintf(
    template,
    attr(x, "start"),
    attr(x, "end"),
    nrow(x)
  )

  cat(txt)
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
}
