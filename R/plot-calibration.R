#' Plot Calibration Curve
#'
#' Plot a radiocarbon calibration curve showing fraction modern carbon over
#' calendar years with a 95% confidence band.
#'
#' @param x a data.frame with class `Calibration` containing columns `cal_age`,
#'   `f14c_mu`, and `f14c_sigma`
#' @param se logical scalar, if `TRUE` draw a shaded polygon showing the
#'   confidence interval
#' @param col line color, defaults to `"#1A1A1A"`
#' @param fill polygon fill color for the confidence band, defaults to
#'   `adjustcolor(col, 0.2)`
#' @param lty line type, defaults to `"solid"`
#' @param lwd line width, defaults to 1
#' @param xlab x-axis label, defaults to `"Years Before Present (t)"`. If
#'   `NULL`, then the x-axis is suppressed.
#' @param ylab y-axis label, defaults to `"Fraction Modern"`
#' @param xlim a length two numeric vector of the x-limits
#' @param ylim a length two numeric vector of the y-limits
#' @param ... additional arguments passed to [plot.window()]
#'
#' @return Invisibly returns `x`.
#'
#' @details This is mainly useful for plotting the calibration curve alongside
#'   the reconstructed chronology, hence allowing for suppressing the x-axis.
plot.Calibration <- function(
  x,
  se = TRUE,
  col = "#1A1A1A",
  fill = adjustcolor(col, 0.2),
  lty = "solid",
  lwd = 1,
  xlab = "Years Before Present (t)",
  ylab = "Fraction Modern",
  xlim = NULL,
  ylim = NULL,
  ...
) {
  if (is.null(xlim)) {
    xlim <- range(x[["cal_age"]])
  }

  if (is.null(ylim)) {
    ylim <- range(x[["f14c_mu"]]) + range(x[["f14c_sigma"]]) * c(-1, 1)
  }

  new_plot(xlim, ylim, ...)

  if (!is.null(xlab)) {
    x_axis()
    x_title(xlab)
  }

  if (!is.null(ylab)) {
    y_axis()
    y_title(ylab, line = 2.5)
  }

  add_grid()

  if (se) {
    se <- 1.96 * x[["f14c_sigma"]]

    polygon(
      x = c(x[["cal_age"]], rev(x[["cal_age"]])),
      y = c(x[["f14c_mu"]] - se, rev(x[["f14c_mu"]] + se)),
      col = fill,
      border = NA
    )
  }

  lines(
    x = x[["cal_age"]],
    y = x[["f14c_mu"]],
    col = col,
    lwd = lwd,
    lty = lty
  )

  invisible(x)
}
