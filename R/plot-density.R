#' Plot Density
#'
#' Plot a model estimate of the true chronology as a line showing the
#' probability density over time.
#'
#' @param x a data.frame with class `Density` containing columns `cal_age`,
#'   `density`, `conf_lo`, and `conf_hi`
#' @param se logical scalar, if `TRUE` draw a shaded polygon showing the
#'   confidence interval
#' @param add logical scalar, if `TRUE` add the line to the current plot instead
#'   of creating a new one. Default is `FALSE`.
#' @param col line color (required, no default)
#' @param fill polygon fill color, defaults to `adjustcolor(col, 0.2)`
#' @param lty line type, default is `"solid"`
#' @param lwd line width, default is 1.2
#' @param xlab x-axis label, defaults to `"Years Before Present (t)"`
#' @param ylab y-axis label, defaults to `"Density"`
#' @param xlim a length two numeric vector of the x-limits
#' @param ylim a length two numeric vector of the y-limits
#' @param ... additional arguments passed to [plot.window()] if `add = FALSE`
#'
#' @return Invisibly returns `x`.
plot.Density <- function(
  x,
  se = FALSE,
  add = FALSE,
  col,
  fill = adjustcolor(col, 0.2),
  lty = "solid",
  lwd = 1.2,
  xlab = "Years Before Present (t)",
  ylab = "Density",
  xlim = NULL,
  ylim = NULL,
  ...
) {
  if (!add) {
    if (is.null(xlim)) {
      xlim <- range(x[["cal_age"]])
    }

    if (is.null(ylim)) {
      ylim <- c(0, max(x[["conf_hi"]]))
    }

    new_plot(xlim, ylim, ...)

    x_axis()
    y_axis()

    x_title(xlab)
    y_title(ylab)

    add_grid()
  }

  if (se) {
    polygon(
      x = c(x[["cal_age"]], rev(x[["cal_age"]])),
      y = c(x[["conf_lo"]], rev(x[["conf_hi"]])),
      col = fill,
      border = NA
    )
  }

  lines(
    x[["cal_age"]],
    x[["density"]],
    col = col,
    lwd = lwd,
    lty = lty
  )

  invisible(x)
}
