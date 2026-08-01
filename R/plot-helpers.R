# These are helper functions for consistent styling.

#' Setup Empty Plot
#'
#' Set up a blank plot with specified limits in x and y.
#'
#' @param xlim numeric vector of length 2 giving the x-axis limits
#' @param ylim numeric vector of length 2 giving the y-axis limits
#' @param ... additional arguments passed to [plot.window()]
#'
#' @return `NULL` invisibly.
new_plot <- function(xlim, ylim, ...) {
  plot.new()
  plot.window(
    xlim = xlim,
    ylim = ylim,
    ...
  )
}

#' Add Axis
#'
#' @param lwd numeric scalar, line width for the axis line. Zero or negative
#'   values will suppress the line. Default is 0.
#' @param lwd.ticks numeric scalar, line width for tick marks. Zero or negative
#'   values will suppress the line or ticks. Default is 0.
#' @param las integer scalar, orientation of labels with respect to plot.
#'   Default is 1 (or horizontal).
#' @param ... additional arguments passed to [axis()]
x_axis <- function(lwd = 0, lwd.ticks = 0, las = 1, ...) {
  axis(1, lwd = lwd, lwd.ticks = lwd.ticks, las = las, ...)
}

y_axis <- function(lwd = 0, lwd.ticks = 0, las = 1, ...) {
  axis(2, lwd = lwd, lwd.ticks = lwd.ticks, las = las, ...)
}

#' Add Axis Titles
#'
#' @param lab character scalar, the axis label
#' @param line numeric scalar, the margin line to place the label on. Defaults
#'   are 1.7 for the x-axis and 2.0 for the y-axis.
x_title <- function(lab, line = 1.7, ...) {
  title(xlab = lab, line = line, ...)
}

y_title <- function(lab, line = 2.0, ...) {
  title(ylab = lab, line = line, ...)
}

#' Add Grid
#'
#' @param col grid line color, defaults to `"#999999"`
#' @param lty grid line type, defaults to `"dotted"`
#' @param lwd grid line width, defaults to `0.5`
#' @param h the y-value(s) for horizontal line(s), default is `NULL`
#' @param v the x-value(s) for vertical line(s), default is `NULL`
#' @param nx number of cells in the x direction, passed to [grid()]. Use `NA`
#'   to suppress vertical grid lines.
#' @param ny number of cells in the y direction, passed to [grid()]. Use `NA`
#'   to suppress horizontal grid lines.
#' @param ... additional arguments passed to [grid()]
add_grid <- function(
  col = "#999999",
  lty = "dotted",
  lwd = 0.4,
  h = NULL,
  v = NULL,
  nx = NULL,
  ny = NULL,
  ...
) {
  if (!is.null(h)) {
    abline(h = h, col = col, lty = lty, lwd = lwd)
    ny <- NA
  }

  if (!is.null(v)) {
    abline(v = v, col = col, lty = lty, lwd = lwd)
    nx <- NA
  }

  grid(
    nx = nx,
    ny = ny,
    col = col,
    lty = lty,
    lwd = lwd,
    ...
  )
}

#' Add Legend
#'
#' @param ... arguments passed to [legend()], typically position and labels
#' @param bty character, type of box drawn around the legend. Default is `"n"`
#'   (no box).
#' @param cex numeric scalar, character expansion factor for legend text.
#'   Default is `0.8`.
#' @param y.intersp numeric scalar, vertical spacing between legend entries.
#'   Default is `1.2`.
#' @param title.adj numeric scalar, horizontal adjustment of the legend title.
#'   Default is `0` (left-aligned).
#' @param title.cex numeric scalar, character expansion factor for the legend
#'   title. Default is `0.8`.
add_legend <- function(
  ...,
  bty = "n",
  cex = 0.8,
  y.intersp = 1.2,
  title.adj = 0,
  title.cex = 0.8
) {
  legend(
    ...,
    bty = bty,
    cex = cex,
    y.intersp = y.intersp,
    title.adj = title.adj,
    title.cex = title.cex
  )
}
