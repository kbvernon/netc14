#' Project Color Palette
#'
#' Get colors from the project palette, optionally lightened or darkened, and
#' keep the seventies alive.
#'
#' @param ... character names of colors in the palette, one of `"brown"`,
#'   `"orange"`, `"yellow"`, `"blue"`, or `"gray"`
#' @param shade numeric scalar in `[-1, 1]` giving the amount to shade each
#'   color, with negative values mixing toward white and positive values toward
#'   black. Default is 0 (no shading).
#'
#' @return A character vector of hex color strings.
#'
#' @details Palette generate by coolors.co:
#' https://coolors.co/33261d-ba3f1d-db9d47-107e7d-4d4d4d
coolors <- function(..., shade = 0) {
  .colors <- c(
    "brown" = "#33261D",
    "orange" = "#BA3F1D",
    "yellow" = "#DB9D47",
    "blue" = "#107E7D",
    "gray" = "#4D4D4D"
  )

  .colors <- unname(.colors[c(...)])
  .colors <- col2rgb(.colors)
  target <- if (shade < 0) 255 else 0
  .colors <- .colors + (target - .colors) * abs(shade)
  rgb(t(.colors), maxColorValue = 255)
}

#' Model Colors
#'
#' Get the color assigned to each model, so that models are styled consistently
#' across figures.
#'
#' @param ... character names of models, one of `"BGMM"`, `"NetC14"`, `"CKDE"`,
#'   `"DPMM"`, `"SPD"`, or `"Oracle"`
#' @param shade numeric scalar in `[-1, 1]` passed to [coolors()]
#'
#' @return A character vector of hex color strings.
theme <- function(..., shade = 0) {
  lookup <- c(
    BGMM = "brown",
    NetC14 = "orange",
    CKDE = "yellow",
    DPMM = "blue",
    SPD = "gray",
    Oracle = "gray"
  )
  coolors(lookup[c(...)], shade = shade)
}

#' Default Graphical Parameters
#'
#' A list of default [par()] settings giving figures a consistent look, mostly
#' text sizes and axis label color.
#'
#' @format A named list of graphical parameters.
defaults <- list(
  cex = 0.9,
  cex.axis = 0.7,
  cex.lab = 0.8,
  cex.main = 0.8,
  col.axis = "#666666",
  oma = rep(0.2, 4),
  mgp = c(2.5, 0.3, 0)
)
