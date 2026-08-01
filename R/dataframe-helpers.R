# just wrote all these to prevent methods from stripping the assigned S3 classes

#' Transform a data.frame
#'
#' Update or add variables to a data.frame
#'
#' @param _data a `Density`, `C14Samples`, or `Calibration` data.frame
#' @param ... Further arguments of the form `tag=value`
#'
#' @return A data.frame with class `Density`, `C14Samples`, or `Calibration`.
transform.Calibration <- function(`_data`, ...) {
  curve <- attr(`_data`, "curve")
  structure(NextMethod(), class = c("Calibration", "data.frame"), curve = curve)
}

#' @rdname transform.Calibration
transform.Density <- function(`_data`, ...) {
  structure(NextMethod(), class = c("Density", "data.frame"))
}

#' @rdname transform.Calibration
transform.C14Samples <- function(`_data`, ...) {
  start <- attr(`_data`, "start")
  end <- attr(`_data`, "end")
  structure(
    NextMethod(),
    class = c("C14Samples", "data.frame"),
    start = start,
    end = end
  )
}

#' Rbind a data.frame
#'
#' Bind rows of data.frames together.
#'
#' @param ... `Density`, `C14Samples`, or `Calibration` data.frames to bind.
#' @param deparse.level passed to [rbind.data.frame()].
#'
#' @return A data.frame with class `Density`, `C14Samples`, or `Calibration`.
rbind.Calibration <- function(..., deparse.level = 1) {
  structure(
    rbind.data.frame(..., deparse.level = deparse.level),
    class = c("Calibration", "data.frame")
  )
}

#' @rdname rbind.Calibration
rbind.Density <- function(..., deparse.level = 1) {
  structure(
    rbind.data.frame(..., deparse.level = deparse.level),
    class = c("Density", "data.frame")
  )
}

#' @rdname rbind.Calibration
rbind.C14Samples <- function(..., deparse.level = 1) {
  structure(
    rbind.data.frame(..., deparse.level = deparse.level),
    class = c("C14Samples", "data.frame")
  )
}

#' Subset a data.frame
#'
#' Return a subset of rows matching a condition.
#'
#' @param x a `Density`, `C14Samples`, or `Calibration` data.frame.
#' @param ... passed to [subset.data.frame()].
#'
#' @return A data.frame with class `Density`, `C14Samples`, or `Calibration`.
subset.Calibration <- function(x, ...) {
  curve <- attr(x, "curve")
  structure(NextMethod(), class = c("Calibration", "data.frame"), curve = curve)
}

#' @rdname subset.Calibration
subset.Density <- function(x, ...) {
  structure(NextMethod(), class = c("Density", "data.frame"))
}

#' @rdname subset.Calibration
subset.C14Samples <- function(x, ...) {
  start <- attr(x, "start")
  end <- attr(x, "end")
  structure(
    NextMethod(),
    class = c("C14Samples", "data.frame"),
    start = start,
    end = end
  )
}

#' Extract from a data.frame
#'
#' Extract rows, columns, or both from a data.frame.
#'
#' @param x a `Density`, `C14Samples`, or `Calibration` data.frame.
#' @param ... indices passed to `[.data.frame`.
#'
#' @return A data.frame with class `Density`, `C14Samples`, or `Calibration`,
#'   or a vector if a single column is extracted.
#' @name sub-.Calibration
`[.Calibration` <- function(x, ...) {
  curve <- attr(x, "curve")
  out <- NextMethod()
  if (is.data.frame(out)) {
    out <- structure(out, class = c("Calibration", "data.frame"), curve = curve)
  }
  out
}

#' @rdname sub-.Calibration
`[.Density` <- function(x, ...) {
  out <- NextMethod()
  if (is.data.frame(out)) {
    class(out) <- c("Density", "data.frame")
  }
  out
}

#' @rdname sub-.Calibration
`[.C14Samples` <- function(x, ...) {
  start <- attr(x, "start")
  end <- attr(x, "end")
  out <- NextMethod()
  if (is.data.frame(out)) {
    out <- structure(
      out,
      class = c("C14Samples", "data.frame"),
      start = start,
      end = end
    )
  }
  out
}

#' Assign into a data.frame
#'
#' Assign values to rows, columns, or cells of a data.frame.
#'
#' @param x a `Density`, `C14Samples`, or `Calibration` data.frame.
#' @param ... indices passed to `[<-.data.frame` or `[[<-.data.frame`.
#' @param value the replacement value.
#'
#' @return A data.frame with class `Density`, `C14Samples`, or `Calibration`.
#' @name sub-assign-.Calibration
`[<-.Calibration` <- function(x, ..., value) {
  curve <- attr(x, "curve")
  structure(NextMethod(), class = c("Calibration", "data.frame"), curve = curve)
}

#' @rdname sub-assign-.Calibration
`[[<-.Calibration` <- function(x, ..., value) {
  curve <- attr(x, "curve")
  structure(NextMethod(), class = c("Calibration", "data.frame"), curve = curve)
}

#' @rdname sub-assign-.Calibration
`[<-.Density` <- function(x, ..., value) {
  structure(NextMethod(), class = c("Density", "data.frame"))
}

#' @rdname sub-assign-.Calibration
`[[<-.Density` <- function(x, ..., value) {
  structure(NextMethod(), class = c("Density", "data.frame"))
}

#' @rdname sub-assign-.Calibration
`[<-.C14Samples` <- function(x, ..., value) {
  start <- attr(x, "start")
  end <- attr(x, "end")
  structure(
    NextMethod(),
    class = c("C14Samples", "data.frame"),
    start = start,
    end = end
  )
}

#' @rdname sub-assign-.Calibration
`[[<-.C14Samples` <- function(x, ..., value) {
  start <- attr(x, "start")
  end <- attr(x, "end")
  structure(
    NextMethod(),
    class = c("C14Samples", "data.frame"),
    start = start,
    end = end
  )
}
