#' Autoplot method for microbenchmark objects: Prettier graphs for
#' microbenchmark using ggplot2
#'
#' Uses ggplot2 to produce a more legible graph of microbenchmark timings
#'
#' @param object A microbenchmark object
#' @param unit What unit to use on the time axis. Defaults to the \code{unit}
#'   attribute of \code{object}, or the option \code{microbenchmark.unit} if the
#'   attribute is not set. If neither is set, \dQuote{t} is used.
#' @param \dots Passed to \code{ggplot2::geom_violin}
#' @param log If \code{TRUE} the time axis will be on log scale.
#' @param y_max The maximum time to plot in \code{unit}s (nanoseconds if the
#'   unit is \dQuote{t} (default) or \dQuote{f}. Note that if \code{unit} is a
#'   frequency, \code{y_max} is used as the \emph{minimum} frequency on the
#'   axis.
#' @return A ggplot2 plot
#'
#' @examples
#' if (requireNamespace("ggplot2")) {
#'
#' tm <- microbenchmark(rchisq(100, 0),
#'                      rchisq(100, 1),
#'                      rchisq(100, 2),
#'                      rchisq(100, 3),
#'                      rchisq(100, 5), times=1000L)
#' ggplot2::autoplot(tm)
#' }
#' @author Ari Friedman, Olaf Mersmann, Thomas Nygreen
autoplot.microbenchmark <- function(object, unit,...,
                                    log=TRUE,
                                    y_max=NULL) {
  if (!requireNamespace("ggplot2"))
    stop("Missing package 'ggplot2'.")
  
  if (missing(unit)) {
    unit <- attr(object, "unit")
  }

  if (is.null(unit)) {
    unit <- getOption("microbenchmark.unit", "t")
  } else {
    unit <- normalize_unit(unit)
  }
  
  object$ntime <- convert_to_unit(object$time, unit)
  
  y_min <- 0
  if (is.null(y_max)) {
    y_max <- max(object$ntime)
  } else if (unit == "t") {
    nunit <- normalize_unit(attr(object$ntime, "unit"))
    y_max <- convert_to_unit(y_max, nunit)
  } else if (unit == "f") {
    nunit <- normalize_unit(attr(object$ntime, "unit"))
    y_min <- convert_to_unit(y_max, nunit)
    y_max <- max(object$ntime)
  } else if (unit %in% c("hz", "khz", "mhz", "eps")) {
    y_min <- y_max
    y_max <- max(object$ntime)
  } else if (unit == "relative") {
    stop('Plotting with unit="relative" not implemented yet')
  }
  
  plt <- ggplot2::ggplot(object, ggplot2::aes(x = .data$ntime, y = .data$expr))
  plt <- plt + ggplot2::geom_violin(...)
  plt <- plt + ggplot2::scale_y_discrete(name="")
  y_label <- sprintf("Time [%s]", attr(object$ntime, "unit"))
  if (log) {
    y_min <- max(y_min, min(object$ntime[object$ntime > 0]))
    plt <- plt + ggplot2::scale_x_log10(name=y_label)
  } else {
    plt <- plt + ggplot2::scale_x_continuous(name=y_label)
  }
  plt <- plt + ggplot2::coord_cartesian(xlim=c(y_min, y_max))
  plt
}
