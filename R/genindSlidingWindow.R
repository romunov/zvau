#' @title Create a list of genind objects based on a sliding window.
#'
#' Create a list of \code{genind} objects based on a sliding window of dates. Assumed step size is one year.
#' The resulting list can be written to GENEPOP file using \code{writeGenPop} function.
#'
#' @param gi A \code{genind} object to be subsetted.
#' @param start.year Integer. Starting year. See \code{win.size}.
#' @param win.size Integer. One side of sliding window. If \code{start.year} is 1995 and \code{win.size} is
#' 2, sliding window will encompass years 1993:1997.
#' @param dates A vector of dates that correspond to individual genotype in \code{gi}. Often stored in \code{other(x)}.
#' Dates will be coerced to integer years (\%Y).
#' @param verbose Boolean. If TRUE, a message will be printed about cohort span. Can be silenced by setting this to FALSE.
#' @return A list of \code{genind} objects.
#'
#' @author Roman Lustrik (roman.lustrik@@biolitika.si)
#' @export

genindSlidingWindow <- function(gi, start.year, win.size, dates, verbose = TRUE) {

  dates <- as.integer(format(dates, "%Y"))
  stopifnot(start.year >= min(dates))

  from <- start.year
  to <- start.year + (win.size - 1)

  out <- vector("list")
  i <- 1
  make.step <- TRUE
  cohorts <- 0

  while (make.step == TRUE) {
    # If sliding window is outside the maximum time, abort.
    if (max(dates) <= to) {
      break
    }

    out[[i]] <- gi[dates > from & dates < to, ]
    # message(sprintf("Number of rows subsetted: %d", nInd(out[[i]])))
    # move one year forward
    from <- from + 1
    to <- to + 1
    i <- i + 1
    cohorts <- cohorts + 1
  }

  if (verbose) {
    goSlide(start = start.year, win.size = win.size, cohorts = cohorts)
  }

  out
}
