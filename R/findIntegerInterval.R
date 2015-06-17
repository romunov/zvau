#' Find interval of sequential integers
#'
#' Function will find a sequence of integers and create a factor for
#' each repeat.
#' @author Roman Lustrik (\email{roman.lustrik@@biolitika.si})
#' @param x Vector.
#' @export
#' 
#' @examples
#' x <- data.frame(orig = c(0,0,0, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4,
#'        0,0,0,0,1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,5,5,5,
#'        0,0,0,0,1,1,1,1,2,2,2,2,3,4,5,6,6,6,6,6,6,7,7,7,7,7,8,8,8,8))
#' x$group <- findIntegerInterval(x$orig)
#' x

findIntegerInterval <- function(x) {
  xd <- diff(x)
  xd[xd == 1] <- 0 # skip local repeats
  xr <- rle(xd)$lengths
  xr <- xr[!xr %in% c(-1, 1)] + 1
  out <- as.factor(rep(1:length(xr),  times = xr))
  out
}