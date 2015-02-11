#' Handles dropping levels of subsetted data
#' 
#' Get a fully subsetted \code{genind} object. \code{genind} methods handles subsetting and this
#' functions drops levels by reconstructing the object from scratch.
#' 
#' @param x A subsetted \code{genind} object, possibly with other fields in \code{other} slot.
#' @author Roman Lustrik (roman.lustrik@@biolitika.si)

subsetGenData <- function(x) {
  oth <- lapply(other(x), droplevels)
  xgen <- genind2df(x, sep = "/")[, locNames(x)]
  pet <- df2genind(X = xgen, sep = "/", ind.names = indNames(x),
                   loc.names = locNames(x))
  pet@other <- oth
  pet
}