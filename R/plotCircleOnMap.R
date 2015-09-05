#' Plot circles on a map
#' 
#' This is not yet a fully general function for plotting circles on a google map as presented by the \code{dismo} package.
#' 
#' @param coords \code{data.frame} with columns latitude, longitude and projection already defined as in \code{sp}.
#' @param multiply.r Numeric. Multiplicator that will inflate or deflate circle (original in miles).
#' @param multiply.se Numeric. Same as for \code{multiply.r} but for line width (standard error).
#' @param map A \code{RasterLayer} object from \code{dismo:::gmap}.
#' @author Roman Lustrik (roman.lustrik@@biolitika.si)
#' @return A list of input data and map used in plotting.
#' 
#' @importFrom dismo gmap
#' @importFrom rgeos gBuffer
#' @importFrom sp spTransform CRS projection
#' @export

plotCircleOnMap <- function(coords, multiply.r, multiply.se, map) {
  mile2meter <- function(x) {
    x * 1609.344
  }
  
  coords.mrc <- spTransform(coords, CRS = CRS(projection(map)))
  
  coords.list <- vector("list", nrow(coords.mrc))
  for (i in seq_len(nrow(coords.mrc))) {
    coords.list[i] <- gBuffer(coords.mrc[i, ], width = multiply.r * mile2meter(coords.mrc[i, ]@data["Ar"]))
  }
  
  plot(map)
  
  mapply(coords.list, as.list(coords.mrc@data$SEAr), FUN = function(x, y, multiply.se) {
    plot(x, add = TRUE, lwd = multiply.se*y)
  }, multiply.se = multiply.se)
  
  return(list(coords, map))
}