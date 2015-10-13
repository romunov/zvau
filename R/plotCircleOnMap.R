#' Plot circles on a map
#' 
#' This is not yet a fully general function for plotting circles on a google map as presented by the \code{dismo} package.
#' 
#' @param coords \code{data.frame} with columns latitude, longitude and projection already defined as in \code{sp}.
#' @param multiply.r Numeric. Multiplicator that will inflate or deflate circle (original in miles). If plotting several
#' variables, length should match length of \code{var}.
#' @param multiply.se Numeric. Same as for \code{multiply.r} but for line width (standard error). If plotting several
#' variables, length should match length of \code{var}.
#' @param map A \code{RasterLayer} object from \code{dismo:::gmap}.
#' @param var A character vector of variable names to be plotted.
#' @param lty Parameters for line type passed on to \code{plot}. Should be of equal length as \code{var}.
#' @param plot.se If standard error is to be plotted as line width, mark this as \code{TRUE}.
#' 
#' @author Roman Lustrik (roman.lustrik@@biolitika.si)
#' @return A list of input data and map used in plotting.
#' 
#' @importFrom dismo gmap
#' @importFrom rgeos gBuffer
#' @importFrom sp spTransform CRS
#' @importFrom raster projection
#' @export
#' @examples 
#' 
#' library(sp)
#' library(rgeos)
#' library(dismo)
#' library(raster)
#' library(rgdal)
#' library(dismo)
#' 
#' x <- structure(list(long = c(42.406612, 45.490354, 44.134308, 45.227887, 
#' 63.572205, 62.41059, 63.732431, 59.034802, 52.167186, 58.909385, 
#' 57.638552, 57.873012), lat = c(13.285433, 14.428011, 15.372835, 
#'                                15.526644, 29.303401, 27.455654, 32.296557, 24.09289, 19.732382, 
#'                                25.524104, 26.622737, 32.819026), Ar = c(0.9147714, 1.0376678, 
#'                                                                         1.0789556, 1.1745495, 1.13741, 1.1037158, 1.208743, 1.3474194, 
#'                                                                         1.6095411, 1.3657087, 1.9154771, 1.86279), SEAr = c(0.17197734, 
#'                                                                                                                             0.0674223, 0.06577994, 0.16613932, 0.17072725, 0.15117987, 0.1377877, 
#'                                                                                                                             0.18592455, 0.36878468, 0.14959118, 0.27631184, 0.379309)), .Names = c("long", 
#'                                                                                                                                                                                                    "lat", "Ar", "SEAr"), class = "data.frame", row.names = c(2L, 
#'                                                                                                                                                                                                                                                              8L, 16L, 17L, 23L, 24L, 25L, 26L, 27L, 30L, 31L, 32L))
#' coordinates(x) <- ~ lat + long
#' projection(x) <- "+init=epsg:4326"
#'
#' my.map <- gmap(extent(x), zoom = 4, scale = 2)
#' plotCircleOnMap(coords = x, multiply.r = c(100, 50), multiply.se = c(10, 20), 
#'                 map = my.map, var = c("Ar"), lty = c("solid", "dashed"))

plotCircleOnMap <- function(coords, multiply.r = rep(1, length(var)), multiply.se = rep(1, length(var)), 
                            map, var, lty = rep("solid", length(var)), plot.se = FALSE) {
  mile2meter <- function(x) {
    x * 1609.344
  }
  
  coords.mrc <- spTransform(coords, CRSobj = CRS(projection(map)))
  names(var) <- var
  
  each.var.buff <- mapply(FUN = function(x, multiply.r) {
    coords.list <- vector("list", nrow(coords.mrc))
    for (i in seq_len(nrow(coords.mrc))) {
      # browser()
      coords.list[i] <- gBuffer(coords.mrc[i, ], width = multiply.r * mile2meter(coords.mrc[i, ]@data[x]))
    }
    coords.list
  },
  x = as.list(var),
  multiply.r = as.list(multiply.r), SIMPLIFY = FALSE)
  
  plot(map)
  
  mapply(each.var.buff, 
         as.list(multiply.se), 
         as.list(lty), 
         names(each.var.buff), 
         FUN = function(coords.list, multiply.se, lty, var) {
           # (1) if plotting of SE is selected, select appropriate columns - otherwise return NA
           # this if clause is directly linked to # (2)
           if (plot.se == TRUE) {
             se.cols <- coords.mrc@data[, paste("SE", var, sep = "")] * multiply.se
           } else {
             se.cols <- rep(NA, length(coords.list))
           }
           mapply(coords.list, 
                  as.list(se.cols), 
                  as.list(rep(lty, length(coords.list))), 
                  FUN = function(x, y, lty) {
                    # (2) this is linked to (1)
                    if (plot.se == TRUE) {
                      plot(x, add = TRUE, lwd = y, lty = lty)
                    } else {
                      plot(x, add = TRUE, lty = lty)
                    }
                  })
         })
  
  return(list(coords, map))
}