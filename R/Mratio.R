#' Calculate the Garza-Williamson M-ratio.
#' 
#' M-ratio indicates reduction in population size using data from microsatellite loci.
#' 
#' Formula for calculating for each locus is M = k/(R+1) where \code{k} is the number of alleles on that locus
#' and R is the range of microsatellite repeats. See Garza J.C., Williamson E.G. 2001. Detection of reduction 
#' in population size using data from microsatellite loci. Molecular Ecology, 10: 305-318 for more information.
#'
#' @param x A \code{genind} object.
#' @return A named vector of loci with M ratio.
#' @export
#' 
#' @author Roman Lustrik (roman.lustrik@@biolitika.si)
#' @examples
#' library(adegenet)
#' data(nancycats)
#' Mratio(nancycats)

Mratio <- function(x) {
  sapply(alleles(x), FUN = function(m) {
    k <- length(m)
    num.m <- as.numeric(m)
    rn.m <- range(num.m)
    R <- diff(rn.m)
    k/(R + 1)
  })
}