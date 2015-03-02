#' Draw loci and its corresponding alleles.
#' 
#' This function will take a list of loci alleles and plot it, where y axis represents loci and x axis represents allele "length". Function
#' assumes allele names are coercable to numeric.
#' 
#' @param x List. Named list where names correspond to true loci names. Each element should hold unique allele values. At this time
#' they are assumed to be coercable to integers.
#' @export
#' @title Draw alleles for loci list.
#' @author Roman Lustrik (\email{roman.lustrik@@biolitika.si})
#' @examples 
#' # generate data
#' require(magrittr)
#' require(ggplot2)
#' al <- lapply(1:10, FUN = function(x) {
#'       runif(n = sample(x = 2:7, size = 1), min = 100, max = 300) %>%
#'       round()
#'   })
#' 
#' # add names to the list
#' names(al) <- paste("L", 1:length(al), sep = "")
#' 
#' fig.plotted <- drawLoci(al)

# TODO: coerce alleles to ordered list

drawLoci <- function(x) {
  al.list <- mapply(x, 1:length(x), FUN = function(x, y) {
    data.frame(allele = y, x)
  }, SIMPLIFY = FALSE)
  
  out <- do.call("rbind", al.list)
  
  out$allele <- as.factor(out$allele)
  out$x <- as.numeric(as.character(out$x))
  
  print(
    go <- ggplot(out, aes_string(x = "x", y = "allele")) +
      theme_bw() +
      geom_line(color = "grey50") + 
      geom_point(size = 3, shape = 1) +
      scale_y_discrete(label = names(al.list)) +
      theme(
        panel.grid.major.y = element_line(color = "grey95")
      )
  )
  
  return(list(data = out, fig = go))
}