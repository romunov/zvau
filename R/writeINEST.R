#' Write genind object as INEST file
#' 
#' @param x \code{genind} object
#' @param file Character vector. Path where file is written to.
#' @param spatial Logical. If TRUE, spatial coordinates of samples will be written to file.
#' @author Roman Lustrik (roman.lustrik@@biolitika.si)
#' @examples
#' 
#' library(adegenet)
#' data(nancycats)
#' writeINEST(nancycats, file = "inest_test.txt", spatial = FALSE)
#' unlink("inest_test.txt")

writeINEST <- function(x, file, spatial = FALSE) {
  # write header, number of individuals, number of loci, if spatial coordinates
  if (spatial == TRUE) {
    sp <- 1
    xy <- other(x)$xy %>% as.matrix() %>% unname()
  } else {
    sp <- 0
  }
  write(paste(nInd(x), "\t", nLoc(x), "\t", sp), file = file)
  
  # write marker names
  write(locNames(x), file = file, append = TRUE)
  
  # convert alleles on each locus to unique number
  gdf <- genind2df(x, sep = " ", usepop = FALSE)
  gs <- sapply(gdf, FUN = function(g) {
    out <- strsplit(g, " ")
    out.na <- do.call("rbind", lapply(out, FUN = function(x) all(is.na(x))))
    out[out.na] <- sapply(out[out.na], FUN = function(x) c(NA, NA), simplify = FALSE)
    
    out <- out %>% unlist() %>% as.factor() %>% as.integer()
    out[is.na(out)] <- 0
    out <- matrix(out, ncol = 2, byrow = TRUE)
  }, simplify = FALSE)
  gene <- do.call("cbind", gs)
  
  # add spatial component, if applicapble
  if (spatial == TRUE) {
    out <- cbind(xy, gene)
  } else {
    out <- gene
  }
  
  # add animal ID
  # if coersion to numeric is not possible, add consecutive numbers as IDs
  if (any(is.na(as.numeric(indNames(x))))) {
    mm <- 1:nInd(x)
    out <- cbind(mm, out)
  } else {
    out <- cbind(as.numeric(indNames(x)), out)
  }
  
  write.table(out, file = file, append = TRUE,
              col.names = FALSE, row.names = FALSE,
              sep = "\t")
  out
}

