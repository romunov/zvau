#' Read a Clumpp file
#' 
#' Function reads Clumpp file and outputs all the colums right of colon (:) as a \code{data.frame}.
#' 
#' @author Roman Lustrik {roman.lustrik@@biolitika.si}
#' 
#' @param x Character Character string to the text file produced by Clumpp. Path can be absolute or relative.
#' @return A data.frame with K columns (see Structure documentation for the meaning of K).
#' 
#' @export
#' 
#' @examples
#' x <- "1        1   (0)      2 :  0.0403 0.9597 
#' 2        2   (0)      2 :  0.0209 0.9791 
#' 3        3   (0)      2 :  0.0051 0.9949 
#' 4        4   (0)      2 :  0.0115 0.9885 
#' 5        5   (0)      3 :  0.0502 0.9498 
#' 6        6   (0)      3 :  0.0425 0.9575 
#' 7        7   (0)      3 :  0.0239 0.9761"
#' 
#' write(x, "temp.clumpp.txt")
#' readClumpp("temp.clumpp.txt")
#' unlink("temp.clumpp.txt")

readClumpp <- function(x) {
  x <- readLines(x)

  # extract individuals
  ind <- sapply(unname(sapply(x, function(x) strsplit(x = x, split = "\\("))), "[[", 1)
  ind <- strsplit(trimws(ind), "  ")
  ind <- sapply(ind, "[[", 1)
  
  # extract population
  # http://stackoverflow.com/questions/28267400/extract-a-string-of-words-between-two-specific-words-in-r
  pop <- sub(".*) *(.*?) *:.*", "\\1", x)
  
  # extract assignment values
  pis <- do.call("rbind", strsplit(unlist(lapply(strsplit(x, ":  "), "[[", 2)), " "))
  pis <- as.data.frame(apply(pis, MARGIN = 2, as.numeric))
  names(pis) <- paste("q", 1:ncol(pis), sep = "")
  
  cbind(ind, pop, pis)
}

