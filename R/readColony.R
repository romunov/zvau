#' Function will read in result from Colony.
#' 
#' Specifying the project folder, function will find the desired result and import it into a
#' R specific data structure.
#' 
#' @param proj Character. A full path to the Colony project folder.
#' @param comp Character. Result from colony. See Details.
#' 
#' @section Details:
#' \begin{itemize}
#' \item \code{"LC"} will extract result from \code{*.BestClone}.
#' \item \code{"LR"} (not implemented yet) will extract results from \code{*.PairwiseCloneDyad}. 
#' This will be empty if relatedness wasn't calculated.
#' \item \code{offspring_genotype} will extract genotype of offspring from \code{OffspringGenotype.txt} of the project.
#' \end{itemize}
#' 
#' @author Roman Lustrik (\email{roman.lustrik@@biolitika.si})
#' @export

readColony <- function(proj, comp = c("LC", "LR", "offspring_genotype")) {
  # proj <- "d:/colony/volkovi"
  if (comp == "LC") {
    result <- list.files(proj, pattern = ".BestClone")
    result <- read.table(file.path(proj, result), header = TRUE)
    
    names(result) <- c("CloneIndex", "Prob", "Members")
    prepare.x <- apply(result[, c("CloneIndex", "Prob")], MARGIN = 1, FUN = function(x) list(x))
    prepare.y <- strsplit(as.character(result$Members), split =  ",")
    result <- mapply(FUN = function(x, y) {
      x <- unlist(x)
      suppressWarnings(out <- data.frame(clone.index = x["CloneIndex"], prob = x["Prob"], sample.name = y))
      rownames(out) <- NULL
      out
    }, x = prepare.x, y = prepare.y, SIMPLIFY = FALSE)
    result <- do.call("rbind", result)
    result$clone.index <- as.factor(result$clone.index)
  }
  
  if (comp == "offspring_genotype") {
    result <- read.table(file.path(proj, "OffspringGenotype.txt"), header = TRUE)
    colnames(result)[grepl("Offspring", colnames(result))] <- c("sample.name")
  }
  result
}