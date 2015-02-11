#' Write genind object to text files
#' 
#' Function promises to write two genind objects to a text file.
#' 
#' @author Tomaz Skrbinsek {tomaz.skrbinsek@@gmail.com}
#' @param genotypes1 genind A \code{genind} object.
#' @param genotypes2 genind A \code{genind} object
#' @param path Character A relative or absolute path where file is written to.
#' @param note Character A character vector of length 1 reserved for notes.
#' 
#' @importFrom adegenet locNames
#' @importFrom adegenet genind2df
#' @importFrom adegenet indNames
#'
#' @export
#' 
#' @return Text file

writeGenePop <- function(genotypes1, genotypes2, path, note) {
  
  # writes geneind as genepop file
  write.table(note, path, quote = FALSE, col.names = FALSE, row.names = FALSE, sep = "")
  write.table(paste(c(locNames(genotypes1), "POP"), sep = "\n"), path, col.names = FALSE, 
              row.names = FALSE, sep = "", 
              quote = FALSE, append = TRUE)
  outgenotypes <- genind2df(genotypes1, sep = "")
  outgenotypes[is.na(outgenotypes)] <- "000000"
  names <- indNames(genotypes1)
  names <- paste(names, ",", sep = "")
  outgenotypes <- cbind(names, outgenotypes[2:ncol(outgenotypes)])
  
  write.table(outgenotypes, path, sep = " ", row.names = FALSE, col.names = FALSE, 
              append = T, quote = FALSE)
  
  # second object
  cat("POP\n", file = path, append = TRUE)
  outgenotypes <- genind2df(genotypes2, sep = "")
  outgenotypes[is.na(outgenotypes)] <- "000000"
  names <- indNames(genotypes2)
  names <- paste(names, ",", sep = "")
  outgenotypes <- cbind(names, outgenotypes[2:ncol(outgenotypes)])
  
  write.table(outgenotypes, path, sep = " ", row.names = FALSE, col.names = FALSE, 
              append = TRUE, quote = FALSE)
}