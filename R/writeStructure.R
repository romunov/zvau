#' Function writes a genind object to a text file
#' 
#' Function will convert a genind file and population designation to a text file.
#' 
#' @author Tomaz Skrbinsek {tomaz.skrbinsek@@gmail.com}
#' 
#' @param genotypes genind A \code{genind} object to be exported.
#' @param pop Vector A numeric vector which corresponds to population.
#' @param path Character A character vector of relative or absolute path of the resulting file.
#' 
#' @return Text file
 
writeStructure <- function(genotipi, pop, path) {
  gen <- genind2df(genotipi, sep = " ")
  gen[is.na(gen)] <- "-9 -9"
  output <- cbind(id = row.names(gen), pop = as.numeric(pop), gen[2:ncol(gen)])
  
  write.table(output, path, sep = " ", row.names = FALSE, col.names = FALSE, 
              append = TRUE, quote = FALSE)  #,eol='\r')
  
  outLegend <- data.frame(id = 1:length(levels(pop)), Pop = levels(pop))
  outLegend
}