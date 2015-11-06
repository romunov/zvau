#' Function will write \code{genind} object to Coancestry file format.
#'
#' Using \code{genind} object, function will try hard to format and write the data
#' so that it will be accepted by Coancestry program (http://www.zsl.org/science/software/coancestry).
#'
#' @param gi A \code{genind} object to be written to a file.
#' @param file Character. A file name of the created file.
#'
#' @author Roman Lustrik (\email{roman.lustrik@@biolitika.si})
#' @return A file is written to disk. Rows (individuals) are sorted according to the population quietly.
#' @importFrom adegenet alleles pop genind2df

#' @export

writeCoancestry <- function(gi, file) {

    # Explicitly remove population.
    gen <- genind2df(gi, sep = " ", usepop = FALSE)
    allele.len <- unique(nchar(unlist(alleles(gi))))

    # Find allele length and construct a fake genotype of missing values. In this case, zeros.
    # This only works for diploids.
    gen[is.na(gen)] <- paste(rep(paste(rep("0", allele.len), collapse = ""), 2), collapse = " ")

    # If there is no population designation, assume only one population.
    if (is.null(pop(gi))) {
        gi.pop <- rep(1, times = nrow(gen))
    } else {
        gi.pop <- pop(gi)
    }

    ind.names <- paste(gi.pop, row.names(gen), sep = "_")
    output <- cbind(id = ind.names, gen)
    output <- output[order(pop(gi)), ]
    write.table(output, file, sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)

    output
}
