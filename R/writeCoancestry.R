#' Function will write \code{genind} object to Coancestry file format.
#'
#' Using \code{genind} object, function will try hard to format and write the data
#' so that it will be accepted by Coancestry program (http://www.zsl.org/science/software/coancestry).
#' Function can write a list of \code{genind} objects, in which case, each list element will be
#' considered a population. In that case, user should provide a vector (factor) designating individual
#' affiliation.
#'
#' @param gi A \code{genind} object to be written to a file.
#' @param file Character. A file name of the created file.
#' @param pop.inside Factor. If NULL (default), \code{pop(x)} is used to find population. If user wishes
#' to use a different population designation, he or she should provide a factor.
#'
#' @author Roman Lustrik (\email{roman.lustrik@@biolitika.si})
#'
#' @return A file is written to disk. Result will be quietly sorted according to the population.
#'
#' @importFrom adegenet alleles pop genind2df

#' @export

writeCoancestry <- function(gi, file, pop.inside = NULL) {

    if (is.list(gi) & is.null(pop.inside))  {
        stop("If you are providing a list of genind objects, you should specify pop.inside.")
    }

    if (!is.list(gi)) {
        gi <- list(gi)
    }

    gi.df <- sapply(gi, FUN = function(x) {
        # Explicitly remove population.
        gen <- genind2df(x, sep = " ", usepop = FALSE)
        allele.len <- unique(nchar(unlist(alleles(x))))

        # Find allele length and construct a fake genotype of missing values. In this case, zeros.
        # This only works for diploids.
        gen[is.na(gen)] <- paste(rep(paste(rep("0", allele.len), collapse = ""), 2), collapse = " ")
        gen
    }, simplify = FALSE)

    gi.df <- do.call("rbind", gi.df)
    ind.names <- do.call("c", sapply(gi, indNames, simplify = FALSE))

    # If there is no population designation, assume only one population.
    if (length(gi) == 1 & is.null(pop.inside)) {
        gi.pop <- sapply(gi[1], pop, simplify = FALSE)[[1]]
    } else {
        gi.pop <- pop.inside
    }

    ind.names <- paste(gi.pop, ind.names, sep = "_")
    output <- cbind(id = ind.names, gi.df)
    output <- output[order(gi.pop), ]
    write.table(output, file, sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)

    output
}
