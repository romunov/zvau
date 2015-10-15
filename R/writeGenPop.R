#' Write \code{adegenet}'s \code{genind} object to GENEPOP format
#'
#' Function will write \code{adegenet}'s \code{genind} object to GENEPOP as defined at
#' \url{http://genepop.curtin.edu.au/help_input.html}. First line is a comment, second line is loci names and the rest consists
#' of lines "pop" and genotype names and their allele values on corresponding loci. This function differs from
#' \code{\link{writeGenePop}}, relies on defined \code{strata} and is more general.
#'
#' @param gi A \code{\link[adegenet]{genind}} object with defined strata and population. See \code{\link[adegenet]{setPop}}.
#' @param file.name A character string defining file name. You will have to specify the extension as well.
#' @param comment A character string with a desired comment that will be prepended to the beginning of the file.
#' @return A file on disk, function return NULL to R.
#'
#' @importFrom adegenet pop locNames indNames genind2df popNames
#' @export
#' @author Roman Lustrik (\email{roman.lustrik@@biolitika.si})
#' @title Write genind as GENEPOP file
#'
#' @examples
#' library(adegenet)
#' data(nancycats)
#' # Genind object must have a population defined. This is trivial with strata.
#' other(nancycats)$pop <- sample(letters[1:4], size = nInd(nancycats), replace = TRUE)
#' strata(nancycats) <- data.frame(pop = other(nancycats)$pop)
#' setPop(nancycats) <- ~pop
#'
#' # write the file and remove it
#' writeGenPop(nancycats, file.name = "test.gen", comment = "is a no comment")
#' file.remove("test.gen")

writeGenPop <- function(gi, file.name, comment) {

    # calculate the length of a string to be able to specify number of zeros (as NA)
    lng <- unique(apply(na.omit(sapply(genind2df(gi)[, locNames(gi)], as.character))[, 1, drop = FALSE], MARGIN = 2, nchar))

    cat(paste(comment, "\n"), file = file.name)
    cat(paste(paste(locNames(gi), collapse = ", "), "\n"), file = file.name, append = TRUE)
    for (i in popNames(gi)) {
      browser()
        cat("pop\n", file = file.name, append = TRUE)
        intm <- gi[pop(gi) == i, drop = FALSE]
        ind.names <- indNames(intm)
        intm <- genind2df(intm, sep = "")
        intm[is.na(intm)] <- paste(rep("0", lng), collapse = "")
        out <- cbind(names = paste(ind.names, ",", sep = ""), intm[, locNames(gi)])
        write.table(out, file = file.name, row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
    }

    return(NULL)
}

