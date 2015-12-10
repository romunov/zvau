#' Function writes a genind object to a text file
#'
#' Function will convert a genind file and population designation to a text file.
#'
#' @author Tomaz Skrbinsek (\email{tomaz.skrbinsek@@gmail.com}) and Roman Lustrik (\email{roman.lustrik@@biolitika.si})
#'
#' @param x genind A \code{genind} object to be exported.
#' @param file Character A character vector of relative or absolute path of the resulting file.
#' @param ... Arguments passed to \code{write.table}.
#'
#' @importFrom adegenet genind2df
#' @export
#'
#' @return Writese a text file to a specified path or current \code{getwd()}. Returns
#' a population legend if population is designated. Otherwise returns a message notifying
#' the user legend is not applicable.

writeStructure <- function(x, file, ...) {
  # Explicitly remove population.
  gen <- genind2df(x, sep = " ", usepop = FALSE)
  gen[is.na(gen)] <- "-9 -9"

  # If there is no population designation, assume only one population.
  if (is.null(pop(x))) {
    pop <- rep(1, times = nrow(gen))
  } else {
    pop <- as.numeric(pop(x))
  }

  output <- cbind(id = row.names(gen), pop = pop, gen)
  write.table(output, file, sep = " ", row.names = FALSE, quote = FALSE)

  if (!is.null(pop(x))) {
    out <- data.frame(id = 1:length(unique(pop)), Pop = unique(pop(x)))
  } else {
    message("No population defined, one population assumed, no legend produced.")
  }
}
