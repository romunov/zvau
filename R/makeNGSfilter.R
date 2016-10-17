#' Generate .ngsfilter file from a template
#' 
#' .ngsfilter file holds sample names and maps it to the appropriate tag-primer combination for each well on 
#' plate(s). Tag-primer combination for each well is stored in a template user provides. This template is 
#' appended by an experiment and sample names from "sheet" and accounted for the number of replicates.
#' 
#' @param sheet A data.frame with 8 rows and 12 columns. Each cell holds sample name. If file(path) is
#' provided, it assumes the data comes in an .xlsx file where the 8x12 frame starts in cell B4. 8x12 frame
#' should have numbers column names 1:12 from B3 to M3 and row names A:H from A4 to A11.
#' @param template A file (path) to the template where loci names, well position, tag and primers are defined.
#' See details for the layout.
#' @param experiment Character string denoting experiment name, e.g. UA_FrSlo. First two letters should be
#' species initials, followed by an underscore and followed by a project name. Locus name will be added to
#' this name, e.g. UA_FrSlo_03. The result should have exactly two underscores "_".
#' @param num.replicates Integer. Number of replicates.
#' @param filename Name (path) of the file to be created.
#' @return A data.frame and a side effect of creating the file.
#' 
#' @details 
#' `template` should have seven (7) columns, as described below. Notice the number of underscores.
#' \itemize{
#'    \item Experiment name, followed by locus name, e.g. \code{UA_FrSlo_03} where \code{UA_FrSlo} is name
#'    of the experiment and \code{03} is locus name.
#'    \item Sample name, well number and replicate, e.g. UASwd_001_R1 where UASwd is sample name, 001 is the
#'    first well A1 and R1 means replicate 1.
#'    \item Tag combination for given forward/reverse primer combination, e.g. \code{acacacac:acacacac}.
#'    \item Forward primer, e.g. \code{gctcccataactgcataaggtc}.
#'    \item Reverse primer, e.g. \code{ctggctggctggctagg}.
#'    \item Column for comments. We keep all values as \code{F}.
#'    \item Column where we keep all values as \code{@@}.
#' }
#' 
#' This function is the result of our visit to Grenoble (France) in collaboration with Marta de Barba and her 
#' colleagues (Laboratoire d’Écologie Alpine). Data is from paper by de Barba et al. (2016) and is directly 
#' suitable for the protocol described in the Supplemental materials of the above paper.
#' 
#' De Barba, M., Miquel, C., Lobréaux, S., Quenette, P. Y., Swenson, J. E., & Taberlet, P. (2016). 
#' High-throughput microsatellite genotyping in ecology: improved accuracy, efficiency, standardization 
#' and success with low-quantity and degraded DNA. Molecular Ecology Resources, 1–16. 
#' http://doi.org/10.1111/1755-0998.12594
#' 
#' 
#' @importFrom readxl read_excel
#' @importFrom tidyr gather
#' 
#' @author Roman Lustrik (roman.lustrik@@biolitika.si)
#' @export

makeNGSfilter <- function(sheet, template, experiment, num.replicates, 
                          sample.prefix = "sample", filename) {
  # browser()
  # Prepare the template
  tx <- read.table(template, colClasses = "character")
  
  # Find first sequence of loci. It assumes they are always in the same sequence.
  num.loci <- length(unique(tx[, 1]))
  
  loci <- gsub("^(.*?)_([[:digit:]]+)_([[:alnum:]]+$)", "\\2", tx[, 1])
  
  # Find the number of samples.
  samples <- gsub("^(.*?)_([[:digit:]]+)_([[:alnum:]]+$)", "\\2", tx[, 2])
  samples <- unique(samples)
  
  # Create experiment (1st) column
  onerun.experiment <- paste(experiment, loci, sep = "_")
  
  # Prepare the sheet
  if (!is.data.frame(sheet)) {
    require(readxl)
    xy <- read_excel(sheet, skip = 1)
    xy <- xy[1:which(xy[, 1] == "H"), ]
    
    # remove the firts column, which is row name (A:H)
    xy <- xy[, -1]
    
  } else {
    xy <- sheet
  }
  
  require(tidyr)
  # convert data to "long" format
  xy <- gather(xy, key = name, value = value)
  
  # use only the sample name column
  xy <- as.factor(xy$value)
  
  # There should be no underscores in the sample name due to
  # the way scripts process these columns down the line.
  xy <- gsub(pattern = "\\_", replacement = ".", x = xy)
  
  xy <- rep(xy, each = num.loci)
  
  # Create first (1st) column
  tx[, 1] <- onerun.experiment
  
  # Create sample name (2nd) column
  tx[, 2] <- paste(xy, tx[, 2], sep = "_")
  # Replicate the template appropriate number of times
  ntemplate <- do.call(rbind, replicate(num.replicates, tx, simplify = FALSE))
  
  # Implement experiment into the main data.frame
  R <- paste("R", 1:num.replicates, sep = "")
  R <- rep(R, each = length(onerun.experiment))
  
  ntemplate[, 2] <- paste(tx[, 2], R, sep = "_")
  
  # Write promise to file
  write.table(ntemplate, file = filename, row.names = FALSE, col.names = FALSE, quote = FALSE,
              sep = "\t")
  ntemplate
}