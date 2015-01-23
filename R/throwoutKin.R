#' Function finds non related individuals.
#' 
#' For each individual, remove kin based on treshold value (see Blouin 2003). Data comes from
#' software \code{coancestry} (Julian Wang).
#' 
#' @param data \code{data.frame} A \code{data.frame} with columns (in this order) \code{animal1},
#' \code{animal2} and \code{stat}.
#' @param treshold \code{numeric} A numeric vector of length one. Individuals with comparison
#' value of >= \code{treshold} will be removed from the data set.
#' @author Roman Lustrik, Maja Jelencic
#' 
#' @examples
#' \dontrun{
#'   kin <- read.table("./sandbox/RelatednessCI95.Txt", sep = ",")
#'   animal <- kin[, c(2, 3, 6)]
#'   names(animal) <- c("animal1", "animal2", "stat")
#' 
#'   suppressMessages(rez <- throwoutKin(data = animal, treshold = 0.5))
#'   str(rez)
#'   length(rez$unique.animals) # number of unique animals
#'   str(rez$cleaned.data) # structure of cleaned data
#'   hist(rez$cleaned.data$stat) # histogram of stat values from cleaned data
#'                               # should all be below treshold
#' }

throwoutKin <- function(data, treshold) {
  ua <- sort(unique(c(as.character(data$animal1), as.character(data$animal2)))) # unique animals
  out <- character()
  
  while (length(ua) > 0) {
    # subset data for one individual
    x.all <- data[data$animal1 == ua[1] | data$animal2 == ua[1], ]
    x.sub <- x.all[x.all$stat >= th, ] # and find which comparisons are >= threshold
    
    if (nrow(x.sub) > 0) {
      out <- c(out, ua[1])
      all.unique <- unique(c(as.character(x.sub$animal1), as.character(x.sub$animal2)))
      ua <- ua[-which(ua %in% all.unique)] # remove current and related individuals
      all.unique <- all.unique[!all.unique %in% ua[1]] # all animals related to ua[1]
      data <- data[!(data$animal1 %in% all.unique | data$animal2 %in% all.unique), ]
    } else {
      out <- c(out, ua[1])
      ua <- ua[-1]
    }
    message(length(ua))
  }
  
  out <- sub("D_", "", out) # remove "D_"
  list(unique.animals = out, cleaned.data = master)
}


