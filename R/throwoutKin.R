#' Function finds non related individuals.
#' 
#' @param animal1 Character or factor vector of individuals.
#' @param animal2 Character or factor vector of individuals.
#' @param stat Numeric vector. Statistic based on which individuals will be purged.
#' 


kin <- read.table("./sandbox/RelatednessCI95.Txt", sep = ",")

th <- 0.125
animal <- kin[, c(2, 3, 6)]
names(animal) <- c("animal1", "animal2", "stat")

master <- animal
counter <- 0
ua <- as.character(unique(master$animal2)) # unique animals

while (length(ua) > 0) {
#   browser()
  counter <- counter + 1
  x <- master[master$animal1 %in% ua[1], ]
  ua <- ua[-1]
  x <- x[x$stat >= th, ]
  
  master <- master[!master$animal1 %in% unique(x$animal2), ]
  
  print(paste("animal1:", length(unique(master$animal1)), "| animal2:", length(unique(master$animal2)), "| nrow:", nrow(master)))
}


throwoutKin <- function(animals, treshold) {
  
}