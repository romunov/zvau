#' Calculate sliding window cohorts
#' 
#' In case you need to know years of cohorts, enter start date, sliding window size and number
#' of cohorts. Result is a visualization of from-to years of each cohort. This is a supporting
#' function to \code{genindSlidingWindow} function.
#' 
#' @param start Integer. Year of start of sliding window.
#' @param win.size Integer. Size of sliding window.
#' @param cohorts Integer. Number of cohorts (sliding window steps).
#' 
#' @return A printout to console of cohort years.
#' 
#' @author Roman Lustrik (\email{roman.lustrik@@biolitika.si})
#' 
#' @export
#' 
#' @examples 
#' goSlide(start = 1996, win.size = 4, cohorts = 14)
goSlide <- function(start, win.size, cohorts) {
  for(i in 1:cohorts) {
    message(paste("Cohort:", i, paste(rep(" ", i-1), collapse = ""), start, start + (win.size +1 )))
    start <- start + 1
  }
}

