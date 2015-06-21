#' For a datum, find into which season it belongs to.
#' 
#' Based on definition of seasons by solstices and equinoxes (quinoxi?), find to which a particular date belongs to. 
#' Function is based on Josh O'Brien's answer from StackOverflow 
#' (http://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to).
#' @param x Datum of class Date.
#' @return Character of the season.
#' @export
#' @examples 
#' findSeason(as.Date("21.6.2015", format = "%d.%m.%Y"))

findSeason <- function(x) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(x, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}