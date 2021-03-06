% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/findIntegerInterval.R
\name{findIntegerInterval}
\alias{findIntegerInterval}
\title{Find interval of sequential integers}
\usage{
findIntegerInterval(x)
}
\arguments{
\item{x}{Vector.}
}
\description{
Function will find a sequence of integers and create a factor for
each repeat.
}
\examples{
x <- data.frame(orig = c(0,0,0, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4,
       0,0,0,0,1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,5,5,5,
       0,0,0,0,1,1,1,1,2,2,2,2,3,4,5,6,6,6,6,6,6,7,7,7,7,7,8,8,8,8))
x$group <- findIntegerInterval(x$orig)
x
}
\author{
Roman Lustrik (\email{roman.lustrik@biolitika.si})
}
