\name{lastWeekday}
\alias{lastWeekday}
\alias{nthWeekday}
\title{
  Functions for Computing Days of the Week
}
\description{
  
  Functions for computing a specifed day-of-week, such as \sQuote{the
  last Friday of October 2015}.
  
}
\usage{
lastWeekday(weekday, x, shift = 0L,
            period = "month", before, inclusive = TRUE)
nthWeekday(weekday, x, n = 1L)
}
\arguments{
  \item{x}{
    a vector of class \code{Date}
  }
  \item{shift}{
    a vector of integers
  }
  \item{weekday}{
    an integer (\code{1} is Monday, \code{2} is Tuesday, and so on)
  }
  \item{period}{
    character. 
  }
  \item{before}{
    a \code{\link{Date}}. See also \code{inclusive}.
  }
  \item{inclusive}{
    logical. Is \code{before} meant is \sQuote{before but including}? 
  }
  \item{n}{
    an integer
  }
}
\details{

  \code{lastWeekday} computes the last day-of-the-week
  (specified as an integer 0 to 6, with Sunday being 0)
  in a given month, e.g. \sQuote{the last Friday}.
  \code{shift} moves forward (when positive) or
  backward (when negative) by one week; see Examples.

  \code{nthWeekday} gives the \emph{n}-th
  day-of-the-week (specified as an integer 0 to 6, with
  Sunday being 0) of a given month, e.g. \sQuote{the
  second Monday}.

}
\value{
  
  Vectors of class \code{Date} or \code{POSIXct}; or \code{logical}

}
\references{

  B.D. Ripley and K. Hornik. \emph{Date-Time Classes}. R-News,
  \strong{1}(2):8--12, 2001.

}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{DateTimeClasses}}

  Many useful functions are also in package \pkg{chron}.
}
\examples{
## GOAL:     find the third Friday in March 2013
## SOLUTION: find the last Friday in February 2013 and 
##           shift forward by 3 weeks
lastWeekday(5, as.Date("2013-02-01"), shift = 3)

## ... or much simpler
nthWeekday(5, as.Date("2013-03-01"), 3)

}
\keyword{chron}
