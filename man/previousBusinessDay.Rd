\name{dayOfMonth}
\alias{dayOfMonth<-}
\alias{dayOfMonth}
\alias{mday<-}
\alias{mday}
\alias{endOfMonth}
\alias{endOfYear}
\alias{endOfPreviousMonth}
\alias{firstOfMonth}
\alias{firstOfYear}
\alias{isLeapyear}
\title{
  Handling and Manipulating Dates and Times
}
\description{
  
  Functions for manipulating vectors that inherit from class
  \code{POSIXt} or \code{Date}.  If times are returned, they are
  typically of class \code{POSIXct}.

}
\usage{
isLeapyear(x)
firstOfMonth(x)
firstOfYear(x)
endOfMonth(x, shift = 0L)
endOfYear(x, shift = 0L)
endOfPreviousMonth(x)
dayOfMonth(x)
mday(x)
dayOfMonth(x) <- value
mday(x) <- value
}
\arguments{

  \item{x}{
    a vector of class \code{Date}
  }
  \item{value}{
    a vector of integers
  }
  \item{shift}{
    integer
  }
}
\details{


  \code{endOfMonth} returns the last calendar day of a given month. If
  \code{shift} is positive, then shift \code{months} into the future; if
  negative, the end of previous months. (\code{endOfMonth(x, -1)} is
  equivalent to \code{endOfPreviousMonth(x)}.
  
  \code{mday} is a wrapper for \code{dayOfMonth}.
  
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
## vectorisation: x *or* shift (but not both)
endOfMonth(as.Date("2013-01-15"), shift = c(-1, 0, 1))
endOfMonth(as.Date("2013-01-15") + 0:100)


dayOfMonth(d <- as.Date("2013-01-22"))
dayOfMonth(d) <- 5
d
}
\keyword{chron}
