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
\alias{isBusinessDay}
\alias{nextBusinessDay}
\alias{nextBDay}
\alias{previousBusinessDay}
\alias{prevBDay}
\title{
  Handling and Manipulating Dates and Times
}
\description{
  
  Functions for manipulating vectors that inherit from class
  \code{POSIXt} or \code{Date}.  If times are returned, they are
  typically of class \code{POSIXct}.

}
\usage{
previousBusinessDay(x, holidays = NULL, shift = -1)
prevBDay(x, holidays = NULL, shift = -1)
nextBusinessDay(x, holidays = NULL, shift = 1)
nextBDay(x, holidays = NULL, shift = 1)

isLeapyear(x)
isBusinessDay(x, holidays = NULL)
firstOfMonth(x)
firstOfYear(x)
endOfMonth(x, shift = 0L)
endOfYear(x, shift = 0L)
endOfPreviousMonth(x)
dayOfMonth(x)
dayOfMonth(x) <- value
}
\arguments{

  \item{holidays}{

    A vector of class \code{\link{Date}}, or a character vector in a
    format that is understood by \code{\link{as.Date}}, or anything that
    can be coerced to class \code{Date} by \code{\link{as.Date}} (eg,
    \code{\link{POSIXt}}).

  }
  \item{x}{
    a vector of class \code{Date}
  }
  \item{value}{
    a vector of integers
  }
  \item{shift}{
    a vector of integers
  }
}
\details{

  \code{previousBusinessDay} takes a \code{\link{Date}} \code{x} and
  returns the last non-weekend day before. When \code{shift} is less
  than -1, the function evaluates to the \code{shift}-th previous
  day. When \code{shift} is 0, the function will return \code{x} if it
  is a non-weekend day, else the previous business
  day. \code{nextBusinessDay} works analogously. There are shorter-named
  versions \code{nextBDay} and \code{prevBDay}.

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

## businessdays
previousBusinessDay(Sys.Date())
nextBusinessDay(Sys.Date())
firstOfMonth(Sys.Date())

## vectorisation
endOfMonth(as.Date("2013-01-15"), shift = c(-1, 0, 1))
endOfMonth(as.Date("2013-01-15") + 0:100)

dayOfMonth(d <- as.Date("2013-01-22"))
dayOfMonth(d) <- 5
d

}
\keyword{chron}
