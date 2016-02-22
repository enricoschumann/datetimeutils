\name{lastWeekday}
\alias{dayOfMonth<-}
\alias{dayOfMonth}
\alias{mday<-}
\alias{mday}
\alias{endOfMonth}
\alias{endOfYear}
\alias{endOfPreviousMonth}
\alias{firstOfMonth}
\alias{isWeekend}
\alias{isLeapyear}
\alias{nthWeekday}
\alias{lastWeekday}
\alias{nextBusinessDay}
\alias{previousBusinessDay}
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
nextBusinessDay(x, holidays = NULL, shift = 1)
isWeekend(x)
isLeapyear(x)
firstOfMonth(x)
endOfMonth(x, shift = 0L)
endOfYear(x, shift = 0L)
endOfPreviousMonth(x)
dayOfMonth(x)
dayOfMonth(x) <- value
lastWeekday(weekday, x, shift = 0L,
            period = "month", before, inclusive = TRUE)
nthWeekday(weekday, x, n = 1L)
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
  \item{weekday}{
    an integer (\code{1} is Monday, \code{2} is Tuesday, and so on)
  }
  \item{period}{
    character. 
  }
  \item{before}{
    a Date. See also \code{inclusive}.
  }
  \item{inclusive}{
    logical. Is \code{before} meant is \sQuote{before but including}? 
  }
  \item{n}{
    an integer
  }
}
\details{

  \code{previousBusinessDay} takes a \code{\link{Date}} \code{x} and
  returns the last non-weekend day before. When \code{shift} is less
  than -1, the function evaluates to the \code{shift}-th previous
  day. When \code{shift} is 0, the function will return \code{x} if
  it is a non-weekend day, else the previous business
  day. \code{nextBusinessDay} works analogously.

  \code{endOfMonth} returns the last calendar day of a given month. If
  \code{shift} is positive, then shift \code{months} into the future; if
  negative, the end of previous months. (\code{endOfMonth(x, -1)} is
  equivalent to \code{endOfPreviousMonth(x)}.
  
  \code{lastWeekday} gives the last day of the week (specified as an
  integer 0 to 6, with Sunday being 0) of a given month.  \code{shift}
  moves forward (when positive) or backward (when negative) by one week;
  see Examples.

  \code{mday} is a wrapper for \code{dayOfMonth}.
  
}
\value{
  
  Vectors of class \code{Date} or \code{POSIXct}; \code{isWeekend}
  returns \code{TRUE} or \code{FALSE}.

}
\references{

  B.D. Ripley and K. Hornik. \emph{Date-Time Classes}. R-News,
  \strong{1}(2):8--12, 2001.

}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{DateTimeClasses}}, \code{\link[PMwR]{plotTradingHours}}

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

## GOAL    find the third Friday in March 2013
## HOW TO  find the last Friday in February 2013 and 
##         shift forward by 3 weeks
lastWeekday(5, as.Date("2013-02-01"), shift = 3)

## ... or much simpler
nthWeekday(5, as.Date("2013-03-01"), 3)

}
\keyword{chron}
