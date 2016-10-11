\name{isWeekend}
\alias{isWeekend}
\alias{isBusinessDay}
\alias{nextBusinessDay}
\alias{nextBDay}
\alias{previousBusinessDay}
\alias{prevBDay}


\title{
  Business Days
}
\description{
  
  Check whether a timestamp of class \code{\link{Date}} or
  \code{\link{POSIXt}} is a business day.
  
}
\usage{
isWeekend(x)
isBusinessDay(x, holidays = NULL)
previousBusinessDay(x, holidays = NULL, shift = -1)
prevBDay(x, holidays = NULL, shift = -1)
nextBusinessDay(x, holidays = NULL, shift = 1)
nextBDay(x, holidays = NULL, shift = 1)

}
\arguments{

  \item{x}{
    a vector of class \code{Date} or \code{POSIXct}
  }
  \item{holidays}{
    
    A vector of class \code{\link{Date}}, or a character vector in a
    format that is understood by \code{\link{as.Date}}, or anything that
    can be coerced to class \code{Date} by \code{\link{as.Date}} (e.g.
    \code{\link{POSIXt}}).
    
  }
  \item{shift}{
    integer
  }
}
\details{

  \code{isWeekend} checks whether a given date is a Saturday or Sunday.

  \code{previousBusinessDay} takes a \code{\link{Date}} \code{x} and
  returns the last non-weekend day before. When \code{shift} is less
  than -1, the function evaluates to the \code{shift}-th previous
  day. When \code{shift} is 0, the function will return \code{x} if it
  is a non-weekend day, else the previous business
  day. \code{nextBusinessDay} works analogously. There are shorter-named
  versions \code{nextBDay} and \code{prevBDay}.

}
\value{
  
  Logical.

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
}
\examples{
isWeekend(Sys.Date())
previousBusinessDay(Sys.Date())
nextBusinessDay(Sys.Date())

}
\keyword{chron}
