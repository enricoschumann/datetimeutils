\name{nth_day}
\alias{nth_day}
\title{
  Compute Reference Dates
}
\description{

  Compute sequences of reference dates, such as last day of month or
  first day of quarter.

}
\usage{
nth_day(timestamps, period = "month", n,
        start, end, business.days = FALSE, 
        missing = "previous", index = FALSE) 
}
\arguments{
  \item{timestamps}{
    timestamps
  }
  \item{period}{
    character
  }
  \item{n}{
    see Details and Examples
  }
  \item{start}{
    \code{\link{Date}}
  }
  \item{end}{
    \code{\link{Date}}
  }
  \item{business.days}{
    logical
  }
  \item{missing}{
    character
  }
  \item{index}{
    logical
  }
}
\details{

  Experimental.
  
}
\value{

  A vector of timestamps or, if \code{index} is \code{TRUE}, a vector of
  integers.

}
\author{
  Enrico Schumann
}
\seealso{

  \code{\link{nth_weekday}}
  
}
\examples{
timestamps <- seq(from = as.Date("2001-01-01"),
                  to   = as.Date("2001-4-15"),
                  by   = "1 day")

nth_day(timestamps, period = "quarter", n = "last")
## [1] "2001-03-31" "2001-04-15"

nth_day(timestamps, period = "month", n = "last")
## [1] "2001-01-31" "2001-02-28" "2001-03-31" "2001-04-15"

}
\keyword{chron}
