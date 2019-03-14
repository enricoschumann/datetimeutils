\name{nth_day}
\alias{nth_day}
\title{
  Compute Reference Dates
}
\description{

  Compute sequences of reference dates, such as last
  day of month or first day of quarter.

}
\usage{
nth_day(timestamps, period = "month", n,
        start, end, business.days = FALSE, 
        missing = "previous", index = FALSE) 
}
\arguments{
  \item{timestamps}{

    timestamps: a sorted vector of Dates

  }
  \item{period}{

    numeric or character: supported are \code{"week"},
    \code{"month"}, \code{"quarter"},
    \code{"halfyear"}, \code{"year"}.  If numeric,
    period is interpreted as a month number, with
    January being 1.  Also possible are month names,
    either English as in \code{\link{month.name}} or
    \code{\link{month.abb}}, or as defined in the
    current locale (see \code{\link{strftime}} format
    specification \code{"\%b"} and \code{"\%B"}).

  }
  \item{n}{

    numeric or character: currently supported are
    \code{"first"} and \code{"last"}. If numeric, it
    will be interpreted as the \emph{n}-th day of the
    period.

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
    character. Not supported yet.
  }
  \item{index}{
    logical. If \code{TRUE}, the indices (instead of
    actual timestamps) are returned.
    
  }
}
\details{

  The function computes sequences of dates that are
  often used as reference dates, for instance in
  financial reporting: last day of the month or of the
  year, or a particular day of the month.
  
  The function takes a vector of timestamps and returns
  a subset of these timestamps. Alternatively, a
  sequence of calendar days may be constructed by
  specifying \code{start} and \code{end}.

}
\value{
  
  A vector of timestamps or, if \code{index} is
  \code{TRUE}, a vector of integers.

}
\author{
  Enrico Schumann
}
\seealso{

  \code{\link{nth_weekday}}
  
}
\examples{
timestamps <- seq(from = as.Date("2001-01-01"),
                  to   = as.Date("2001-04-15"),
                  by   = "1 day")

nth_day(timestamps, period = "quarter", n = "last")
## [1] "2001-03-31" "2001-04-15"

nth_day(timestamps, period = "quarter", n = 10)
## [1] "2001-01-10" "2001-04-10"

nth_day(timestamps, period = "quarter", n = 1:2)
## [1] "2001-01-01" "2001-01-02" "2001-04-01" "2001-04-02"

nth_day(timestamps, period = "month", n = "last")
## [1] "2001-01-31" "2001-02-28" "2001-03-31" "2001-04-15"


nth_day(start = as.Date("2016-06-03"),
        end   = as.Date("2017-08-01"),
        period = c(6, 12), n = 3)
## [1] "2016-06-05" "2016-12-03" "2017-06-03"

nth_day(start = as.Date("2016-06-03"),
        end   = as.Date("2017-08-01"),
        period = c("Jun", "Dec"), n = c(3, 5))
## [1] "2016-06-05" "2016-06-07" "2016-12-03" "2016-12-05"
## [5] "2017-06-03" "2017-06-05"

}
\keyword{chron}
