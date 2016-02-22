\name{timegrid}
\alias{timegrid}
\title{
  POSIXct Time Grid
}
\description{
  Build an equally-spaced sequence of POSIXct timestamps.
}
\usage{
timegrid(from, to, interval, excludeWeekends = TRUE, holidays = NULL,
         fromHHMMSS = "080000", toHHMMSS = "220000")
}
\arguments{
  \item{from}{
    a vector of length one that inherits from class \code{\link{POSIXt}}
  }
  \item{to}{
    a vector of length one that inherits from class \code{\link{POSIXt}}
  }
  \item{interval}{
    
    A character string like \dQuote{\code{num} \code{units}}, in which
    \code{num} is a number, and \code{units} is \code{sec}, \code{min},
    \code{hour} or \code{day}. \code{num} and \code{units} must be
    separated by white space.

  }
  \item{excludeWeekends}{
    logical; default is \code{TRUE}
  }
  \item{fromHHMMSS}{
    
    A character vector of length one like
    \dQuote{\code{HHMMSS}}. Times-of-day earlier than \code{HHMMSS} are
    excluded from the grid.

  }
  \item{toHHMMSS}{

    A character vector of length one like
    \dQuote{\code{HHMMSS}}. Times-of-day later than \code{HHMMSS} are
    excluded from the grid.

  }
  \item{holidays}{

    A vector of class \code{\link{Date}}, or a character vector in a format that
    is understood by \code{\link{as.Date}}, or anything that can be
    coerced to class \code{Date} by \code{\link{as.Date}} (eg, \code{\link{POSIXt}}).

  }

}
\details{

  \code{timegrid} creates an equally-spaced grid of class \code{POSIXct}.
  
}
\value{
  a vector of class \code{\link{POSIXct}} (or a character vector of
  length zero, in case no valid points remain)
}
\author{
  Enrico Schumann
}
\references{
  \url{https://www.ietf.org/rfc/rfc0822.txt}

  \url{http://www.rssboard.org/rss-specification}
}
\seealso{
  \code{\link{strftime}}, \code{\link{date}}
}
\examples{
from <- as.POSIXct("2012-04-30 08:00:00")
to   <- as.POSIXct("2012-05-04 22:00:00")
res  <- timegrid(from, to, interval = "1 hour",
                 holidays = as.Date("2012-05-01"))
}
