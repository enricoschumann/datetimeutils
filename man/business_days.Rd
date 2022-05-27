\name{business_days}
\alias{is_weekend}
\alias{is_businessday}
\alias{next_businessday}
\alias{next_bday}
\alias{previous_businessday}
\alias{prev_bday}


\title{
  Business Days
}
\description{

  Check whether a timestamp of class \code{\link{Date}} or
  \code{\link{POSIXt}} is a business day; compute past or future
  business days.

}
\usage{
is_businessday(x, holidays = NULL)
is_weekend(x)
previous_businessday(x, holidays = NULL, shift = -1)
prev_bday(x, holidays = NULL, shift = -1)
next_businessday(x, holidays = NULL, shift = 1)
next_bday(x, holidays = NULL, shift = 1)

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

  \code{is_weekend} checks whether a given date is a Saturday or Sunday.

  \code{previous_businessday} takes a \code{\link{Date}} \code{x} and
  returns the last non-weekend day before. When \code{shift} is less
  than -1, the function evaluates to the \code{shift}-th previous
  day. When \code{shift} is 0, the function will return \code{x} if it
  is a business day, else the previous business
  day. \code{next_businessday} works analogously. There are shorter-named
  versions \code{next_bday} and \code{prev_bday}.

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
is_weekend(Sys.Date())
previous_businessday(Sys.Date())
next_businessday(Sys.Date())

}
\keyword{chron}
