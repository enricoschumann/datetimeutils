\name{convert_date}
\alias{convert_date}
\title{
  Convert Various Formats to Date
}
\description{
  Convert dates in external formats (e.g. from MATLAB) to Date or POSIXct.
}
\usage{
convert_date(x, type, fraction = FALSE, tz = "")
}
\arguments{
  \item{x}{
    numeric
  }
  \item{type}{
    character: currently \code{"excel"} and \code{"matlab"} are supported.
  }
  \item{fraction}{
    logical: should fractional dates (i.e. times) be used?  Default is
    \code{FALSE}.
  }
  \item{tz}{
    character: if fraction is TRUE, then what time zone is to be
    assumed?  Default is \code{""}, i.e. the local time zone.
  }
}
\details{

  Convert the numeric representation of a date to class
  \code{\link{Date}}. For Excel, only the Windows format is supported.
  Note that dates before 1 March 1900 are probably wrong since Excel
  considers 1900 a leap year (which it is not).

}
\value{
  A vector of class \code{\link{Date}}, or \code{\link{POSIXct}} if
  fraction is \code{TRUE}.
}
\author{
  Enrico Schumann
}
\seealso{
\code{\link{as.Date}}
}
\examples{
convert_date(40000, "excel")
}
