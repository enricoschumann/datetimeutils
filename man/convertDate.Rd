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
    character: \code{"excel"}, \code{"excel1904"}, \code{"matlab"} and
    \code{"spss"}/\code{"pspp"} are supported.
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
  \code{\link{Date}}.  Note that different versions of Excel use
  different origins: 1900-01-01 or 1904-01-01. For the latter, set
  \code{type} to \code{"excel1904"}.
  For the former, \code{convert_date} uses 1899-12-31 because Excel
  considers 1900 a leap year (which it is not).
  So dates before 1 March 1900 are probably wrong (off by one day).

}
\value{
  A vector of class \code{\link{Date}}, or \code{\link{POSIXct}} if
  fraction is \code{TRUE}.
}
\author{
  Enrico Schumann; type \code{spss}/\code{pspp}
  suggested and based on a patch by J\"org Beyer
}
\seealso{
\code{\link{as.Date}}, \code{\link{as.POSIXlt}}
}
\examples{
convert_date(40000, "excel")
}
