\name{tznames}
\alias{tznames}
\docType{data}
\title{
  Timezone Names
}
\description{
  A mapping of Olson to Windows timezone names.
}
\usage{data("tznames")}
\format{
  A data frame with 596 observations on the following 2 variables.
  \describe{

    \item{\code{Windows}}{a character vector: the timezone names used
      under Windows and its applications (e.g. in Outlook calendars)}

    \item{\code{Olson}}{a character vector the names returned by
      \code{\link{OlsonNames}}}

 }
}
\details{

  The data are auto-generated from file '\code{windowsZones.xml}' in the
  Unicode Common Locale Data Repository
  (\url{http://cldr.unicode.org/}).  See
  \url{https://www.unicode.org/copyright.html} and
  \url{https://www.unicode.org/license.html} for the terms of use.

  There is no 1-to-1 mapping between names: several Olson names
  typically map to a single  Windows name.

  Column \code{Olson} contains the names returned by
  \code{\link{OlsonNames}}.

}
\source{

  Unicode Common Locale Data Repository (CLDR) \url{http://cldr.unicode.org/}

}
\references{

  See \url{https://www.iana.org/time-zones}
  and \url{http://web.cs.ucla.edu/~eggert/tz/tz-link.htm} for more
  information about the \code{tz} database.

  See also \code{\link{OlsonNames}}

}
\examples{
str(tznames)
}
\keyword{datasets}
