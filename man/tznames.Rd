\name{tznames}
\alias{tznames}
\docType{data}
\title{
  Timezone Names
}
\description{

  A mapping between \code{tz} database (a.k.a. Olson database)
  and Windows timezone names.

}
\usage{data("tznames")}
\format{
  A data frame of the following 2 variables:
  \describe{

    \item{\code{Windows}}{a character vector: the timezone names used
      under Windows and its applications (e.g. in Outlook calendars)}

    \item{\code{Olson}}{a character vector of the names returned by
      \code{\link{OlsonNames}}}

 }
}
\source{

  Unicode Common Locale Data Repository (CLDR) \url{https://cldr.unicode.org/}

}
\details{

  The data are auto-generated from file \code{windowsZones.xml} in the
  Unicode Common Locale Data Repository
  (\url{https://cldr.unicode.org/}).  See
  \url{https://www.unicode.org/copyright.html} and
  \url{https://www.unicode.org/license.html} for the terms of use.

  There is no 1-to-1 mapping between names: several Olson names
  typically map to a single  Windows name.

}
\references{

  See \url{https://www.iana.org/time-zones}
  and \url{https://web.cs.ucla.edu/~eggert/tz/tz-link.htm} for more
  information about the \code{tz} database.

  See also \code{\link{OlsonNames}}.

  A plain-text table is at
  \url{https://github.com/enricoschumann/datetimeutils/blob/master/data/tznames.txt}

}
\examples{
str(tznames)
}
\keyword{datasets}
