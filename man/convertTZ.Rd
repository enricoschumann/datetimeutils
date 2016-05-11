\name{convertTZ}
\alias{convertTZ}
\title{
  Convert Timestamps from one Timezone to another
}
\description{
  Convert a timestamp from one timezone to another.
}
\usage{
convertTZ(datetime, from = "", to)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datetime}{
    character: \code{YYYY-MM-DD HH:MM:SS}
}
  \item{from}{
    the timezone of datetime. If \code{""}, the local timezone is used.
}
  \item{to}{
    to timezone to which datetime should be converted
}
}
\details{
  See \code{\link{timezones}}.
}
\value{
  \code{\link{POSIXct}}
}
\references{

    B.D. Ripley and K. Hornik. \emph{Date-Time Classes}. R-News,
  \strong{1}(2):8--12, 2001.

}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{POSIXct}}
}
\examples{

convertTZ("2016-05-10 12:00:00",
          "America/Chicago", "America/Chicago")

convertTZ("2016-05-10 12:00:00",
          "Europe/Berlin", "America/Chicago")

convertTZ(Sys.time(), to = "Europe/London")
convertTZ(Sys.time(), to = "America/Chicago")


}