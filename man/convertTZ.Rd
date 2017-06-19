\name{convert_tz}
\alias{convert_tz}
\title{
  Convert a Timestamp from one Timezone to Another
}
\description{
  Convert a timestamp from one timezone to another.
}
\usage{
convert_tz(datetime, from = "", to)
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

  Be careful: if the specified timezone does not exist on your
  system, the function will \bold{not} return an error.
  
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

convert_tz("2016-05-10 12:00:00",
          "America/Chicago", "America/Chicago")

convert_tz("2016-05-10 12:00:00",
          "Europe/Berlin", "America/Chicago")

convert_tz(Sys.time(), to = "Europe/London")
convert_tz(Sys.time(), to = "America/Chicago")


}