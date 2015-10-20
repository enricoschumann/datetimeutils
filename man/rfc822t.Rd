\name{rfc822t}
\alias{rfc822t}
\title{
  Format Date and Time as Described in RFC 822
}
\description{
  Format a timestamp as described in RFC 822.
}
\usage{
rfc822t(x, include.dow = TRUE)
}
\arguments{
  \item{x}{
    a vector that can be coerced to \code{\link{POSIXlt}}
  }
  \item{include.dow}{
    logical; include the day of the week?
  }
}
\details{

  Formats a timestamp as \sQuote{\code{\%Y \%H:\%M:\%S \%z}}, possibly
  prepending an abbreviated day-of-week. The function ignores locale
  settings; day-of-week or month names are all English. The format is
  required for timestamps in RSS feeds.
  
}
\value{
  a character vector
}
\author{
  Enrico Schumann
}
\references{
  \url{https://www.ietf.org/rfc/rfc0822.txt}
  \url{http://www.rssboard.org/rss-specification}
}
\seealso{
  \code{\link{strftime}}
}
\examples{
rfc822t(Sys.time())
}
