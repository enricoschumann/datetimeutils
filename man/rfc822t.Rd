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
  prepending an abbreviated day-of-week. The function ignores the
  current locale: day-of-week and month names are in English. The
  format is required for timestamps in RSS feeds.

}
\value{
  a character vector
}
\author{
  Enrico Schumann
}
\references{
  \url{https://www.rfc-editor.org/rfc/rfc822.txt}

  \url{https://www.rssboard.org/rss-specification}
}
\seealso{
  \code{\link{strftime}}, \code{\link{date}}
}
\examples{
rfc822t(Sys.time())
}
