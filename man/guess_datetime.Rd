\name{guess_datetime}
\alias{guess_datetime}
\title{
  Guess Timestamp Format
}
\description{
  Tries to convert a character vector to POSIXct.
}
\usage{
guess_datetime(s)
}
\arguments{
  \item{s}{
    character
}
}
\details{
  
}
\value{
  \code{\link{POSIXct}}
}
\references{
  \code{\link{strptime}}
}
\author{
  Enrico Schumann
}
\note{
%%  ~~further notes~~
}

\section{Warning }{

  If you know the format of a timestamp, then \bold{do not use this function}
  (use strptime instead). If you have no idea at all about the format of
  a timestamp, then \bold{do not use this function}.

}

\seealso{

  See package anytime.
  
}
\examples{
s <- c("  1999-08-19     10:00:31   ",
       "1999-08-19 10:00",
       "19.8.1999 10:00",
       "8/19/99  10:00:31",
       "8/19/1999 10:00:31",
       "19.8.1999 10:00:31")

guess_datetime(s)
}
\keyword{chron}
