\name{roundPOSIXt}
\alias{roundPOSIXt}
\title{
  Round POSIXt Objects to Specified Interval
}
\description{
  Round POSIXt objects to specified intervals such as \sQuote{5 minutes}.
}
\usage{
roundPOSIXt(t, interval, up = FALSE)
}
\arguments{
  \item{t}{
    
    a vector that inherits from class \code{\link{POSIXt}}
    
  }
  \item{interval}{
    
    A character string like \dQuote{\code{num} \code{units}}, in which
    \code{num} is a number, and \code{units} is \code{sec}, \code{min},
    \code{hour} or \code{day}. \code{num} and \code{units} must be
    separated by white space.
    
  }
  \item{up}{
    logical: round down (the default) or up?
}
}
\details{

  \code{roundPOSIXt} rounds an input of class \code{POSIXt}; it
  returns a vector of class \code{POSIXct}.

}
\value{
  Vectors of class \code{POSIXct}.
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
times <- as.POSIXct("2012-03-24 22:17:27") + 1:3
roundPOSIXt(times, "5 min")
roundPOSIXt(times, "5 min", TRUE)
}
\keyword{chron}
