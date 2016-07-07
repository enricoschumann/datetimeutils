\name{isWeekend}
\alias{isWeekend}
\title{
  Check if Date is Weekend
}
\description{
  
  Check whether a timestamp of class \code{\link{Date}} or
  \code{\link{POSIXt}} is a Saturday or Sunday.
  
}
\usage{

isWeekend(x)
}
\arguments{

  \item{x}{
    a vector of class \code{Date} or \code{POSIXct}
  }
}
\details{

  Check whether a given date is a Saturday or Sunday.
  
}
\value{
  
  Logical.

}
\references{

  B.D. Ripley and K. Hornik. \emph{Date-Time Classes}. R-News,
  \strong{1}(2):8--12, 2001.

}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{DateTimeClasses}}, \code{\link[PMwR]{plotTradingHours}}
}
\examples{
isWeekend(Sys.Date())
}
\keyword{chron}
