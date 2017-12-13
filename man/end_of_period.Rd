\name{end_of_period}
\alias{day_of_month<-}
\alias{day_of_month}
\alias{mday<-}
\alias{mday}
\alias{end_of_month}
\alias{end_of_year}
\alias{end_of_previous_year}
\alias{end_of_previous_month}
\alias{first_of_month}
\alias{first_of_year}
\alias{is_leapyear}
\alias{month}
\alias{year}
\title{
  Handling and Manipulating Dates and Times
}
\description{
  
  Functions for manipulating vectors that inherit from class
  \code{POSIXt} or \code{Date}.

}
\usage{
is_leapyear(x)

first_of_month(x)
end_of_month(x, shift = 0L)
end_of_previous_month(x)

first_of_year(x)
end_of_year(x, shift = 0L)
end_of_previous_year(x)

day_of_month(x)
day_of_month(x) <- value
mday(x)
mday(x) <- value

month(x, as.character = FALSE)
year(x, as.character = FALSE)

}
\arguments{

  \item{x}{
    a vector of class \code{Date}
  }
  \item{value}{
    a vector of integers
  }
  \item{shift}{
    integer
  }
  \item{as.character}{
    logical
  }
}
\details{


  \code{end_of_month} returns the last calendar day of a given month. If
  \code{shift} is positive, then shift \code{months} into the future; if
  negative, the end of previous months. \code{end_of_month(x, -1)} is
  equivalent to \code{end_of_previous_month(x)}. \code{end_of_year}
  works in the same way, but for calendar years.

  
  \code{mday} is a wrapper for \code{day_of_month}.
  
}
\value{
  
  Vectors of class \code{Date} or \code{POSIXct}; or \code{logical}

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

  Many useful functions are also in package \pkg{chron}.
}
\examples{
## vectorisation: x *or* shift (but not both)
end_of_month(as.Date("2013-01-15"), shift = c(-1, 0, 1))
end_of_month(as.Date("2013-01-15") + 0:100)


day_of_month(d <- as.Date("2013-01-22"))
day_of_month(d) <- 5
d
}
\keyword{chron}
