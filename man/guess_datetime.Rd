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

  The function first coerces its argument to character.
  It then applies a list of patterns to each element of
  \code{s}. Let \code{d} be a numeric digit; then the
  rules are roughly those in the table below. (For the
  precise rules, see Examples below.)

  \tabular{ll}{
    \emph{original pattern}     \tab \emph{assumed format}          \cr
    \code{dddd-dd-dd dd:dd:dd}  \tab \code{\%Y-\%m-\%d \%H:\%M:\%S} \cr
    \code{dd/dd/dddd dd:dd:dd}  \tab \code{\%m/\%d/\%Y \%H:\%M:\%S} \cr
    \code{dd.dd.dddd dd:dd:dd}  \tab \code{\%d.\%m.\%Y \%H:\%M:\%S} 
  }

  The rules are followed in the given order; an element
  will be matched only once. If there is a match,
  \code{\link{strptime}} will be tried with the \emph{assumed
    format}.

  For elements that do not match any pattern or for
  which \code{\link{strptime}} fails, \code{\link{NA}} is returned.
  
}
\value{
  \code{\link{POSIXct}}
}
\author{
  Enrico Schumann
}
\section{Warning }{

  If you know the format of a timestamp, then \bold{do
  not use this function} (use \code{\link{strptime}}
  instead). If you have no idea at all about the format
  of a timestamp, then \bold{do not use this function}.

}

\seealso{

  \code{\link{strptime}}; see also the function
  \code{\link[anytime]{anytime}} in the package of the
  same name.
  
}
\examples{
s <- c("  1999-08-19     10:00:31   ",
       "   1999-08-19 10:00",
       "19.8.1999 10:00",
       "8/19/99      10:00:31",
       "8/19/1999 10:00:31",
       "19.8.1999 10:00:31")

guess_datetime(s)

\dontrun{## the actual rules
rules <- as.data.frame(matrix(datetimeutils:::.dt_patterns,
                              byrow = TRUE, ncol = 2),
                       stringsAsFactors = FALSE)
names(rules) <- c("pattern", "assumed_format")
rules

}
}
\keyword{chron}
