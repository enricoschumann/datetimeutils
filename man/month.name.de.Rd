\name{month.name.de}
\alias{month.name.de}
\alias{month.abb.de.din1355.1}
\docType{data}
\title{
  Non-English Month Names and Abbreviations
}
\description{

  Month names and abbreviations in languages other than
  English: Currently only German is supported.

}
\usage{
month.name.de
month.abb.de.din1355.1
}
\format{
  Character vectors; encoded as \acronym{UTF}-8 if necessary.
}
\details{
  Character vectors, encoded as \acronym{UTF}-8.

  \code{month.abb.de.din1355.1} contains the
  abbreviations of the withdrawn \acronym{DIN} 1355-1,
  which uses \dQuote{Mrz} for March.

}
\source{
  \url{https://de.wikipedia.org/wiki/DIN_1355-1}
}
\references{
  \url{https://de.wikipedia.org/wiki/DIN_1355-1}
}
\examples{
month.name.de
month.name.de[month(Sys.Date())]
}
\keyword{datasets}
