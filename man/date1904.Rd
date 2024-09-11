\name{date1904}
\alias{date1904}
\title{
    Is File Origin 1904?
}
\description{
    Checks whether an xlsx file uses 1 Jan 1904 as its origin.
}
\usage{
date1904(filename)
}
\arguments{
  \item{filename}{%
    character: one or more filenames
  }
}
\details{

  Requires \code{utils::unzip}.

}
\value{
  A logical vector:
  \code{TRUE} if origin is 1904;
  \code{FALSE} if origin is 1900;
  \code{NA} if file could not be processed.
}
\author{
  Enrico Schumann
}
\references{
  ECMA-376-1:2016 \emph{Office Open XML File Formats}.
}
\seealso{
  \code{\link{convert_date}}
}
\examples{
\donttest{
date1904("~/Desktop/02_company_statistics.pdf")
}
}
