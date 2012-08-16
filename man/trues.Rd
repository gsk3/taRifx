\name{trues}
\alias{trues}
\title{Return vector of equal length containing all TRUEs}
\usage{
  trues(vec)
}
\arguments{
  \item{vec}{any vector (or valid object for \code{length}
  )}
}
\value{
  a vector of TRUEs of the length of the object passed to
  it
}
\description{
  Takes a vector and returns a vector of equal length
  containing all trues (used for selecting all of a given
  vector)
}
\examples{
x <- runif(100)
trues(x)
}
\keyword{TRUE}
\keyword{boolean}

