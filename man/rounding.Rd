\name{roundnear}
\alias{round_sigfig}
\alias{roundnear}
\title{Rounds a numeric vector to arbitrary values (not just decimal values as with
round) or to a specified number of significant digits.}
\usage{
  roundnear(vec, roundvec)

  round_sigfig(vec, digits = 2)
}
\arguments{
  \item{vec}{numeric vector}

  \item{roundvec}{What value to round things to (e.g.
  nearest 1, 10, 0.3, etc.).  Typically a single item to
  apply to all of vec.  If of length greater than 1, usual
  wrapping rules apply.}

  \item{digits}{Number of significant digits to round to}
}
\value{
  Rounded numeric vector of length length(vec)
}
\description{
  Rounds a numeric vector to arbitrary values (not just
  decimal values as with round).  E.g. allows you to round
  to nearest 0.3 instead of just nearest 1 or 0.1
}
\examples{
roundnear( runif(10) , .03 )
}
\references{
  http://en.wikipedia.org/wiki/Significant_figures
}

