\name{fpart}
\alias{fpart}
\title{Obtain the fractional part of a numeric}
\usage{
  fpart(vec)
}
\arguments{
  \item{vec}{A numeric vector of any length}
}
\value{
  A vector of the same length as the input vec containing
  only the decimal component.
}
\description{
  Takes a numeric vector and returns a vector of the
  numbers after the decimal place
}
\examples{
x <- runif(100)
fpart(x)
}

