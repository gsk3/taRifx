\name{evens}
\alias{evens}
\alias{odds}
\title{Shortcut functions to return the odd and even values from a vector}
\usage{
  evens(vec)
}
\arguments{
  \item{vec}{Integer vector}
}
\value{
  Returns an integer vector consisting of only the odd/even
  elements.
}
\description{
  Takes an integer vector and returns every odd or even
  element
}
\examples{
x <- as.integer(c(6,3,4,7,8,1047482,7))
evens(x)
odds(x)
}

