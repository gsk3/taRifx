\name{searchPattern}
\alias{searchPattern}
\title{Create a vector that starts with a given number and widens out}
\usage{
  searchPattern(center = 0, length = 5, interval = 1)
}
\arguments{
  \item{center}{Number to center search pattern around}

  \item{length}{Number of elements in search pattern}

  \item{interval}{Distance between each element}
}
\value{
  numeric vector
}
\description{
  Create a vector that starts with a given number and
  widens out
}
\examples{
library(gdata)
searchPattern()
}

