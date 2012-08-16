\name{hist_horiz}
\alias{hist_horiz}
\title{Kludgy horizontal histogram function (really should just fix the lattice
equivalent)}
\usage{
  hist_horiz(formula, data, n = 20)
}
\arguments{
  \item{formula}{Plot formula}

  \item{data}{Data.frame}

  \item{n}{Number of groups}
}
\value{
  plot
}
\description{
  Kludgy horizontal histogram function (really should just
  fix the lattice equivalent)
}
\examples{
library(lattice)
library(datasets)
hist_horiz(~ len | supp, data=ToothGrowth, n=5)
}
\seealso{
  hist
}

