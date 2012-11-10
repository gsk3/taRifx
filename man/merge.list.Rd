\name{merge.list}
\alias{merge.list}
\title{Method to merge two lists
Matches names of each list element and combines any sub-elements}
\usage{
  \method{merge}{list} (x, y, ...)
}
\arguments{
  \item{x}{First list}

  \item{y}{Second list}

  \item{\dots}{Other arguments}
}
\value{
  A list
}
\description{
  Method to merge two lists Matches names of each list
  element and combines any sub-elements
}
\examples{
x <- list( A=list(p=runif(5)), B=list(q=runif(5)) )
y <- list( A=list(r=runif(5)), C=list(s=runif(5)) )
merge.list(x,y)
}

