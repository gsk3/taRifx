\name{munch}
\alias{munch}
\title{Recursively delete entries containing `what` before entry pointed to by `which`}
\usage{
  munch(x, wch, what = "")
}
\arguments{
  \item{x}{data vector}

  \item{wch}{Vector of indices to check preceding element
  for `what`}

  \item{what}{What to check for and delete if found in
  preceding element}
}
\value{
  A vector of the same type as x with all the `what`'s
  removed if they were at the `which`-(1,2,3...) locations
}
\description{
  Recursively delete entries containing `what` before entry
  pointed to by `which`
}
\examples{
x <- c("a","","b","","","","","c","d","","","","e","")
munch( x, c(3,8,9,13) )
}

