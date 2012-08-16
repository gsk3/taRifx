\name{rep_along}
\alias{rep_along}
\title{Repeat a vector until it matches the length of another vector}
\usage{
  rep_along(x, along.with)
}
\arguments{
  \item{x}{Vector to be repeated}

  \item{along.with}{Vector whose length to match}
}
\value{
  A vector of same type as x
}
\description{
  Repeat a vector until it matches the length of another
  vector
}
\examples{
rep_along(1:4,letters)
}

