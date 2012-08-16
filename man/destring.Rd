\name{destring}
\alias{destring}
\title{Convert character vector to numeric, ignoring irrelevant characters.}
\usage{
  destring(x, keep = "0-9.")
}
\arguments{
  \item{x}{A vector to be operated on}

  \item{keep}{Characters to keep in, in bracket regular
  expression form. Typically includes 0-9 as well as the
  decimal separator (. in the US and , in Europe).}
}
\value{
  vector of type numeric
}
\description{
  Convert character vector to numeric, ignoring irrelevant
  characters.
}
\examples{
test <- "50,762.83a"
destring(test)
}

