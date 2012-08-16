\name{middle.group}
\alias{middle.group}
\title{Return a vector containing the locations of the middle of every group in a
vector, either as a numerical index or as a TRUE/FALSE boolean.}
\usage{
  middle.group(vec, type = "tf")
}
\arguments{
  \item{vec}{Any vector which you want to know the middle
  of.}

  \item{type}{Either "tf" to return a boolean or "loc" to
  return a set of numerical locations.}
}
\value{
  If type=="tf": Boolean of length length(vec) containing
  TRUE if the middle of a grouping and FALSE if not.  If
  type=="loc": Vector of length equal to the number of
  groups in vec, containing locations of the group centers.
  Ties (for groups of even length) are broken by rounding
  up.
}
\description{
  This function uses run length encoding to determine the
  middle of every group of repeated values within a larger
  vector.
}
\examples{
test <- c(1,2,2,2,2,2,2,2,2,2,1)
middle.group(test)
middle.group(test,type="loc")
}

