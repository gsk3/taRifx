\name{distinct}
\alias{distinct}
\title{Returns number of distinct observations in each column of a data frame or in
a vector}
\usage{
  distinct(input, na.rm = TRUE)
}
\arguments{
  \item{input}{data.frame or vector}

  \item{na.rm}{remove nas or not}
}
\value{
  Num of distinct obs
}
\description{
  Returns number of distinct observations in each column of
  a data frame or in a vector
}
\examples{
x <- sample(letters[1:3],10,replace=TRUE)
#distinct(x)
}

