\name{splitc}
\alias{splitc}
\alias{splitc-package}
\title{Split data over columns}
\usage{
  splitc(X, INDEX, FUN = NULL, ...)
}
\arguments{
  \item{X}{A \code{data.frame}, \code{matrix}, \code{array}
  or a \code{list}.}

  \item{INDEX}{A factor of \code{length(X)} (number of
  columns or list elements). If not a factor, it will be
  coerced into one.}

  \item{FUN}{A function to be applied to individual subset
  of data (each factor level). If not provided
  (\code{NULL}), raw (split) data is returned.}

  \item{\dots}{Additional arguments to \code{FUN}.}
}
\value{
  A list of the same length as there are factor levels in
  \code{INDEX}.
}
\description{
  Split data column-wise on \code{data.frame},
  \code{matrix} and \code{array} or element-wise on a
  \code{list}.
}
\details{
  Function splits a \code{data.frame}, \code{matrix} and
  \code{array} column-wise according to \code{INDEX} and
  \code{list} is sliced according to \code{INDEX}. Output
  is returned as a list of the same length as the number of
  levels in \code{INDEX}.
}
\note{
  Simplification sensu \code{tapply} is not yet
  implemented.
}
\examples{
my.list <- list(a = runif(5), b = runif(5), c = runif(5), d = runif(5), e = runif(10),
		f = runif(10), g = runif(10), h = runif(10), i = runif(10), j = runif(10))
my.df <- as.data.frame(my.list)
my.matrix <- as.matrix(my.df)

ind <- factor(c(1,1,1,1, 2,3, 4,4,4,4))
ind2 <- factor(c(1,1,1,1, 2,3, 4,4,4,4), levels = 1:5)

# applies mean to each, you must use \\code{colMeans} as \\code{mean} is deprecated for \\code{data.frame}s
splitc(X = my.df, INDEX = ind, FUN = colMeans)
splitc(X = my.matrix, INDEX = ind2) # level 5 empty because not populated
splitc(X = my.list, INDEX = ind, FUN = sum) # applied to elements INDEX-wise
}
\author{
  Roman Lustrik \email{roman.lustrik@gmail.com}
}
\seealso{
  \code{\link{tapply}}, \code{\link{by}},
  \code{\link{aggregate}}, \code{\link{apply}},
  \code{\link{split}}
}
\keyword{manip}

