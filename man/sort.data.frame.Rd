\name{sort.data.frame}
\alias{sort.data.frame}
\title{Sort a data.frame}
\usage{
  \method{sort}{data.frame} (x, decreasing = NULL, formula,
    ...)
}
\arguments{
  \item{x}{Data.frame to sort}

  \item{formula}{Formula by which to sort the data.frame
  (e.g. ~group1+group2 sorts first by group1 then by
  group2)}

  \item{decreasing}{Ignored.  Exists for compatibility with
  generic S3 method.}

  \item{\dots}{Used to pass ,drop=FALSE to [}
}
\value{
  Returns a sorted data.frame
}
\description{
  Sorts a data frame by one or more variables
}
\note{
  Modifications by Ari Friedman and Roman Lustrik Original
  Author: Kevin Wright
  http://tolstoy.newcastle.edu.au/R/help/04/09/4300.html
  Some ideas from Andy Liaw
  http://tolstoy.newcastle.edu.au/R/help/04/07/1076.html
  Use + for ascending, - for decending. Sorting is left to
  right in the formula

  If you are Kevin Wright, please contact me.  I have
  attempted to reach you by every means thinkable, to no
  avail.  My assumption is that this is in the public
  domain since you posted it for others to use, but please
  tell me if that is not the case.
}
\examples{
library(datasets)
sort.data.frame(ChickWeight,formula=~weight+Time)

mydf <- data.frame(col1 = runif(10))
rownames(mydf) <- paste("x", 1:10, sep = "")
sort(mydf, f = ~col1) # drops a dimension
sort(mydf, f = ~col1, drop = FALSE) # does not drop a dimension (returns a data.frame)
}
\author{
  Kevin Wright, with generic compatibility by Ari B.
  Friedman
}
\seealso{
  \link[plyr]{arrange}
}

