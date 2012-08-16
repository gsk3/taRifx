\name{categorize}
\alias{categorize}
\title{Categorize a vector based on a data.frame with two columns, the low and high
end points of each category.}
\usage{
  categorize(vec, cutpoints.df, match.min = TRUE,
    names = TRUE)
}
\arguments{
  \item{vec}{vector to categorize}

  \item{cutpoints.df}{quantile_cutpoints will create a
  data.frame of the proper format here}

  \item{match.min}{Whether to include or exclude the
  minimum value}

  \item{names}{Return names or row numbers}
}
\value{
  Categorized values
}
\description{
  Categorize a vector based on a data.frame with two
  columns, the low and high end points of each category.
}
\seealso{
  \code{\link{quantile_cutpoints}}
}

