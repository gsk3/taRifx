\name{shift}
\alias{shift}
\alias{shift.data.frame}
\alias{shift.default}
\title{Shifts a vector's elements left or right by N elements.}
\usage{
  shift(x, ...)

  \method{shift}{default} (x, n = 1, wrap = TRUE,
    pad = FALSE, ...)

  \method{shift}{data.frame} (x, ...)
}
\arguments{
  \item{x}{A vector to be operated on}

  \item{n}{Number of rows to shift by (if negative, shift
  to right instead of left)}

  \item{wrap}{Whether to wrap elements or not (adds the
  entry at the beginning to the end)}

  \item{pad}{Whether to pad with NAs or not.  pad does
  nothing unless wrap is false, in which case it specifies
  whether to pad with NAs}

  \item{\dots}{Other items to pass along}
}
\value{
  vector of the same type as vec
}
\description{
  Shifts a vector's elements left or right by N elements.
}
\examples{
test <- seq(10)
shift(test)
}

