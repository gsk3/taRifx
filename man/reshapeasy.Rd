\name{reshapeasy}
\alias{reshapeasy}
\title{reshapeasy: Easier reshaping from "wide" to "long" and back again}
\usage{
  reshapeasy(data, direction,
    id = (sapply(data, is.factor) | sapply(data, is.character)),
    vary = sapply(data, is.numeric), omit = c("_", "."),
    vars = NULL, ...)
}
\arguments{
  \item{data}{A data.frame to be reshaped}

  \item{direction}{"wide" or "long"}

  \item{vars}{he names of the (stubs of) the variables to
  be reshaped (if omitted, defaults to everything not in id
  or vary)}

  \item{id}{The names of the variables that identify unique
  observations}

  \item{vary}{he variable that varies.  Going to wide this
  variable will cease to exist.  Going to long it will be
  created.}

  \item{omit}{vector of characters which are to be omitted
  if found at the end of variable names (e.g. price_1
  becomes price in long)}

  \item{...}{Options to be passed to stats::reshape}
}
\value{
  A data.frame
}
\description{
  reshapeasy is a wrapper around base R's reshape which
  allows for saner syntax. In particular, it makes it
  possible to reverse the operation by only specifying that
  the direction change (e.g. the names of the arguments are
  consistent between the direction of reshaping).
}
\author{
  Written with the help of the StackOverflow R community,
  see
  http://stackoverflow.com/questions/10055602/wrapping-base-r-reshape-for-ease-of-use
}

