\name{japply}
\alias{japply}
\title{japply: Judiciously sapply to only selected columns}
\usage{
  japply(df, sel, FUN = function(x) x, ...)
}
\arguments{
  \item{df}{data.frame}

  \item{sel}{A logical vector or vector of column numbers
  to select}

  \item{FUN}{The function to apply to selected columns}

  \item{\dots}{Pass-alongs to sapply}
}
\value{
  A data.frame
}
\description{
  japply is a wrapper around sapply that only sapplys to
  certain columns
}

