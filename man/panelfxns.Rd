\name{panel.ecdf}
\alias{panel.densityplot.enhanced}
\alias{panel.ecdf}
\alias{panel.xyplot_rug}
\title{Various panel functions}
\usage{
  panel.ecdf(x, y, lines = TRUE, ...)

  panel.densityplot.enhanced(x, ...)

  panel.xyplot_rug(x, y, rug.color = "grey", ...)
}
\arguments{
  \item{x}{Numerical vector}

  \item{y}{Numerical vector}

  \item{lines}{Whether to connect the points with lines or
  not}

  \item{\dots}{Arguments to pass along to other lattice
  functions}

  \item{rug.color}{Color of rugplots}
}
\value{
  Lattice panel object
}
\description{
  panel.ecdf is a panel function for xyplot to create
  lattice plots of the empirical CDF.
  panel.densityplot.enhanced is a panel function for
  densityplot to add in descriptives as text.
  panel.xyplot_rug is an xyplot panel function with rug
  plots on x and y axes.
}

