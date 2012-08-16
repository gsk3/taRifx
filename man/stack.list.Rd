\name{stack.list}
\alias{stack.list}
\title{Stack lists of data.frames}
\usage{
  \method{stack}{list} (x, label = FALSE, ...)
}
\arguments{
  \item{x}{A list of rbindable objects (typically
  data.frames)}

  \item{label}{If false, drops labels}

  \item{\dots}{Ignored}
}
\value{
  A ggplot2 plot
}
\description{
  Method of stack for lists of data.frames (e.g. from
  replicate() )
}

