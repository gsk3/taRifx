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
  Typically a data.frame
}
\description{
  Method of stack for lists of data.frames (e.g. from
  replicate() )
}
\examples{
dat <- replicate(10, data.frame(x=runif(2),y=rnorm(2)), simplify=FALSE)
str(dat)
stack(dat)
}

