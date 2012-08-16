\name{between}
\alias{between}
\alias{bin}
\alias{quantile_cutpoints}
\title{Classify values into groups based on which numbers they're between}
\usage{
  between(vec, cutpoints)

  bin(vec, n = 10)

  quantile_cutpoints(vec, probs)
}
\arguments{
  \item{vec}{Numeric vector to classify}

  \item{cutpoints}{Vector listing what values the grouping
  should be done on. Should include the max and the min in
  this list as well.}

  \item{n}{Number of groups to bin into}

  \item{probs}{Probabilities at which to create cutpoints}
}
\value{
  Vector of length(vec) indicating which group each element
  is in (for between). Or vector of length(vec) indicating
  the lower bound of the group that it's in.
}
\description{
  Classify values into groups based on which numbers
  they're between. quantile.cutpoints creates a data.frame
  of quantiles for feeding into e.g. categorize()
}
\examples{
test <- runif(100)
between(test,c(0,.1,.5,.9,1))
bin(test,n=5)
}
\seealso{
  categorize
}

