\name{latex.table.by}
\alias{latex.table.by}
\title{Exports a latex table with the first N columns being multirow grouping
variables.}
\usage{
  latex.table.by(df, num.by.vars = 1, ...)
}
\arguments{
  \item{df}{data.frame with first num.by.vars columns being
  grouping variables}

  \item{num.by.vars}{Number of columns to interpret as
  grouping vars}

  \item{\dots}{Other arguments to pass to xtable}
}
\value{
  A modified xtable object.
}
\description{
  Given a data.frame with the first N columns of grouping
  variables, makes each group print nicely in a LaTeX
  table.
}
\examples{
my.test.df <- data.frame(grp=rep(c("A","B"),each=10),data=runif(20))
library(xtable)
latex.table.by(my.test.df)
#   print(latex.table.by(test.df), include.rownames = FALSE, include.colnames = TRUE, sanitize.text.function = force)
#   then add \\usepackage{multirow} to the preamble of your LaTeX document
#   for longtable support, add ,tabular.environment='longtable' to the print command (plus add in ,floating=FALSE), then \\usepackage{longtable} to the LaTeX preamble
}
\seealso{
  xtable, bytable
}

