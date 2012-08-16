\name{xtablelm}
\alias{xtablelm}
\title{Produces the output of an lm object as it appears in the R console when you
type summary(lmobject)}
\usage{
  xtablelm(lm.object, titref, labname, extracaption = NULL)
}
\arguments{
  \item{lm.object}{the name of your linear model object
  that you want to make a summary table for.}

  \item{titref}{the label name of the equation you made in
  Latex to cross reference}

  \item{labname}{the label name you want for this table}

  \item{extracaption}{adds whatever text string you pass to
  the title of the table.}
}
\value{
  xtable object
}
\description{
  Produces the output of an lm object as it appears in the
  R console when you type summary(lmobject)
}
\examples{
##
}
\seealso{
  xtable
}

