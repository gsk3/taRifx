\name{expandDF}
\alias{expandDF}
\alias{splitDF}
\alias{unsplitDF}
\title{Functions to manipulate data frames}
\usage{
  expandDF(df, obs, numtimes = 1)

  splitDF(df, splitvar)

  unsplitDF(splitdfs)
}
\arguments{
  \item{df}{Data.frame to be manipulated}

  \item{obs}{Vector to select rows of df (e.g. vector of
  row numbers or a boolean of length nrow(df) )}

  \item{numtimes}{Number of times to replicate}

  \item{splitvar}{Name of variable which defines groups on
  which df will be split}

  \item{splitdfs}{List of data.frames to recombine
  (generally created by splitDF)}
}
\value{
  expandDF and unsplitDF return a data.frame splitDF
  returns a list of data.frames
}
\description{
  expandDF takes a dataframe and replicates the chosen
  observations n times
}
\details{
  splitDF takes a dataframe and splits it into a bunch of
  data.frames held in a list, according to one variable

  unsplitDF takes a list of data.frames produced by splitDF
  and returns them as one appended data.frame
}
\examples{
library(datasets)
# Duplicate a dataset
expandDF(sleep,TRUE)
# Expand the final observation
expandDF(sleep,nrow(sleep),numtimes=10)
# Split a data.frame by group
s.df <- splitDF(sleep,'group')
s.df
# Reconstitute original data.frame
unsplitDF(s.df)
}

