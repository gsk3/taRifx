\name{readdir}
\alias{readdir}
\title{Loads all readable files in a directory into a list, with names according to the filenames}
\usage{
  readdir(path, exclude = "",
    filename.as.variable = "filename", stack = FALSE)
}
\arguments{
  \item{path}{is the directory path}

  \item{exclude}{is a regular expression. Matching
  filenames will be excluded}

  \item{filename.as.variable}{is a variable name to store
  the filename.  "" means it will not be stored.}

  \item{stack}{if true attempts to stack the resultant
  data.frames together into a single data.frame}
}
\value{
  A list of data.frames or a single data.frame
}
\description{
  Loads all readable files in a directory into a list, with
  names according to the filenames
}

