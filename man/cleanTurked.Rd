\name{cleanTurked}
\alias{cleanTurked}
\title{Function to restore Mechanical Turk output to its original state (plus the
added columns)}
\usage{
  cleanTurked(dat, drop.duplicates = TRUE,
    post.process = identity)
}
\arguments{
  \item{dat}{A data.frame}

  \item{drop.duplicates}{Has three modes of operation: 1.
  If false, no duplicates are dropped. 2. If true or
  character use built-in function (drop only if all (TRUE)
  or the listed (character) "Answer." columns identical).
  3. Or can be a user-defined function that takes in a
  data.frame and returns a logical vector corresponding to
  whether to drop a row (TRUE) or not (FALSE).}

  \item{post.process}{A user-defined function run on the
  data.frame after it is cleaned}
}
\value{
  A data.frame
}
\description{
  Takes Amazon Mechanical Turk output, (optionally)
  deduplicates it, and returns the file to the state where
  you uploaded it, plus the additional columns derived from
  Turking.
}

