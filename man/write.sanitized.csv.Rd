\name{write.sanitized.csv}
\alias{write.sanitized.csv}
\title{Outputs a sanitized CSV file for fussy input systems e.g. ArcGIS and Mechanical Turk
Performs three cleansing actions: converts text to latin1 encoding, eliminates funny characters in column names, and writes a CSV without the leading row.names column}
\usage{
  write.sanitized.csv(x, file = "", ...)
}
\arguments{
  \item{x}{The data.frame to clean and write}

  \item{file}{The filename to write to}

  \item{\dots}{Arguments to pass to write.csv}
}
\value{
  NULL
}
\description{
  Outputs a sanitized CSV file for fussy input systems e.g.
  ArcGIS and Mechanical Turk Performs three cleansing
  actions: converts text to latin1 encoding, eliminates
  funny characters in column names, and writes a CSV
  without the leading row.names column
}

