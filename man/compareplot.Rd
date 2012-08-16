\name{compareplot}
\alias{compareplot}
\title{Bar plot divided by three groupings}
\usage{
  compareplot(formula, data.frame, show.outlines = FALSE,
    main = "", x.label = "", div.axis.major = 10,
    div.axis.minor = 20, log.x = FALSE,
    colors.plot = c("salmon", "mediumblue", "olivedrab", "cyan", "brown", "darkgreen", "purple"),
    panel = "panel.tuftebox", box.width.large.scale = 0.4,
    box.width.small.scale = 0.25, box.show.mean = TRUE,
    box.show.box = FALSE, box.show.whiskers = FALSE, ...)
}
\arguments{
  \item{formula}{Plot formula.  Of the form:
  ~cts|group1*group2*group3 , where cts is the continuous
  data you want to make boxplots out of, and group_ are
  factors to group by in descending heirarchical order.}

  \item{data.frame}{Data.frame containing data}

  \item{show.outlines}{Whether to include boxes around
  plots or leave it open}

  \item{main}{Plot text}

  \item{x.label}{X axis label}

  \item{div.axis.major}{How many major axis ticks to use}

  \item{div.axis.minor}{How many minor axis ticks to use}

  \item{log.x}{Log transform the x data?}

  \item{colors.plot}{Plot colors}

  \item{panel}{Panel function to use}

  \item{box.width.large.scale}{%% ~~Describe
  \code{box.width.large.scale} here~~}

  \item{box.width.small.scale}{%% ~~Describe
  \code{box.width.small.scale} here~~}

  \item{box.show.mean}{%% ~~Describe \code{box.show.mean}
  here~~}

  \item{box.show.box}{%% ~~Describe \code{box.show.box}
  here~~}

  \item{box.show.whiskers}{%% ~~Describe
  \code{box.show.whiskers} here~~}

  \item{\dots}{Other arguments to pass to lattice function}
}
\value{
  Plot
}
\description{
  Bar plot divided by three groupings
}
\examples{
library(datasets)
cw <- transform(ChickWeight,
 Time = cut(ChickWeight$Time,4)
 )
cw$Chick <- as.factor( sample(LETTERS[seq(3)], nrow(cw), replace=TRUE) )
levels(cw$Diet) <- c("Low Fat","Hi Fat","Low Prot.","Hi Prot.")
compareplot(~weight | Diet * Time * Chick,
 data.frame=cw ,
 main = "Chick Weights",
 box.show.mean=FALSE,
 box.show.whiskers=FALSE,
 box.show.box=FALSE
 )
}

