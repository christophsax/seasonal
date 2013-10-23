\name{static}
\alias{static}
\title{Static Call of a seas Object}
\usage{
  static(x, name = NULL)
}
\arguments{
  \item{x}{an object of class \code{seas}}

  \item{name}{character string, optionally specify the name
  of the input time series}
}
\value{
  Static call of an object of class \code{seas}. Can be
  copy/pasted into an R script.
}
\description{
  Static Call of a seas Object
}
\examples{
x <- seas(AirPassengers)
static(x)
static(x, name = "ArbitrayName")
}

