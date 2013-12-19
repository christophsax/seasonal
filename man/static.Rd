\name{static}
\alias{static}
\title{Static Call of a seas Object}
\usage{
static(x, coef = FALSE, name = NULL, test = TRUE)
}
\arguments{
  \item{x}{an object of class \code{seas}}

  \item{coef}{logical, if \code{TRUE}, the coefficients are
  treated as fixed, instead of beeing estimated.}

  \item{name}{character string, optionally specify the name
  of the input time series}

  \item{test}{logical. By default the static call is
  executed and compared to the input call. If the final
  series is not identical, an error is returned.  If
  \code{FALSE}, the option is disabled (useful for
  debugging)}
}
\value{
Static call of an object of class \code{seas}. Can be
copy/pasted into an R script.
}
\description{
A static call is a static replication of a call. Automatic
procedures are subistuted by the automatically selected
spec/argument options.
}
\examples{
\dontrun{
x <- seas(AirPassengers)
static(x)
static(x, name = "ArbitrayName", test = FALSE)
}
}

