\name{spc}
\alias{final}
\alias{irregular}
\alias{mdl}
\alias{original}
\alias{out}
\alias{spc}
\alias{trend}
\title{Extractor Functions for seas Elements and Series}
\usage{
  spc(x)

  mdl(x)

  out(x)

  final(x)

  original(x)

  trend(x)

  irregular(x)
}
\arguments{
  \item{x}{an object of class \code{"seas"}.}
}
\value{
  return an element or a series, depending on the function
}
\description{
  These functions extract elements or series from a
  \code{"seas"} object.
}
\examples{
\dontrun{
x <- seas(AirPassengers)

final(x)
original(x)
irregular(x)
trend(x)

spc(x)  # X13-ARIMA-SEATS .spc file
mdl(x)  # X13-ARIMA-SEATS .mdl file

# out(x)  # returns an error
x2 <- seas(AirPassengers, out = TRUE)
out(x2)    # this works: X13-ARIMA-SEATS .out file
}
}
\seealso{
  \code{\link{seas}} for the main function.
}

