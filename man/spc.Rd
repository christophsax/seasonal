\name{spc}
\alias{final}
\alias{irregular}
\alias{mdl}
\alias{original}
\alias{siratio}
\alias{spc}
\alias{trend}
\title{Extractor Functions for seas Elements and Series}
\usage{
spc(x)

mdl(x)

final(x)

original(x)

trend(x)

irregular(x)

siratio(x)
}
\arguments{
  \item{x}{an object of class \code{"seas"}.}
}
\value{
return an element or a series, depending on the function
}
\description{
These functions extract elements or series from a
\code{"seas"} object. The extractor functions for time
series also deal with the treatment of NA values.
}
\examples{
\dontrun{
x <- seas(AirPassengers)

final(x)
original(x)
irregular(x)
trend(x)

# QS seasonality statistics
qs(x)

spc(x)  # X13-ARIMA-SEATS .spc file
mdl(x)  # X13-ARIMA-SEATS .mdl file
}
}
\seealso{
\code{\link{seas}} for the main function.

\code{\link{regressioneffects}}, for how to extract the
regression effects of the regARIMA model.

\code{\link{fivebestmdl}}, for how to extract the five best
ARIMA models.
}

