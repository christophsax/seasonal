\name{final}
\alias{final}
\alias{irregular}
\alias{original}
\alias{residuals.seas}
\alias{trend}
\title{Time Series of a Seasonal Adjustment Model}
\usage{
final(object)

original(object)

trend(object)

irregular(object)

\method{residuals}{seas}(object, ...)
}
\arguments{
  \item{object}{an object of class \code{"seas"}.}

  \item{...}{not used. For compatibility with the generic.}
}
\value{
returns a \code{"ts"} object, depending on the function.
}
\description{
Functions to extract the main time series from a
\code{"seas"} object. For universal import of
X-13ARIMA-SEATS tables, use the \code{\link{series}}
function.
}
\details{
These functions support R default NA handling. If
\code{na.action = na.exclude} is specified in the call to
\code{seas}, the time series will also contain NAs.
}
\examples{
\dontrun{

m <- seas(AirPassengers)

final(m)
original(m)
irregular(m)
trend(m)

# NA handling
AirPassengersNA <- window(AirPassengers, end = 1962, extend = TRUE)
final(seas(AirPassengersNA, na.action = na.omit))    # no NA in final series
final(seas(AirPassengersNA, na.action = na.exclude)) # NA in final series
# final(seas(AirPassengersNA, na.action = na.fail))    # fails
}
}
\references{
Vignette with a more detailed description:
\url{http://cran.r-project.org/web/packages/seasonal/vignettes/seas.pdf}

Wiki page with a comprehensive list of R examples from the
X-13ARIMA-SEATS manual:
\url{https://github.com/christophsax/seasonal/wiki/Examples-of-X-13ARIMA-SEATS-in-R}

Official X-13ARIMA-SEATS manual:
\url{http://www.census.gov/ts/x13as/docX13AS.pdf}
}
\seealso{
\code{\link{seas}} for the main function of seasonal.

\code{\link{series}}, for universal X-13 output extraction.
}

