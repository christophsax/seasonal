\name{final}
\alias{final}
\alias{irregular}
\alias{original}
\alias{regressioneffects}
\alias{residuals.seas}
\alias{trend}
\title{Time Series from a Seasonal Adjustment Model}
\usage{
final(object)

original(object)

trend(object)

irregular(object)

regressioneffects(object)

\method{residuals}{seas}(object, ...)
}
\arguments{
  \item{object}{an object of class \code{"seas"}.}
}
\value{
returns a \code{"ts"} object, depending on the function.

\code{regressioneffects} returns a \code{"mts"} object,
containing the regression variables multiplied by the
vector of estimated regression coefficients
}
\description{
These functions extract time series from a \code{"seas"}
object.
}
\examples{
\dontrun{

m <- seas(AirPassengers)

final(m)
original(m)
irregular(m)
trend(m)
regressioneffects(m)

# trading day and easter adjustment w/o seasonal adjustment
summary(m)
re <- regressioneffects(m)
ce <- re[, 'Trading.Day'] + re[, 'Holiday']
# be aware of the log transformation
AirPassengersWoTd <- exp(log(AirPassengers) - ce)

# NA handling
AirPassengersNA <- window(AirPassengers, end = 1962, extend = TRUE)
final(seas(AirPassengersNA, na.action = na.omit))    # no NA in final series
final(seas(AirPassengersNA, na.action = na.exclude)) # NA in final series
final(seas(AirPassengersNA, na.action = na.fail))    # fails
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
}

