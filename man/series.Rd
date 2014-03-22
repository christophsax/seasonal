\name{series}
\alias{series}
\title{Universal Import of X-13ARIMA-SEATS Output}
\usage{
series(x, series, reeval = TRUE)
}
\arguments{
  \item{x}{an object of class \code{"seas"}.}

  \item{series}{character vector, short or long names of an
  X-13ARIMA-SEATS table. If a long name is specified, it
  needs to be combined with the spec name and separated by
  a dot (it is not unique, otherwise). More than one series
  can be specified (see examples).}

  \item{reeval}{logical, if TRUE, the model is re-evaluated
  with the corresponding specs enabled (also returning a
  message)}
}
\description{
With the exception the composite spec, the \code{series}
function imports all tables that can be saved in
X-13ARIMA-SEATS.
}
\details{
If the save argument is not specified in the model call,
\code{series} re-evaluates the call with the corresponding
specs enabled (also returning a message). Note that
re-evaluation doubles the overall computational time. If
you want to accelerate the procedure, you have to be
explicit about the output in the model call (see examples).
}
\examples{
\dontrun{

m <- seas(AirPassengers)
series(m, "fct")  # re-evaluate with the forcast activated

# more than one series
series(m, c("d7", "d8", "fct"))

m <- seas(AirPassengers, forecast.save = "fct")
series(m, "fct")

# using long names
series(m, "forecast.forecasts")


### Some X-13ARIMA-SEATS functions can be replicated in R:

# X-13ARIMA-SEATS spectrum
plot(series(m, "spectrum.specorig")[,-1], t = "l")
# R equivalent: spectrum from stats
spectrum(diff(log(AirPassengers)), method = "ar")

# X-13ARIMA-SEATS pacf
x13.pacf <- series(m, "identify.pacf")
plot(x13.pacf[,1:2])
lines(x13.pacf[,3])
lines(-x13.pacf[,3])

# R equivalent: pacf from stats
pacf(AirPassengers, lag.max = 35)


### advanced examples
# (for more, see the wiki.)

# trading day and easter adjustment w/o seasonal adjustment
summary(m)
re <- series(m, "estimate.regressioneffects")
ce <- re[, 'Trading.Day'] + re[, 'Holiday']
# be aware of the log transformation
AirPassengersWoTd <- exp(log(AirPassengers) - ce)
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
\code{\link{seas}} for the main function.
}

