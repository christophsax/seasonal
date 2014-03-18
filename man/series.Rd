\name{series}
\alias{series}
\title{Universal import of X-13ARIMA-SEATS output}
\usage{
series(x, series, reeval = TRUE)
}
\arguments{
  \item{x}{an object of class \code{"seas"}.}

  \item{series}{character, short or long names of an
  X-13ARIMA-SEATS save file. See the manual for a
  description. If a long name is specified, it needs to be
  combined with the spec name and separated by a dot (it is
  not unique, otherwise). More than one series can be
  specified. See examples.}
}
\description{
With the \code{series} function, it is possible to import
all tables that can be saved in X-13ARIMA-SEATS (exception:
composite spec).
}
\details{
If the save argument has not been specified in the model
call, \code{series} re-evaluates the call with the
'forecast' spec enabled. Note that re-evaluation doubles
the overall computational time. If you want to speed it up,
you have to be explicit about the output in the model call.
}
\examples{
\dontrun{

m <- seas(AirPassengers)
series(m, "fct")  # re-evaluate with the forcast activated


# more than one series
series(m, c("d7", "d8"))

m <- seas(AirPassengers, forecast.save = "fct")
series(m, "fct")

}
}

