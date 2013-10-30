\name{seas}
\alias{seas}
\title{Seasonal Adjustment with X13-ARIMA-SEATS}
\usage{
  seas(x, xreg = NULL, seats = list(),
    transform.function = "auto",
    regression.chi2test = "yes", outlier = list(),
    automdl = list(), out = FALSE, dir = NULL, ...)
}
\arguments{
  \item{x}{object of class \code{"ts"}": time series to
  seasonaly adjust.}

  \item{xreg}{(optional) object of class \code{"ts"}": one
  or several user defined exogenous variables for regARIMA
  modelling.}

  \item{seats}{spec 'seats' without arguments (default).
  Seasonal adjustment by SEATS.}

  \item{transform.function}{spec \code{transform} with
  argument \code{function = "auto"} (default). Automatic
  transformation detection.}

  \item{regression.chi2test}{spec \code{regression} with
  argument \code{chi2test = "yes"} (default). Chi Square
  test for calendar effects.}

  \item{outlier}{spec \code{outlier} without arguments
  (default). Automatic oulier detection.}

  \item{automdl}{spec \code{automdl} without arguments
  (default). Automatic model search with the automodl
  module.}

  \item{out}{logical, should the standard output be saved
  in the \code{"seas"} object? (increases object size)}

  \item{dir}{character string with output path. If
  specified, the X13-ARIMA-SEATS output files are copied to
  this folder.}

  \item{...}{additional spec/arguments options (see
  details).}
}
\value{
  returns an object of class \code{"seas"}, essentially a
  list with the following elements: \item{err}{Warning
  messages from X13-ARIMA-SEATS.} \item{data}{An object of
  class \code{"ts"}", containing the seasonally adjusted
  data, the raw data, the trend component, the irregular
  component and the seasonal component.} \item{mdl}{A list
  with the model specification, similar to \code{"spc"}".
  It typically contains \code{"regression"}", which
  contains the regressors and parameter estimates, and
  \code{"arima"}", which contains the ARIMA specification
  and the parameter estimates.} \item{est}{More detailed
  information on the estimation.} \item{lks}{Summary
  statistics.} \item{coefficients}{Coefficients of the
  regARIMA model.} \item{se}{Standard errors of the
  regARIMA model.} \item{spc}{An object of class
  \code{"spclist"}, a list containing everything that is
  send to X-13ARIMA-SEATS. Each spec is on the first level,
  each argument is on the second level. Checking
  \code{"spc"}" is good start for debugging.}
  \item{call}{Function call.}

  The \code{final} function returns the adjusted series,
  the \code{plot} method shows a plot with the unadjusted
  and the adjusted series.  \code{summary} gives an
  overview of the regARIMA model. \code{static} returns the
  static call from above that is needed to replicate an
  automatic seasonal adjustment procedure the model.
}
\description{
  Core function of the seasonal package. By default,
  \code{seas} calls the automatic procedures of
  X-13ARIMA-SEATS to perform a seasonal adjustment that
  works well in most circumstances.
}
\details{
  Seasonal uses the same syntax as X-13ARIMA-SEATS. It is
  possible to invoke most options that are available in
  X-13ARIMA-SEATS. The X-13ARIMA-SEATS syntax uses specs
  and arguments, while each spec may contain some
  arguments. An additional spec/argument can be added to
  the \code{seas} function by separating spec and argument
  by a dot (\code{.}). For a more extensive description of
  the X-13ARIMA-SEATS in \code{seas}, consider the website
  on github (\url{www.github/christophsax/seasonal})
}
\examples{
x <- seas(AirPassengers)
summary(x)

x2 <- seas(x = AirPassengers, regression.aictest = c("td", "easter"))
summary(x2)
static(x2)

seas(x = AirPassengers, regression.variables = c("td1coef", "easter[1]",
"ao1951.May"), arima.model = "(0 1 1)(0 1 1)", regression.chi2test = "no",
outlier.types = "none", transform.function = "log")

final(x)
original(x)
resid(x)
coef(x)

plot(x2)
plot(x2, trend = TRUE)
monthplot(x2)
monthplot(x2, choice = "irregular")

spectrum(final(x))
spectrum(original(x))

spc(x)
mdl(x)

x3 <- seas(AirPassengers, out = TRUE)
\dontrun{
out(x3)
}

x$est$variance
x$lks

\dontrun{
inspect(AirPassengers)
}
}

