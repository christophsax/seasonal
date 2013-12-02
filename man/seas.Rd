\name{seas}
\alias{fivebestmdl}
\alias{seas}
\title{Seasonal Adjustment with X-13ARIMA-SEATS}
\usage{
  seas(x, xreg = NULL, seats.noadmiss = "yes",
    transform.function = "auto",
    regression.aictest = c("td", "easter"),
    outlier = list(), automdl = list(),
    na.action = na.omit, out = FALSE, dir = NULL, ...)

  fivebestmdl(x)
}
\arguments{
  \item{x}{object of class \code{"ts"}": time series to
  seasonaly adjust.}

  \item{xreg}{(optional) object of class \code{"ts"}": one
  or several user defined exogenous variables for regARIMA
  modelling.}

  \item{seats.noadmiss}{spec 'seats' with argument
  \code{noadmiss = "yes"} (default). Seasonal adjustment by
  SEATS, if SEATS decomposition is invalid, an alternative
  model is used with a warning.}

  \item{transform.function}{spec \code{transform} with
  argument \code{function = "auto"} (default). Automatic
  log transformation detection.  Set equal to \code{"none"}
  or \code{"log"} to turn off.}

  \item{regression.aictest}{spec \code{regression} with
  argument \code{aictest = c("td", "easter")} (default).
  AIC test for trading days and Easter effects. Set equal
  to \code{NULL} to turn off.}

  \item{outlier}{spec \code{outlier} without arguments
  (default). Automatic oulier detection. Set equal to
  \code{NULL} to turn off.}

  \item{automdl}{spec \code{automdl} without arguments
  (default). Automatic model search with the automodl
  module. Set equal to \code{NULL} to turn off.}

  \item{na.action}{a function which indicates what should
  happen when the data contain NAs. \code{na.omit}
  (default), \code{na.exclude} or \code{na.fail}.}

  \item{out}{logical, should the X-13ARIMA-SEATS standard
  output be saved in the \code{"seas"} object? (increases
  object size)}

  \item{dir}{character string with output path. If
  specified, the X-13ARIMA-SEATS output files are copied to
  this folder.}

  \item{...}{additional spec-arguments options (see
  details).}
}
\value{
  returns an object of class \code{"seas"}, essentially a
  list with the following elements: \item{err}{Warning
  messages from X-13ARIMA-SEATS.} \item{data}{An object of
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
  arguments. An additional spec-argument can be added to
  the \code{seas} function by separating spec and argument
  by a dot (\code{.}). For a more extensive description of
  the X-13ARIMA-SEATS in \code{seas}, consider the website
  on github
  (\url{https://github.com/christophsax/seasonal})
}
\examples{
\dontrun{
x <- seas(AirPassengers)
summary(x)

# invoke X-13ARIMA-SEATS options as 'spec.argument'
# (consult the X-13ARIMA-SEATS manual for many more options and the wiki for
# for more examples)
seas(AirPassengers, regression.aictest = c("td"))  # no easter testing
seas(AirPassengers, force.type = "denton")  # force equality of annual values
seas(AirPassengers, x11 = list())  # use x11, overrides the 'seats' spec

# turn off automatic procedures:
seas(x = AirPassengers, regression.variables = c("td1coef", "easter[1]",
"ao1951.May"), arima.model = "(0 1 1)(0 1 1)", regression.aictest = NULL,
outlier = NULL, transform.function = "log")

# static replication of the first call
static(x)  # this also tests the equivalence of the static call
static(x, test = FALSE)  # no testing (for debuging)
static(x, coef = TRUE)  # also fixes the coefficients

# extractor functions
final(x)
original(x)
resid(x)
coef(x)

# five best models
fivebestmdl(x)

# replicating the default plots in Win X-13
plot(x)
plot(x, trend = TRUE)
monthplot(x)
monthplot(x, choice = "irregular")
spectrum(diff(final(x)) )
spectrum(diff(original(x)))
residplot(x)

# user defined regressors:
# a temporary level shift in R base
tls <- ts(0, start = 1949, end = 1965, freq = 12)
window(tls, start = c(1955, 1), end = c(1957, 12)) <- 1
seas(AirPassengers, xreg = tls, outlier = NULL)
# identical to a X-13ARIMA-SEATS specification of the the level shift
seas(AirPassengers, regression.variables = c("tl1955.01-1957.12"),
     outlier = NULL)

# analyzing X-13ARIMA-SEATS input and output files (for debuging)
spc(x)  # the .spc file, as generated by seas
mdl(x)  # the .mdl file, as received by seas
\dontrun{
x3 <- seas(AirPassengers, out = TRUE)
out(x3)  # the full .out output from X-13ARIMA-SEATS (very long!)
}

# more components in a "seats" object
x$est$variance
x$lks

# standard NA handling with na.action
AirPassengersNA <- window(AirPassengers, start = c(1948, 6), end = c(1961, 4),
                          extend = TRUE)
final(seas(AirPassengersNA, na.action = na.omit))     # default
final(seas(AirPassengersNA, na.action = na.exclude))
# final(seas(AirPassengersNA, na.action = na.fail))   # fails

# inspection tool
inspect(AirPassengers)
}
}
\references{
  Github page with a more detailed description.
  \url{https://github.com/christophsax/seasonal}

  Wiki page with R examples from the X-13ARIMA-SEATS:
  \url{https://github.com/christophsax/seasonal/wiki/Examples-of-X-13ARIMA-SEATS-in-R}

  X-13ARIMA-SEATS manual:
  \url{http://www.census.gov/ts/x13as/docX13AS.pdf}
}

