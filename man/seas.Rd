\name{seas}
\alias{seas}
\title{Seasonal Adjustment with X-13ARIMA-SEATS}
\usage{
seas(x, xreg = NULL, xtrans = NULL, seats.noadmiss = "yes",
  transform.function = "auto", regression.aictest = c("td", "easter"),
  outlier = list(), automdl = list(), na.action = na.omit, out = FALSE,
  dir = NULL, ...)
}
\arguments{
  \item{x}{object of class \code{"ts"}: time series to
  seasonaly adjust.}

  \item{xreg}{(optional) object of class \code{"ts"}: one
  or several user defined exogenous variables for regARIMA
  modelling, can be used both with \code{regression} or
  \code{x11regression}.}

  \item{xtrans}{(optional) object of class \code{"ts"}: one
  or two user defined exogenous variables for the
  \code{transform} spec. Can be specifed together with
  \code{xreg}.}

  \item{seats.noadmiss}{spec 'seats' with argument
  \code{noadmiss = "yes"} (default). Seasonal adjustment by
  SEATS, if SEATS decomposition is invalid, an alternative
  model is used (a warning is given).}

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
  (default). Automatic model search with the automdl spec.
  Set equal to \code{NULL} to turn off.}

  \item{na.action}{a function which indicates what should
  happen when the data contain NAs. \code{na.omit}
  (default), \code{na.exclude} or \code{na.fail}.}

  \item{out}{logical, should the X-13ARIMA-SEATS standard
  output be saved in the \code{"seas"} object? (this
  increases object size substantially, it is recommended to
  re-evaluate the model using the \code{\link{out}}
  function instead.)}

  \item{dir}{character string with a user defined file
  path. If specified, the X-13ARIMA-SEATS output files are
  copied to this folder. Useful for debugging.}

  \item{...}{additional spec-arguments options sent to
  X-13ARIMA-SEATS (see details).}
}
\value{
returns an object of class \code{"seas"}, essentially a
list with the following elements: \item{err}{warning
messages from X-13ARIMA-SEATS} \item{data}{seasonally
adjusted data, the raw data, the trend component, the
irregular component and the seasonal component}
\item{regressioneffects}{regression variables multiplied by
the vector of estimated regression coefficients}
\item{model}{list with the model specification, similar to
\code{"spc"}. It typically contains \code{"regression"},
which contains the regressors and parameter estimates, and
\code{"arima"}, which contains the ARIMA specification and
the parameter estimates.} \item{estimates}{detailed
information on the estimation} \item{lkstats}{summary
statistics} \item{sfspans}{(optional) data from sliding
spans analysis} \item{transform.function}{character string,
applied transformation} \item{fivebestmdl}{five best models
according to BIC criterion} \item{qs}{QS statistics}
\item{x}{input series} \item{spc}{object of class
\code{"spclist"}, a list containing everything that is send
to X-13ARIMA-SEATS. Each spec is on the first level, each
argument is on the second level.} \item{call}{function
call}

The \code{final} function returns the final adjusted
series, the \code{plot} method shows a plot with the
unadjusted and the adjusted series. \code{summary} gives an
overview of the regARIMA model.
}
\description{
Core function of the seasonal package. By default,
\code{seas} calls the automatic procedures of
X-13ARIMA-SEATS to perform a seasonal adjustment that works
well in most circumstances. Through the \code{...}
argument, it is possible to invoke almost all options that
are available in X-13ARIMA-SEATS (see details). The default
options are specified as explicit arguments and are
discussed in the arguments section.
}
\details{
\emph{seasonal} uses a very similar syntax than the
official X-13ARIMA-SEATS. The X-13ARIMA-SEATS syntax uses
\emph{specs} and \emph{arguments}, and each spec optionally
contains some arguments. In \code{seas}, an additional
spec-argument can be added by separating spec and argument
by a dot (\code{.}) (see examples).

For a more extensive description, consider the vignette or
the wiki page, which contains replications of almost all
examples from the official X-13ARIMA-SEATS manual.
}
\examples{
\dontrun{
m <- seas(AirPassengers)
summary(m)

# invoke X-13ARIMA-SEATS options as 'spec.argument'
# (consult the X-13ARIMA-SEATS manual for many more options and the wiki for
# for more examples)
seas(AirPassengers, regression.aictest = c("td"))  # no easter testing
seas(AirPassengers, force.type = "denton")  # force equality of annual values
seas(AirPassengers, x11 = list())  # use x11, overrides the 'seats' spec

# turn off the automatic procedures
seas(AirPassengers, regression.variables = c("td1coef", "easter[1]",
"ao1951.May"), arima.model = "(0 1 1)(0 1 1)", regression.aictest = NULL,
outlier = NULL, transform.function = "log")

# static replication of the first call
static(m)  # this also tests the equivalence of the static call
static(m, test = FALSE)  # no testing (useful for debugging)
static(m, coef = TRUE)  # also fixes the coefficients

# extractor functions
final(m)
original(m)
resid(m)
coef(m)

# five best models
fivebestmdl(m)

# replicating the default plots in Win X-13
plot(m)
plot(m, trend = TRUE)
monthplot(m)
monthplot(m, choice = "irregular")
spectrum(diff(final(m)) )
spectrum(diff(original(m)))
residplot(m)

# user defined regressors
# a temporary level shift in R base
tls <- ts(0, start = 1949, end = 1965, freq = 12)
window(tls, start = c(1955, 1), end = c(1957, 12)) <- 1
seas(AirPassengers, xreg = tls, outlier = NULL)
# identical to a X-13ARIMA-SEATS specification of the the level shift
seas(AirPassengers, regression.variables = c("tl1955.01-1957.12"),
     outlier = NULL)

# input and output from and to X-13ARIMA-SEATS
spc(m)  # analyzing the .spc file (for debugging)
out(m)  # display the .out file (see ?out, for details)

# NA handling
AirPassengersNA <- window(AirPassengers, end = 1962, extend = TRUE)
final(seas(AirPassengersNA, na.action = na.omit))    # no NA in final series
final(seas(AirPassengersNA, na.action = na.exclude)) # NA in final series
# final(seas(AirPassengersNA, na.action = na.fail))    # fails

# inspect tool
inspect(m)
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
\code{\link{static}}, to return the static call that is
needed to replicate an automatic model

\code{\link{inspect}}, to interactively inspect a seasonal
adjustment model.

\code{\link{plot.seas}}, for diagnostical plots.

\code{\link{qs}}, for diagnostical statistics.

\code{\link{out}}, for diagnostical re-evaluation.
}

