#' Seasonal Adjustment with X-13ARIMA-SEATS
#'
#' Main function of the seasonal package. With the default options,
#' \code{seas} calls the  automatic procedures of X-13ARIMA-SEATS to perform a
#' seasonal adjustment that works well in most circumstances. Via the \code{...}
#' argument, it is possible to invoke almost all options that are available in
#' X-13ARIMA-SEATS (see  details). The default options of \code{seas} are listed
#' as explicit arguments and are  discussed in the arguments section. A
#' full-featured graphical user interface can be accessed by the
#' \code{\link{view}} function.
#'
#'  It is possible to use the almost complete syntax of X-13ARIMA-SEAT via the
#' \code{...} argument. The syntax of X-13ARIMA-SEATS uses \emph{specs} and
#' \emph{arguments}, and each spec optionally contains some arguments. In
#' \code{seas}, an additional spec-argument can be added by separating spec and
#' argument by a dot (\code{.}) (see examples). Alternatvily, spec-argument
#' combinations can be supplied as a named list, which is useful for
#' programming.
#'
#' Similarily, the \code{\link{series}} function can be used to read almost all
#' series from X-13ARIMA-SEATS. The \code{\link{udg}} function provides access
#' to a large number of diagnostical statistics.
#'
#' For a more extensive description, consider the vignette or the wiki page,
#' which contains replications of almost all examples from the official
#' X-13ARIMA-SEATS manual.
#'
#' @param x   object of class \code{"ts"}: time series to seasonaly adjust.
#' @param xreg   (optional) object of class \code{"ts"}: one or several user
#'   defined exogenous variables for regARIMA modelling, can be used both with
#'   \code{regression} or \code{x11regression}.
#' @param xtrans   (optional) object of class \code{"ts"}: one or two user
#'   defined exogenous variables for the \code{transform} spec. Can be specifed
#'   together with \code{xreg}.
#' @param seats.noadmiss   spec 'seats' with argument \code{noadmiss = "yes"}
#'   (default). Seasonal adjustment by SEATS, if SEATS decomposition is invalid,
#'   an alternative model is used (a message is returned). If \code{noadmiss =
#'   "no"}, no approximation is done. If the seats spec is removed
#'   (\code{seats = NULL}), no seasonal adjustment is performed.
#' @param transform.function   spec \code{transform} with argument
#'   \code{function = "auto"} (default). Automatic log transformation detection.
#'   Set equal to \code{"none"}, \code{"log"} or any value that is allowed by
#'   X-13 to turn it off.
#' @param regression.aictest   spec \code{regression} with argument
#'   \code{aictest = c("td", "easter")} (default). AIC test for trading days and
#'   Easter effects. Set equal to \code{NULL} to turn it off.
#' @param outlier   spec \code{outlier} without arguments (default). Automatic
#'   oulier detection. Set equal to \code{NULL} to turn it off.
#' @param automdl   spec \code{automdl} without arguments (default). Automatic
#'   model search with the automdl spec. Set equal to \code{NULL} to turn it off.
#' @param na.action  a function which indicates what should happen when the data
#'   contain NAs. \code{na.omit} (default), \code{na.exclude} or \code{na.fail}.
#'   If \code{na.action = na.x13}, NA handling is done by X-13, i.e. NA values
#'   are substituted by -99999.
#' @param out   logical. Should the X-13ARIMA-SEATS standard output be saved in
#'   the \code{"seas"} object? (this increases object size substantially, it is
#'   recommended to re-evaluate the model using the \code{\link{out}} function
#'   instead.)
#' @param dir   character string with a user defined file path. If specified,
#'   the X-13ARIMA-SEATS output files are copied to this folder. Useful for
#'   debugging.
#' @param ...  additional spec-arguments options sent to X-13ARIMA-SEATS (see
#'   details).
#' @param list  a named list with additional spec-arguments options. This is an
#'   alternative to the \code{...} argument. It is useful for programming.
#'
#' @return returns an object of class \code{"seas"}, essentially a list with the
#'   following components:
#'   \item{series}{a list containing the output tables of X-13. To be accessed
#'   by the \code{series} function.}
#'   \item{data}{seasonally adjusted data, the
#'   raw data, the trend component, the irregular component and the seasonal
#'   component (deprecated).}
#'   \item{err}{warning messages from X-13ARIMA-SEATS}
#'   \item{udg}{content of the \code{.udg} output file}
#'   \item{est}{content of the \code{.est} output file}
#'   \item{model}{list with the model specification,
#'   similar to \code{"spc"}. It typically contains \code{"regression"}, which
#'   contains the regressors and parameter estimates, and \code{"arima"}, which
#'   contains the ARIMA specification and the parameter estimates.}
#'   \item{fivebestmdl}{Best Five ARIMA Models (unparsed)}
#'   \item{x}{input series}
#'   \item{spc}{object of class \code{"spclist"}, a list containing the content of the \code{.spc} file that is
#'   used by X-13ARIMA-SEATS. Each spec is on the first level, each
#'   argument is on the second level.}
#'   \item{call}{function call}
#'   \item{wdir}{temporary directory in which X-13ARIMA-SEATS has been run}
#'
#'   The \code{final} function returns the final adjusted series, the
#'   \code{plot} method shows a plot with the unadjusted and the adjusted
#'   series. \code{summary} gives an overview of the regARIMA model. The
#'   \code{\link{udg}} function returns diagnostical statistics.
#'
#' @seealso \code{\link{view}}, for accessing the graphical user interface.
#' @seealso \code{\link{update.seas}}, to update an existing \code{"seas"}
#'   model.
#' @seealso \code{\link{static}}, to return the 'static' call, with automated
#'   procedures substituted by their choices.
#' @seealso \code{\link{series}}, for universal X-13 table series import.
#' @seealso \code{\link{out}}, to view the full X-13 diagnostical output.
#'
#' @references
#'   Sax C, Eddelbuettel D (2018). "Seasonal Adjustment by X-13ARIMA-SEATS
#'   in R." \emph{Journal of Statistical Software}, \emph{87}(11), 1-17. doi:
#'   10.18637/jss.v087.i11 (\url{http://doi.org/10.18637/jss.v087.i11}).
#'
#'   On-Line Interface to seasonal
#'   \url{http://www.seasonal.website}
#'
#'   Comprehensive list of R examples from the X-13ARIMA-SEATS manual:
#'   \url{http://www.seasonal.website/examples.html}
#'
#'   Official X-13ARIMA-SEATS manual:
#'   \url{https://www.census.gov/ts/x13as/docX13ASHTML.pdf}
#' @export
#' @import datasets
#' @import grDevices
#' @import graphics
#' @import utils
#'
#' @examples
#' \dontrun{
#' Basic call
#' m <- seas(AirPassengers)
#' summary(m)
#'
#' # Graphical user interface
#' view(m)
#'
#' # invoke X-13ARIMA-SEATS options as 'spec.argument' through the ... argument
#' # (consult the X-13ARIMA-SEATS manual for many more options and the list of
#' # R examples for more examples)
#' seas(AirPassengers, regression.aictest = c("td"))  # no easter testing
#' seas(AirPassengers, force.type = "denton")  # force equality of annual values
#' seas(AirPassengers, x11 = "")  # use x11, overrides the 'seats' spec
#'
#' # 'spec.argument' combinations can also be supplied as a named list, which is
#' # useful for programming
#' seas(AirPassengers, list = list(regression.aictest = c("td"), outlier = NULL))
#' # constructing the list step by step
#' ll <- list()
#' ll[["x"]] <- AirPassengers
#' ll[["regression.aictest"]] <- "td"
#' ll["outlier"] <- list(NULL)  # assigning NULL to a list using single brackets
#' seas(list = ll)
#'
#' # options can be entered as vectors
#' seas(AirPassengers, regression.variables = c("td1coef", "easter[1]"))
#' seas(AirPassengers, arima.model = c(0, 1, 1, 0, 1, 1))
#' seas(AirPassengers, arima.model = "(0 1 1)(0 1 1)")     # equivalent
#'
#' # turn off the automatic procedures
#' seas(AirPassengers, regression.variables = c("td1coef", "easter[1]",
#' "ao1951.May"), arima.model = "(0 1 1)(0 1 1)", regression.aictest = NULL,
#' outlier = NULL, transform.function = "log")
#'
#' # static replication of 'm <- seas(AirPassengers)'
#' static(m)  # this also tests the equivalence of the static call
#' static(m, test = FALSE)  # no testing (much faster)
#' static(m, coef = TRUE)  # also fixes the coefficients
#'
#' # updating an existing model
#' update(m, x11 = "")
#'
#' # specific extractor functions
#' final(m)
#' predict(m)   # equivalent
#' original(m)
#' resid(m)
#' coef(m)
#' fivebestmdl(m)
#' out(m)                  # the X-13 .out file (see ?out, for details)
#' spc(m)                  # the .spc input file to X-13 (for debugging)
#'
#' # universal extractor function for any X-13ARIMA-SEATS output (see ?series)
#' series(m, "forecast.forecasts")
#'
#' # copying the output of X-13 to a user defined directory
#' seas(AirPassengers, dir = "~/mydir")
#'
#' # user defined regressors (see ?genhol for more examples)
#' # a temporary level shift in R base
#' tls <- ts(0, start = 1949, end = 1965, freq = 12)
#' window(tls, start = c(1955, 1), end = c(1957, 12)) <- 1
#' seas(AirPassengers, xreg = tls, outlier = NULL)
#' # identical to a X-13ARIMA-SEATS specification of the the level shift
#' seas(AirPassengers, regression.variables = c("tl1955.01-1957.12"),
#'      outlier = NULL)
#'
#' # forecasting an annual series without seasonal adjustment
#' m <- seas(airmiles, seats = NULL, regression.aictest = NULL)
#' series(m, "forecast.forecasts")
#'
#' # NA handling
#' AirPassengersNA <- window(AirPassengers, end = 1962, extend = TRUE)
#' final(seas(AirPassengersNA, na.action = na.omit))    # no NA in final series
#' final(seas(AirPassengersNA, na.action = na.exclude)) # NA in final series
#' # final(seas(AirPassengersNA, na.action = na.fail))    # fails
#'
#' # NA handling by X-13 (works with internal NAs)
#' AirPassengersNA[20] <- NA
#' final(seas(AirPassengersNA, na.action = na.x13))
#'
#' ## performing 'composite' adjustment
#' m.direct <- seas(ldeaths, x11 = "")
#' final.direct <- final(m.direct)
#' m.indirect <- lapply(list(mdeaths, fdeaths), seas, x11 = "")
#'
#'  # not very efficient, but keeps time series properties
#' final.indirect <- Reduce(`+`, lapply(m.indirect, final))
#'
#' ts.plot(cbind(final.indirect, final(m.direct)), col = 1:2)
#' legend("topright", legend = c("disagregated", "aggregated"), lty = 1, col = 1:2)
#'
#' }
#'
seas <- function(x = NULL, xreg = NULL, xtrans = NULL,
         seats.noadmiss = "yes", transform.function = "auto",
         regression.aictest = c("td", "easter"), outlier = "",
         automdl = "", composite = NULL, na.action = na.omit,
         out = FALSE, dir = NULL, multimode = c("x13", "R"), ..., list = NULL){

  # intial checks
  checkX13(fail = TRUE, fullcheck = FALSE, htmlcheck = FALSE)


  # multi mode
  multi_x <- !is.null(x) && NCOL(x) > 1
  multi_list <-
    !is.null(list) &&
    all(sapply(list, inherits, "list")) &&
    length(list) > 1

  if (multi_x || multi_list) {
    z <- seas_multi(
      x = x,
      xreg = xreg,
      xtrans = xtrans,
      seats.noadmiss = seats.noadmiss,
      transform.function = transform.function,
      regression.aictest = regression.aictest,
      outlier = outlier,
      automdl = automdl,
      composite = composite,
      na.action = na.omit,
      out = out,
      dir = dir,
      list_dots = list(...),
      multimode = multimode,
      list = list
    )
    return(z)
  }

  # standard run
  list_combined <- enrich_list(
    list = list,
    list_dots = list(...),
    x = x,
    xreg = xreg,
    xtrans = xtrans,
    seats.noadmiss = seats.noadmiss,
    transform.function = transform.function,
    regression.aictest = regression.aictest,
    outlier = outlier,
    automdl = automdl
  )

  seas_list(
    list = list_combined,
    na.action = na.omit,
    out = out,
    dir = dir,
    call = match.call(),
    series.name = deparse(substitute(x))[1]
  )
}
