#' Seasonal Adjustment with X-13ARIMA-SEATS
#'
#' Main function of the seasonal package. With the default options,
#' `seas` calls the  automatic procedures of X-13ARIMA-SEATS to perform a
#' seasonal adjustment that works well in most circumstances. Via the `...`
#' argument, it is possible to invoke almost all options that are available in
#' X-13ARIMA-SEATS (see  details). The default options of `seas` are listed
#' as explicit arguments and are  discussed in the arguments section. A
#' full-featured graphical user interface can be accessed by the
#' [view()] function.
#'
#'  It is possible to use the almost complete syntax of X-13ARIMA-SEAT via the
#' `...` argument. The syntax of X-13ARIMA-SEATS uses *specs* and
#' *arguments*, and each spec optionally contains some arguments. In
#' `seas`, an additional spec-argument can be added by separating spec and
#' argument by a dot (`.`) (see examples). Alternatively, spec-argument
#' combinations can be supplied as a named list, which is useful for
#' programming.
#'
#' Similarly, the [series()] function can be used to read almost all
#' series from X-13ARIMA-SEATS. The [udg()] function provides access
#' to a large number of diagnostical statistics.
#'
#' For a more extensive description, consider `vignette("seas")` or the wiki page,
#' which contains replications of almost all examples from the official
#' X-13ARIMA-SEATS manual.
#'
#' @param x   object of class `"ts"` or `"mts", or a list of `"ts"` objects: time series to seasonally adjust.
#' @param xreg   (optional) object of class `"ts"`: one or several user
#'   defined exogenous variables for regARIMA modeling, can be used both with
#'   `regression` or `x11regression`.
#' @param xtrans   (optional) object of class `"ts"`: one or two user
#'   defined exogenous variables for the `transform` spec. Can be specifed
#'   together with `xreg`.
#' @param seats.noadmiss   spec 'seats' with argument `noadmiss = "yes"`
#'   (default). Seasonal adjustment by SEATS, if SEATS decomposition is invalid,
#'   an alternative model is used (a message is returned). If `noadmiss =
#'   "no"`, no approximation is done. If the seats spec is removed
#'   (`seats = NULL`), no seasonal adjustment is performed.
#' @param transform.function   spec `transform` with argument
#'   `function = "auto"` (default). Automatic log transformation detection.
#'   Set equal to `"none"`, `"log"` or any value that is allowed by
#'   X-13 to turn it off.
#' @param regression.aictest   spec `regression` with argument
#'   `aictest = c("td", "easter")` (default). AIC test for trading days and
#'   Easter effects. Set equal to `NULL` to turn it off.
#' @param outlier   spec `outlier` without arguments (default). Automatic
#'   outlier detection. Set equal to `NULL` to turn it off.
#' @param automdl   spec `automdl` without arguments (default). Automatic
#'   model search with the automdl spec. Set equal to `NULL` to turn it off.
#' @param composite   spec `composite`. A named list with spec-arguments
#'   for the aggregation of multiple series. Also requries
#'   `series.comtype = "add"` or similar. Set equal to `NULL` to turn
#'   it off (default). See `vignette("multiple")`.
#' @param na.action  a function which indicates what should happen when the data
#'   contain NAs. `na.omit` (default), `na.exclude` or `na.fail`.
#'   If `na.action = na.x13`, NA handling is done by X-13, i.e. NA values
#'   are substituted by -99999.
#' @param out   logical. Should the X-13ARIMA-SEATS standard output be saved in
#'   the `"seas"` object? (this increases object size substantially, it is
#'   recommended to re-evaluate the model using the [out()] function
#'   instead.)
#' @param dir   character string with a user defined file path. If specified,
#'   the X-13ARIMA-SEATS output files are copied to this folder. Useful for
#'   debugging.
#' @param multimode   one of `"x13"` or `"R"`. When multiple series
#'   are supplied, should they be processed in a single call (`"x13"`) or
#'   processed individually (`"R"`). See `vignette("multiple")`.
#' @param ...  additional spec-arguments options sent to X-13ARIMA-SEATS (see
#'   details).
#' @param list  a named list with additional spec-arguments options. This is an
#'   alternative to the `...` argument. It is useful for programming.
#'
#' @return returns an object of class `"seas"`, essentially a list with the
#'   following components:
#'   \item{series}{a list containing the output tables of X-13. To be accessed
#'   by the `series` function.}
#'   \item{data}{seasonally adjusted data, the
#'   raw data, the trend component, the irregular component and the seasonal
#'   component (deprecated).}
#'   \item{err}{warning messages from X-13ARIMA-SEATS}
#'   \item{udg}{content of the `.udg` output file}
#'   \item{est}{content of the `.est` output file}
#'   \item{model}{list with the model specification,
#'   similar to `"spc"`. It typically contains `"regression"`, which
#'   contains the regressors and parameter estimates, and `"arima"`, which
#'   contains the ARIMA specification and the parameter estimates.}
#'   \item{fivebestmdl}{Best Five ARIMA Models (unparsed)}
#'   \item{x}{input series}
#'   \item{spc}{object of class `"spclist"`, a list containing the content of the `.spc` file that is
#'   used by X-13ARIMA-SEATS. Each spec is on the first level, each
#'   argument is on the second level.}
#'   \item{call}{function call}
#'   \item{wdir}{temporary directory in which X-13ARIMA-SEATS has been run}
#'
#'   The `final` function returns the final adjusted series, the
#'   `plot` method shows a plot with the unadjusted and the adjusted
#'   series. `summary` gives an overview of the regARIMA model. The
#'   [udg()] function returns diagnostical statistics.
#'
#' @seealso [view()], for accessing the graphical user interface.
#' @seealso [update.seas()], to update an existing `"seas"`
#'   model.
#' @seealso [static()], to return the 'static' call, with automated
#'   procedures substituted by their choices.
#' @seealso [series()], for universal X-13 table series import.
#' @seealso [out()], to view the full X-13 diagnostical output.
#'
#' @references
#'   Sax C, Eddelbuettel D (2018). "Seasonal Adjustment by X-13ARIMA-SEATS
#'   in R." *Journal of Statistical Software*, *87*(11), 1-17.
#'   \doi{10.18637/jss.v087.i11}.
#'
#'   On-Line Interface to seasonal
#'   <http://www.seasonal.website>
#'
#'   Comprehensive list of R examples from the X-13ARIMA-SEATS manual:
#'   <http://www.seasonal.website/examples.html>
#'
#'   Official X-13ARIMA-SEATS manual:
#'   <https://www.census.gov/ts/x13as/docX13ASHTML.pdf>
#' @export
#' @import datasets
#' @import grDevices
#' @import graphics
#' @import utils
#'
#' @examples
#'
#' # Basic call
#' \donttest{
#' m <- seas(AirPassengers)
#' summary(m)
#' }
#'
#' # Graphical user interface
#' \dontrun{
#' view(m)
#' }
#'
#' \donttest{
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
#' spc(m)                  # the .spc input file to X-13 (for debugging)
#' out(m)                  # the X-13 .out file (see ?out, for details)
#'
#' # universal extractor function for any X-13ARIMA-SEATS output (see ?series)
#' series(m, "forecast.forecasts")
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
#' seas(
#'   cbind(mdeaths, fdeaths),
#'   composite = list(),
#'   series.comptype = "add"
#' )
#' }
seas <- function(x = NULL, xreg = NULL, xtrans = NULL,
         seats.noadmiss = "yes", transform.function = "auto",
         regression.aictest = c("td", "easter"), outlier = "",
         automdl = "", composite = NULL, na.action = na.omit,
         out = FALSE, dir = NULL, multimode = c("x13", "R"), ..., list = NULL){

  # initial checks
  checkX13(fail = TRUE, fullcheck = FALSE, htmlcheck = FALSE)


  # multi mode
  multi_x <- (!is.null(x) && NCOL(x) > 1) || is.list(x)
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
      na.action = na.action,
      out = out,
      dir = dir,
      list_dots = list(...),
      multimode = multimode,
      list = list,
      call = match.call()
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
    na.action = na.action,
    out = out,
    dir = dir,
    call = match.call(),
    series.name = deparse(substitute(x))[1]
  )
}
