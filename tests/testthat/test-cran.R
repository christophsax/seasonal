# --- Startup tests ------------------------------------------------------------

x13binary::checkX13binary()


# --- Baisc tests of seasonal --------------------------------------------------

# examples from ?seas and ?series, without the graphs

# # don't let the tests fail on oldrel (remove if x13binary works on oldrel)
# r.version <- paste(R.Version()$major, R.Version()$minor, sep = ".")
# is.oldrel <- .Platform$OS.type == "windows" && (compareVersion(r.version, "3.1.3") < 1)

if (!x13binary::supportedPlatform()) skip("platform not supported")

testthat::skip_if(
  getRversion() < "4.0" && !identical(Sys.getenv("NOT_CRAN"), "true"),
  "skip on CRAN mac oldrel"
)

test_that("Baisc examples of seasonal work through", {
  expect_null(checkX13())
})


test_that("Basic examples of seasonal work through", {

  m <- seas(AirPassengers)
  summary(m)

  expect_s3_class(m, "seas")

  # invoke X-13ARIMA-SEATS options as 'spec.argument' through the ... argument
  # (consult the X-13ARIMA-SEATS manual for many more options and the list of
  # R examples for more examples)
  seas(AirPassengers, regression.aictest = c("td"))  # no easter testing
  seas(AirPassengers, force.type = "denton")  # force equality of annual values
  seas(AirPassengers, x11 = "")  # use x11, overrides the 'seats' spec

  # 'spec.argument' combinations can also be supplied as a named list, which is
  # useful for programming
  seas(AirPassengers, list = list(regression.aictest = c("td"), outlier = NULL))
  # constructing the list step by step
  ll <- list()
  ll[["x"]] <- AirPassengers
  ll[["regression.aictest"]] <- "td"
  ll["outlier"] <- list(NULL)  # assigning NULL to a list using single brackets
  seas(list = ll)

  # options can be entered as vectors
  seas(AirPassengers, regression.variables = c("td1coef", "easter[1]"))
  seas(AirPassengers, arima.model = c(0, 1, 1, 0, 1, 1))
  seas(AirPassengers, arima.model = "(0 1 1)(0 1 1)")     # equivalent

  # turn off the automatic procedures
  seas(AirPassengers, regression.variables = c("td1coef", "easter[1]",
  "ao1951.May"), arima.model = "(0 1 1)(0 1 1)", regression.aictest = NULL,
  outlier = NULL, transform.function = "log")

  # static replication of 'm <- seas(AirPassengers)'
  static(m)  # this also tests the equivalence of the static call
  static(m, test = FALSE)  # no testing (much faster)
  # static(m, coef = TRUE)  # also fixes the coefficients

  # specific extractor functions
  expect_s3_class(final(m), "ts")
  original(m)
  resid(m)
  coef(m)
  fivebestmdl(m)
  spc(m)                  # the .spc input file to X-13 (for debugging)

  # universal extractor function for any X-13ARIMA-SEATS output (see ?series)
  expect_s3_class(series(m, "forecast.forecasts"), "ts")

  # copying the output of X-13 to a user defined directory
  expect_s3_class(seas(AirPassengers, dir = tempdir()), "seas")

  # user defined regressors (see ?genhol for more examples)
  # a temporary level shift in R base
  tls <- ts(0, start = 1949, end = 1965, freq = 12)
  window(tls, start = c(1955, 1), end = c(1957, 12)) <- 1
  seas(AirPassengers, xreg = tls, outlier = NULL)
  # identical to a X-13ARIMA-SEATS specification of the the level shift
  seas(AirPassengers, regression.variables = c("tl1955.01-1957.12"),
       outlier = NULL)

  # forecasting an annual series without seasonal adjustment
  m <- seas(airmiles, seats = NULL, regression.aictest = NULL)
  series(m, "forecast.forecasts")

  # NA handling
  AirPassengersNA <- window(AirPassengers, end = 1962, extend = TRUE)
  final(seas(AirPassengersNA, na.action = na.omit))    # no NA in final series
  final(seas(AirPassengersNA, na.action = na.exclude)) # NA in final series
  # final(seas(AirPassengersNA, na.action = na.fail))    # fails

  # NA handling by X-13 (works with internal NAs)
  AirPassengersNA[20] <- NA
  final(seas(AirPassengersNA, na.action = na.x13))

  m <- seas(AirPassengers)
  series(m, "fct")  # re-evaluate with the forecast spec activated

  # more than one series
  expect_s3_class(series(m, c("rsd", "fct")), "ts")

  m <- seas(AirPassengers, forecast.save = "fct")
  series(m, "fct") # no re-evaluation (much faster!)

  # using long names
  expect_s3_class(series(m, "forecast.forecasts"), "ts")

  # history spec (takes )
  # series(m, "history.trendestimates")
  # series(m, "history.sfestimates")
  # series(m, "history.saestimates")
  # series(m, c("history.sfestimates", "history.trendestimates"))

  # # slidingspans spec
  # series(m, "slidingspans.sfspans")
  # series(m, "slidingspans.tdspans")

  # fundamental identities of seasonal adjustment
  # Y = T * I * (S * TD)
  all.equal(AirPassengers, series(m, "seats.trend") *
           series(m, "seats.irregular") * series(m, "seats.adjustfac"))
  # Y_sa = Y / (S * TD)
  all.equal(final(m), AirPassengers / series(m, "seats.adjustfac"))

  ### Some X-13ARIMA-SEATS functions can be replicated in R:

  # X-13ARIMA-SEATS spectrum
  series(m, "spectrum.specorig")[,-1]

  # X-13ARIMA-SEATS pacf
  x13.pacf <- series(m, "identify.pacf")

  update(m, x11 = "")
  update(m, x = sqrt(AirPassengers), x11 = "")

  ### multi seas
  # series as a list
  m <- seas(x = list(a = mdeaths, b = AirPassengers), x11 = "", list = list(list(), list(outlier.critical = 3)))

  m_a <- seas(mdeaths, x11 = "")
  m_b <- seas(AirPassengers, x11 = "", outlier.critical = 3)

  expect_equal(final(m$a), final(m_a))
  expect_equal(final(m$b), final(m_b))

})

