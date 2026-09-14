# The remaining branches: input validation, list methods, and the paths that
# only a second model or a different frequency reaches.

test_that("seas() refuses input that is not a time series", {
  skip_if_no_x13()

  expect_error(seas(1:10), "not a time series")
  expect_error(
    seas(ts(1:24, start = c(500, 1), frequency = 12)),
    "start year of 'x' must be > 999"
  )
})

test_that("seas() requires xreg and xtrans to match the frequency of x", {
  skip_if_no_x13()

  quarterly <- ts(rnorm(40), start = 2000, frequency = 4)

  expect_error(
    seas(AirPassengers, xreg = quarterly),
    "xreg and x must be of the same frequency"
  )
  expect_error(
    seas(AirPassengers, xtrans = quarterly),
    "xtrans and x must be of the same frequency"
  )
})

test_that("seas() takes a user defined transformation series", {
  skip_if_no_x13()

  m <- seas(
    AirPassengers,
    xtrans = sqrt(AirPassengers),
    transform.function = "log",
    transform.type = "temporary"
  )

  expect_s3_class(m, "seas")
  expect_true("transform" %in% names(spc(m)))
})

test_that("seas() names a single user regressor", {
  skip_if_no_x13()

  # must extend past the forecast horizon, or X-13 refuses the run
  ser <- ts(rnorm(240), start = 1949, frequency = 12)
  m <- seas(AirPassengers, xreg = ser, regression.usertype = "ao")

  expect_s3_class(m, "seas")
  expect_true("xreg" %in% spc(m)$regression$user)
})

test_that("extractors work on multiple models at once", {
  skip_if_no_x13()

  m <- seas(cbind(mdeaths, fdeaths), x11 = "")

  expect_s3_class(original(m), "mts")
  expect_s3_class(trend(m), "mts")
  expect_s3_class(irregular(m), "mts")
  expect_equal(ncol(trend(m)), 2)
})

test_that("update.seas() refuses multiple models", {
  skip_if_no_x13()

  m <- seas(cbind(mdeaths, fdeaths), x11 = "")

  # note that update(m) does not reach this guard: "seas_multi" has no
  # update method of its own, so dispatch does not land in update.seas()
  expect_error(
    seasonal:::update.seas(m),
    "does not work on multiple"
  )
})

test_that("update(evaluate = FALSE) returns the call", {
  skip_if_no_x13()

  z <- update(test_model("x11"), evaluate = FALSE)
  expect_true(is.call(z))
})

test_that("predict() is final(), with or without new data", {
  skip_if_no_x13()

  m <- test_model("x11")

  expect_equal(predict(m), final(m))
  expect_s3_class(predict(m, x11.seasonalma = "s3x9"), "ts")

  short <- seas(window(AirPassengers, end = c(1959, 12)), x11 = "")
  expect_s3_class(predict(short, AirPassengers), "ts")
})

test_that("outlier() handles quarterly models and ranges", {
  skip_if_no_x13()

  m <- test_model("quarterly")
  m$model$regression$variables <- c("ao1980.2", "qi1985.2-1985.4")

  ol <- outlier(m)
  expect_equal(sum(!is.na(ol)), 2)
})

test_that("outlier() refuses a frequency it cannot place", {
  skip_if_no_x13()

  m <- test_model("x11")
  m$data <- ts(m$data, start = 2000, frequency = 6)
  m$model$regression$variables <- "ao2000.2"

  expect_error(outlier(m), "Frequency not supported")
})

test_that("Date_ts() handles semi annual series", {
  z <- seasonal:::Date_ts(ts(1:10, start = 2000, frequency = 2))

  expect_s3_class(z, "Date")
  expect_true(all(as.integer(format(z, "%m")) %in% c(1, 7)))
})

test_that("print() of a model shows the X-13 messages", {
  skip_if_no_x13()

  m <- test_model("seats")
  m$err <- NULL

  expect_no_error(capture.output(print(m)))
})

test_that("fivebestmdl() reads the plain text table", {
  skip_if_no_x13()

  # the stored table is html, so build the fixed width plain text form:
  # the model sits at columns 19 to 32, the BIC at 51 to 56
  plain_line <- function(arima, bic) {
    s <- strrep(" ", 60)
    substr(s, 19, 18 + nchar(arima)) <- arima
    substr(s, 51, 50 + nchar(bic)) <- bic
    s
  }

  m <- test_model("seats")
  m$fivebestmdl <- c(
    "  Best Five ARIMA Models",
    "  header",
    plain_line("(0 1 1)(0 1 1)", "-5.123"),
    plain_line("(0 1 2)(0 1 1)", "-5.120"),
    plain_line("(1 1 1)(0 1 1)", "-5.118"),
    plain_line("(0 1 1)(1 1 0)", "-5.110"),
    plain_line("(2 1 0)(0 1 1)", "-5.101")
  )

  with_htmlmode(0, {
    f <- fivebestmdl(m)

    expect_s3_class(f, "data.frame")
    expect_equal(nrow(f), 5)
    expect_equal(f$arima[1], "(0 1 1)(0 1 1)")
    expect_equal(f$bic[1], -5.123)
  })
})

test_that("fivebestmdl() re-runs with automdl when the model was given", {
  skip_if_no_x13()

  # the model was specified, so X-13 did not search and saved no table
  m <- test_model("regressors")
  expect_null(m$fivebestmdl)

  f <- fivebestmdl(m)
  expect_s3_class(f, "data.frame")
  expect_equal(nrow(f), 5)
})

test_that("fivebestmdl() gives up rather than looping", {
  skip_if_no_x13()

  m <- test_model("regressors")
  m$reeval <- TRUE

  expect_null(fivebestmdl(m))
})

test_that("transformfunction() resolves an automatic choice", {
  skip_if_no_x13()

  m <- test_model("seats")
  m$spc$transform$`function` <- "auto"

  expect_true(transformfunction(m) %in% c("log", "none"))

  # without the diagnostic, it falls back to none
  m$udg <- m$udg[setdiff(names(m$udg), "aictrans")]
  expect_equal(transformfunction(m), "none")
})

test_that("out() refuses what it cannot show", {
  skip_if_no_x13()

  expect_error(out(AirPassengers), "class of x is not supported")

  m <- seas(cbind(mdeaths, fdeaths), x11 = "")
  expect_error(out(m), "Not a composite object")

  with_htmlmode(0, {
    expect_error(out(test_model("x11")), "Non HTML mode is not supported")
  })
})

test_that("out() returns the path to the html output", {
  skip_if_no_x13()

  f <- out(test_model("x11"), browser = NULL)

  expect_type(f, "character")
  expect_true(file.exists(f))
  expect_match(f, "\\.html$")
})

test_that("parse_spec() splits a spec into its arguments", {
  z <- seasonal:::parse_spec(
    "regression variables = ( const td ) aictest = ( easter ) "
  )

  expect_type(z, "list")
  expect_true(length(z) >= 1)
})

test_that("extractors work on a single model", {
  skip_if_no_x13()

  m <- test_model("x11")

  expect_s3_class(trend(m), "ts")
  expect_s3_class(irregular(m), "ts")
  expect_equal(length(trend(m)), length(final(m)))
})

test_that("several user regressors are numbered", {
  skip_if_no_x13()

  set.seed(1)
  a <- ts(rnorm(240), start = 1949, frequency = 12)
  b <- ts(rnorm(240), start = 1949, frequency = 12)

  m <- seas(AirPassengers, xreg = cbind(a, b), regression.usertype = "ao")

  expect_equal(spc(m)$regression$user, c("xreg1", "xreg2"))
})

test_that("user regressors can go into the x11regression spec", {
  skip_if_no_x13()

  set.seed(2)
  a <- ts(rnorm(240), start = 1949, frequency = 12)

  m <- seas(
    AirPassengers,
    x11 = "",
    xreg = a,
    regression.aictest = NULL,
    x11regression.variables = "td"
  )

  expect_equal(spc(m)$x11regression$user, "xreg")
  expect_match(spc(m)$x11regression$format, "datevalue")
})

test_that("several transformation series are numbered", {
  skip_if_no_x13()

  m <- seas(
    AirPassengers,
    xtrans = cbind(sqrt(AirPassengers), AirPassengers^2),
    transform.function = "log",
    transform.type = "temporary"
  )

  expect_equal(spc(m)$transform$name, c("xtrans1", "xtrans2"))
})

test_that("arima.model wins over automdl and pickmdl", {
  skip_if_no_x13()

  m <- seas(
    AirPassengers,
    arima.model = "(0 1 1)(0 1 1)",
    automdl = "",
    regression.aictest = NULL
  )

  expect_null(spc(m)$automdl)
  expect_true("arima" %in% names(spc(m)))
})

test_that("siratio() adds the components without a log transform", {
  skip_if_no_x13()

  m <- seas(AirPassengers, x11 = "", transform.function = "none")

  si <- siratio(m)
  expect_equal(
    as.numeric(si),
    as.numeric(na.omit(m$data[, "irregular"] + m$data[, "seasonal"])),
    tolerance = 1e-8
  )

  # and falls back to zero when the components are gone
  m2 <- m
  m2$data <- m2$data[, setdiff(colnames(m2$data), c("seasonal", "irregular"))]
  expect_true(all(siratio(m2) == 0))
})
