# Accessors and methods on a "seas" object

test_that("outlier() returns a series with the outliers labelled", {
  skip_if_no_x13()

  m <- test_model("seats")
  ol <- outlier(m)

  expect_s3_class(ol, "ts")
  expect_equal(length(ol), length(final(m)))

  # every non missing entry corresponds to a detected outlier regressor
  detected <- m$model$regression$variables
  detected <- detected[grepl("\\.", detected) & !grepl("/", detected)]
  expect_equal(sum(!is.na(ol)), length(detected))
})

test_that("outlier() labels a declared outlier", {
  skip_if_no_x13()

  ol <- outlier(test_model("regressors"))

  # the model declares ao1951.May and nothing else
  expect_equal(sum(!is.na(ol)), 1)
  expect_equal(as.character(na.omit(as.character(ol))), "AO")
  expect_equal(time(ol)[!is.na(ol)], 1951 + 4 / 12, tolerance = 1e-8)
})

test_that("outlier() returns all NA when the model has no outliers", {
  skip_if_no_x13()

  m <- test_model("regressors")
  m$model$regression$variables <- "td"

  expect_true(all(is.na(outlier(m))))
})

test_that("outlier(full = TRUE) spells the types out", {
  skip_if_no_x13()

  m <- test_model("seats")
  short <- outlier(m)
  full <- outlier(m, full = TRUE)

  expect_equal(is.na(short), is.na(full))
  if (any(!is.na(full))) {
    expect_true(any(nchar(na.omit(as.character(full))) > 2))
  }
})

test_that("outlier() refuses anything that is not a model", {
  expect_error(outlier(AirPassengers))
})

test_that("as.data.frame() puts a date column in front of the series", {
  skip_if_no_x13()

  m <- test_model("x11")
  df <- as.data.frame(m)

  expect_s3_class(df, "data.frame")
  expect_equal(colnames(df)[1], "date")
  expect_s3_class(df$date, "Date")
  expect_equal(nrow(df), nrow(m$data))
  expect_equal(df$date[1], as.Date("1949-01-01"))
})

test_that("as.data.frame() handles quarterly series", {
  skip_if_no_x13()

  df <- as.data.frame(test_model("quarterly"))

  expect_s3_class(df$date, "Date")
  # quarters start in January, April, July and October
  expect_true(all(as.integer(format(df$date, "%m")) %in% c(1, 4, 7, 10)))
})

test_that("Date_ts() refuses an unsupported frequency", {
  weekly <- ts(1:10, start = c(2000, 1), frequency = 52)
  expect_error(seasonal:::Date_ts(weekly), "unsupported frequency")
})

test_that("udg() returns the diagnostics", {
  skip_if_no_x13()

  m <- test_model("seats")
  u <- udg(m)

  expect_type(u, "list")
  expect_true(length(u) > 0)
  expect_true("aicc" %in% names(u))
  expect_type(udg(m, "aicc"), "double")
})

test_that("udg() on a missing statistic fails, or returns nothing", {
  skip_if_no_x13()

  m <- test_model("seats")

  expect_error(udg(m, "nosuchstatistic"), "not contained in")
  expect_null(udg(m, "nosuchstatistic", fail = FALSE))

  # a mix of present and missing keeps the present one
  expect_length(udg(m, c("aicc", "nosuchstatistic"), fail = FALSE), 1)
})

test_that("udg() refuses anything that is not a model", {
  expect_error(udg(AirPassengers))
})

test_that("coef() and print() work on a model", {
  skip_if_no_x13()

  m <- test_model("regressors")

  expect_type(coef(m), "double")
  expect_true(length(coef(m)) > 0)

  out <- capture.output(print(m))
  expect_match(out, "Call:", all = FALSE)
})

test_that("print() of a spclist shows the spc", {
  skip_if_no_x13()

  z <- capture.output(print(spc(test_model("seats"))))
  expect_match(z, "series", all = FALSE)
})

test_that("spc() returns the spc list that was sent to X-13", {
  skip_if_no_x13()

  s <- spc(test_model("x11"))

  expect_s3_class(s, "spclist")
  expect_true("series" %in% names(s))
  expect_true("x11" %in% names(s))
})

test_that("transformfunction() reports the transformation", {
  skip_if_no_x13()

  expect_equal(transformfunction(test_model("regressors")), "log")
  expect_true(transformfunction(test_model("seats")) %in% c("log", "none"))

  m <- test_model("seats")
  m$spc$transform$`function` <- NULL
  expect_error(transformfunction(m), "no transform function")
})

test_that("fivebestmdl() lists five candidate models", {
  skip_if_no_x13()

  f <- fivebestmdl(test_model("seats"))

  expect_s3_class(f, "data.frame")
  expect_equal(nrow(f), 5)
  expect_equal(colnames(f), c("arima", "bic"))
  expect_type(f$bic, "double")
})
