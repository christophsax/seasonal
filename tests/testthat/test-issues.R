skip_on_cran()
if (!x13binary::supportedPlatform()) skip("platform not supported")

test_that("defaults are not in m$list (#251)", {
  m <- seas(mdeaths)
  expect_identical(names(m$list), "x")
})

test_that("'fts' import via series works (#240)", {
  m <- seas(AirPassengers, arima.model = '(0 1 1)(0 1 1)', outlier.types = "all", outlier.save = "fts")
  expect_false(any(is.na(m$series$fts[,'t.TC.'])))
})

test_that("update works with out = TRUE (#259)", {
  m <- seas(mdeaths)
  m1 <- update(m, out = TRUE)
  expect_s3_class(m1, "seas")
  expect_true(file.exists(out(m1, browser = NULL)))
  expect_true(file.exists(out(m, browser = NULL)))

})


test_that("list arguments works with coefs", {

  m0 <- seas(
    x = AirPassengers,
    regression.variables = c("td1coef", "easter[1]", "ao1951.May"),
    arima.model = "(0 1 1)(0 1 1)",
    regression.aictest = NULL,
    outlier = NULL,
    transform.function = "log",
    regression.b = c("-0.002949699141f", "0.01776737357f", "0.1001558244f"),
    arima.ma = c("0.1156204139f", "0.4973600193f")
  )

  m1 <- seas(
    list = list(
    x = AirPassengers,
    regression.variables = c("td1coef", "easter[1]", "ao1951.May"),
    arima.model = "(0 1 1)(0 1 1)",
    regression.aictest = NULL,
    outlier = NULL,
    transform.function = "log",
    regression.b = c("-0.002949699141f", "0.01776737357f", "0.1001558244f"),
    arima.ma = c("0.1156204139f", "0.4973600193f")
    )
  )

  expect_equal(final(m0), final(m1))

})

test_that("seas works with forking parallelization #276", {
  skip_on_os("windows")
  library(parallel)
  library(seasonal)
  runs <- mclapply(1:2, function(x) seas(AirPassengers), mc.cores = 2)
  expect_false(any(sapply(runs, inherits, "try-error")))
})

test_that("annual series are read correctly #264", {
  y <- ts(c(41,39.8,40,40.5,37.8,37.3,34.6,36.1,37.1,32.9,35,33.4,34.7,36.2,38.1),freq=1,start=2005)
  m <- seas(y,x11=NULL,seats=NULL,regression.aictest = NULL,forecast.maxlead=1, forecast.save = "fct")
  expect_true(time(m$series$fct)[1] == 2020)
})


test_that("January first can be used in genhol() #261", {
  holiday  <- structure(c(8401, 8766, 9131, 9496, 9862, 10227, 10592, 10957,
  11323, 11688, 12053, 12418, 12784, 13149, 13514, 13879, 14245,
  14610, 14975, 15340, 15706, 16071, 16436, 16801, 17167, 17532,
  17897, 18262, 18628), class = "Date", control = c(method = "trunc",
  FinCenter = "GMT"))
  ans <- seasonal::genhol(x = holiday, frequency = 12, center = "calendar")
  expect_s3_class(ans, "ts")
})


test_that("genhol() produces correct result in 1931 #193", {
  ans <- genhol(easter, start = -46, end = 0, frequency = 12)
  expect_equal(sum(abs(rowSums(matrix(ans, ncol = 12, byrow = TRUE)) - 1)), 0)
})

test_that("missing iofile.est for short series causes an error #296", {
  tdata <- structure(c(52604.81, 53141.78, 59271.6, 54476.88, 66938.94,
                       65500.01, 71684.73, 63713.29, 58863.21, 63600.67, 73312.29, 72956.85,
                       64393.54, 63106.64, 62815.14, 84729.49, 108466.78, 117980.63,
                       125717.44, 125629.39, 102390.65, 121348.14, 169331.13, 134011.12,
                       120678.75, 116665.94, 119349.04, 111402.42, 125400.23, 139325.92,
                       145616.87, 166295.89, 162347.97, 155599.53, 188771.92, 135550.8,
                       119277.33, 106787.06, 126194.51, 125725.55, 149677.71, 153743.23,
                       148186.37, 158082.96, 162852.29), tsp = c(2019, 2022.66666666667,
                                                                 12), class = "ts")
  expect_no_error(seas(tdata))
})


test_that("removing an argument does not create the spec (#293, #294)", {
  spc <- structure(list(), class = c("spclist", "list"))
  expect_length(mod_spclist(spc, list(seats.noadmiss = NULL)), 0)
})


test_that("seats = NULL removes the seats spec (#293, #294)", {
  m <- seas(AirPassengers, seats = NULL)
  expect_false("seats" %in% names(m$spc))
})


test_that("seats = NULL does not limit forecasts and backcasts (#293, #294)", {
  m <- seas(
    AirPassengers,
    seats = NULL,
    forecast.maxlead = 60,
    forecast.maxback = 24,
    forecast.save = c("fct", "bct")
  )
  expect_identical(NROW(series(m, "fct")), 60L)
  expect_identical(NROW(series(m, "bct")), 24L)
})


test_that("series longer than the X-13 limit are reported as such (#287)", {
  expect_silent(check_span(ts(1:(85 * 12), start = c(1930, 1), frequency = 12)))
  expect_error(
    check_span(ts(1:(86 * 12), start = c(1930, 1), frequency = 12)),
    "85 years"
  )
  expect_error(seas(ts(1:(93 * 12), start = c(1930, 1), frequency = 12)), "85 years")
})


test_that("table names that are used by two specs work (#289)", {
  m <- seas(AirPassengers, arima.model = "(0 1 1)(0 1 1)", x11 = "")
  b1 <- suppressMessages(series(m, "b1"))
  expect_s3_class(b1, "ts")
  expect_equal(b1, suppressMessages(series(m, "series.adjoriginal")))
})


test_that("series of the original call survive the re-run (#290)", {
  m <- seas(
    AirPassengers,
    arima.model = "(0 1 1)(0 1 1)",
    x11 = "",
    series.save = "b1"
  )
  z <- suppressMessages(series(m, c("a1", "b1", "d11")))
  expect_equal(colnames(z), c("a1", "b1", "d11"))
})


test_that("a failed spectral plot is a warning, not an error (#337)", {
  # X-13 cannot draw the spectrum of the logged adjusted series if the
  # adjustment turns negative
  x <- ts(
    c(19, 0, -32, -47, -19, 8, 41, 51, 31, -1, -27, -39,
      19, -4, -28, -50, -20, 12, 42, 51, 32, 2, -30, -44,
      21, 0, -30, -53, -21, 11, 43, 50, 31, 0, -33, -41,
      19, 0, -28, -48, -20, 9, 41, 51, 29, -1, -29, -38),
    start = c(2010, 1), frequency = 12
  )
  m <- seas(x, transform.function = "log", transform.constant = 200, x11 = "")
  expect_s3_class(final(m), "ts")
  expect_length(m$err$error, 0)
  expect_match(m$err$warning, "Spectral plot", all = FALSE)
})


test_that("Box-Ljung in summary() reproduces the statistic of X-13 (#310)", {
  # lbq() recomputes what X-13 puts in the acf table of the check spec, so that
  # the table does not have to be saved on every run. Verify they agree.
  cases <- list(
    quote(seas(AirPassengers)),
    quote(seas(AirPassengers, x11 = "")),
    quote(seas(AirPassengers, arima.model = "(2 1 0)(0 1 1)",
               regression.aictest = NULL, outlier = NULL)),
    quote(seas(AirPassengers, arima.model = "(1 0 0)(0 0 0)",
               regression.variables = "const", regression.aictest = NULL,
               outlier = NULL, transform.function = "log")),
    quote(seas(AirPassengers, arima.model = "(0 2 2)(0 1 1)",
               regression.aictest = NULL, outlier = NULL)),
    # all ARMA coefficients fixed, none of them costs a degree of freedom
    quote(seas(AirPassengers, arima.model = "(0 1 1)(0 1 1)",
               arima.ma = c("0.1156f", "0.4974f"), regression.aictest = NULL,
               outlier = NULL, transform.function = "log")),
    # one fixed, one estimated
    quote(seas(AirPassengers, arima.model = "(0 1 1)(0 1 1)",
               arima.ma = c("0.1156f", "0.4974"), regression.aictest = NULL,
               outlier = NULL, transform.function = "log")),
    quote(seas(mdeaths)),
    quote(seas(ts(cumsum(rnorm(160)) + 200, start = c(1980, 1), frequency = 4)))
  )

  set.seed(42)
  for (cl in cases) {
    cl$check.save <- "acf"
    m <- eval(cl)

    acf <- m$series$acf
    last <- acf[NROW(acf), ]
    x13 <- c(statistic = unname(last["Ljung.Box_Q"]),
             parameter = unname(last["df_of_Q"]),
             p.value = unname(last["P.value"]))

    expect_equal(seasonal:::lbq(m), x13, tolerance = 1e-4,
                 info = deparse(cl, width.cutoff = 500))
  }
})

test_that("summary() shows the corrected Box-Ljung statistic (#310)", {
  m <- seas(AirPassengers)

  bl <- seasonal:::lbq(m)
  expect_equal(unname(bl["statistic"]), 23.885, tolerance = 1e-4)

  # 24 lags less the two estimated MA coefficients, not 24 as before
  expect_equal(unname(bl["parameter"]), 22)

  expect_output(print(summary(m)), "Box-Ljung \\(no autocorr.\\): 23.88")

  # the check spec is not saved, summary() costs no extra output file
  expect_null(m$series$acf)
})
