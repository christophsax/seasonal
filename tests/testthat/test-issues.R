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

