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

