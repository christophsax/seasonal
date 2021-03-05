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
