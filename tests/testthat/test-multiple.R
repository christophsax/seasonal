skip_on_cran()
if (!x13binary::supportedPlatform()) skip("platform not supported")

library(testthat)
library(seasonal)


test_that("multirun works", {

  m1 <- seas(cbind(fdeaths, mdeaths), x11 = "")
  expect_s3_class(final(m1), "mts")

  m2 <- seas(
    cbind(fdeaths, mdeaths),
    list = list(
      list(x11 = ""),
      list()
    )
  )
  expect_s3_class(final(m2), "mts")


  m3 <- seas(
    cbind(fdeaths, mdeaths),
    regression.aictest = NULL,
    list = list(
      list(x11 = ""),
      list()
    )
  )
  expect_s3_class(final(m3), "mts")

})
