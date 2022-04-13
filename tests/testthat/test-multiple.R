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

test_that("composite works", {
  seas(
    cbind(mdeaths, fdeaths),
    composite = list(),
    series.comptype = "add"
  )
})


test_that("series() works on composite #278", {
  # https://github.com/christophsax/seasonal/issues/278

  m0 <- seas(
    cbind(mdeaths, fdeaths),
    composite = list(),
    series.comptype = "add"
  )

  a <- series(m0, "composite.indseasadj")


  m1 <- seas(
    cbind(mdeaths, fdeaths),
    composite = list(composite.save = "isa"),
    series.comptype = "add"
  )

  b <- m1$composite$series$isa
  b2 <- series(m1, "composite.indseasadj")

  expect_equal(a, b)
  expect_equal(a, b2)

})




test_that("long names can be used as inputs", {
  # https://github.com/christophsax/seasonal/issues/274

  tt <- list("National.retail.sales..CNY..Monthly" = AirPassengers,
              "National.retail.sales..catering..CNY..Monthly" = AirPassengers + 200)
  ans <- seas(tt)

  a <- final(ans$Nationalretailsalesc)
  b <- final(seas(tt[[2]]))
  expect_equal(a, b)


  # MTS input
  tt2 <- do.call("cbind", tt)
  ans2 <- seas(tt2)
  expect_false(identical(coef(ans2[[1]]), coef(ans2[[2]])))
  expect_equal(final(ans2), final(ans))
})


