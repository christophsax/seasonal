skip_on_cran()
if (!x13binary::supportedPlatform()) skip("platform not supported")

test_that("glance_df() summarizes a seas object (#192)", {
  m <- seas(AirPassengers, x11 = "")
  gl <- glance_df(summary(m))

  expect_s3_class(gl, "data.frame")
  expect_identical(nrow(gl), 1L)
  expect_identical(gl$adjustment, "X11")
  expect_identical(gl$transform, "log")
  expect_identical(gl$nobs, nobs(m))
  expect_equal(gl$BIC, BIC(m))
  expect_true(all(c("qs", "box.ljung", "shapiro") %in% names(gl)))

  # the same statistic summary() shows: the one X-13 computes, not
  # Box.test(resid(m), lag = 24) (#310)
  expect_equal(gl$box.ljung, unname(lbq(m)["statistic"]))
  expect_equal(gl$box.ljung.df, unname(lbq(m)["parameter"]))
})

test_that("glance_df() reports the adjustment method", {
  expect_identical(glance_df(summary(seas(AirPassengers)))$adjustment, "SEATS")
  expect_identical(
    glance_df(summary(seas(AirPassengers, seats = NULL)))$adjustment,
    "none"
  )
})

test_that("tidier methods are registered lazily (#192)", {
  skip_if_not_installed("generics")

  m <- seas(AirPassengers, x11 = "")

  expect_identical(generics::tidy(m), as.data.frame(summary(m)))
  expect_identical(generics::augment(m), as.data.frame(m))
  expect_identical(generics::glance(m), glance_df(summary(m)))

  # soft dependency: nothing imported, nothing exported
  hard <- read.dcf(
    system.file("DESCRIPTION", package = "seasonal"),
    fields = c("Depends", "Imports")
  )
  expect_false(any(grepl("generics|broom", hard)))
  expect_false("tidy.seas" %in% getNamespaceExports("seasonal"))
})
