library(testthat)
library(seasonal)

skip_on_cran()

test_that("Multiple series and specs work correctly", {
  mdta <- cbind(a = AirPassengers, b = AirPassengers)

  # Test multiple series, one spec
  expect_no_error(seas(x = mdta, x11 = ""))

  # Test R multimode
  expect_no_error(seas(x = mdta, x11 = "", multimode = "R"))
  expect_no_error(seas(x = mdta, x11 = "", multimode = "x13"))

  # Test series as a list
  m <- seas(x = list(a = mdeaths, b = AirPassengers), x11 = "", list = list(list(), list(outlier.critical = 3)))

  m_a <- seas(mdeaths, x11 = "")
  m_b <- seas(AirPassengers, x11 = "", outlier.critical = 3)

  expect_equal(final(m$a), final(m_a))
  expect_equal(final(m$b), final(m_b))
  expect_equal(original(m$a), original(m_a))
  expect_equal(original(m$b), original(m_b))
  expect_equal(trend(m$a), trend(m_a))
  expect_equal(trend(m$b), trend(m_b))
  expect_equal(irregular(m$a), irregular(m_a))
  expect_equal(irregular(m$b), irregular(m_b))

  # Test using list =
  expect_no_error(seas(x = mdta, list = list(x11 = "")))

  # Test multiple series, multiple specs
  expect_no_error(
    seas(
      x = mdta,
      list = list(
        list(x11 = ""),
        list()
      )
    )
  )

  # Test specifying some specs.args for all series
  expect_no_error(
    seas(
      x = mdta,
      regression.aictest = NULL,
      list = list(
        list(x11 = ""),
        list()
      )
    )
  )

  # Test using x in lists
  expect_no_error(
    seas(
      list = list(
        list(x = AirPassengers, x11 = ""),
        list(x = AirPassengers)
      )
    )
  )
})

test_that("Composite specs work correctly", {
  # Test same spec for all series
  m <- seas(
    cbind(mdeaths, fdeaths),
    composite = list(),
    series.comptype = "add"
  )
  expect_s3_class(m, "seas_multi")

  m <- seas(
    cbind(mdeaths, fdeaths),
    series.comptype = "add",
    composite = list(x11 = "")
  )
  expect_s3_class(m, "seas_multi")

  # Test different spec for all series
  m <- seas(
    cbind(mdeaths, fdeaths),
    series.comptype = "add",
    composite = list(
      regression.aictest = NULL,
      x11.seasonalma = "s3x9"
    ),
    list = list(
      list(x11 = ""),
      list()
    )
  )
  expect_s3_class(m, "seas_multi")
})

test_that("Extractor functions work correctly", {
  m <- seas(x = cbind(mdeaths, fdeaths), x11 = "")

  expect_s3_class(final(m), "mts")
  expect_s3_class(original(m), "mts")
  expect_s3_class(trend(m), "mts")
  expect_s3_class(irregular(m), "mts")
})
