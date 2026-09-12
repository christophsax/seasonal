skip_on_cran()
if (!x13binary::supportedPlatform()) skip("platform not supported")

m_x11 <- seas(AirPassengers, arima.model = "(0 1 1)(0 1 1)", x11 = "")
m_seats <- seas(AirPassengers, arima.model = "(0 1 1)(0 1 1)")
m_composite <- seas(cbind(mdeaths, fdeaths), composite = list(), series.comptype = "add")

test_that("reeval_dots resolves unambiguous table names", {
  expect_equal(reeval_dots(m_x11, "d1"), list(x11.save = "d1"))
})

test_that("reeval_dots resolves table names of more than one spec (#289)", {
  expect_equal(reeval_dots(m_x11, "b1"), list(series.save = "b1"))
  expect_equal(reeval_dots(m_composite$composite, "b1"), list(composite.save = "b1"))
  expect_equal(reeval_dots(m_x11, "tac"), list(x11.save = "tac"))
  expect_equal(reeval_dots(m_seats, "tac"), list(seats.save = "tac"))
})

test_that("reeval_dots skips series that are already there", {
  expect_true("d11" %in% names(m_x11$series))
  expect_equal(reeval_dots(m_x11, c("d11", "b1")), list(series.save = "b1"))
})

test_that("reeval_dots adds the options a series requires", {
  expect_equal(
    reeval_dots(m_x11, "sfe", verbose = FALSE),
    list(history.estimates = "seasonal", history.save = "sfe")
  )
})
