skip_on_cran()
if (!x13binary::supportedPlatform()) skip("platform not supported")

library(testthat)
library(seasonal)


test_that("the call is an attribute, not an element #288", {
  m <- seas(cbind(fdeaths, mdeaths), x11 = "")

  expect_equal(names(m), c("fdeaths", "mdeaths"))
  expect_true(is.call(attr(m, "call")))

  # every element is a model, so lapply() needs no Filter()
  expect_equal(names(lapply(m, udg)), c("fdeaths", "mdeaths"))

  # the call used to end up in the output of original()
  expect_equal(colnames(original(m)), c("fdeaths", "mdeaths"))
  expect_equal(original(m)[, "fdeaths"], fdeaths)
})


test_that("series() works without a composite #306", {
  # https://github.com/christophsax/seasonal/issues/306
  m <- seas(cbind(fdeaths, mdeaths), x11 = "")

  z <- series(m, "x11.seasadj")
  expect_equal(colnames(z), c("fdeaths", "mdeaths"))
  expect_equal(z[, "fdeaths"], series(m$fdeaths, "x11.seasadj"))

  # one column per model and series
  expect_equal(
    colnames(series(m, c("x11.seasadj", "x11.trend"))),
    c("fdeaths.d11", "fdeaths.d12", "mdeaths.d11", "mdeaths.d12")
  )

  # a model that cannot produce the series is not silently left out
  m2 <- seas(cbind(fdeaths, mdeaths), list = list(list(x11 = ""), list()))
  expect_error(series(m2, "x11.seasadj"), "adjustment method")
})


test_that("series() re-evaluates the models in a single run #306", {
  m <- seas(cbind(fdeaths, mdeaths))

  n_runs <- 0L
  trace(
    "x13_run",
    tracer = function() n_runs <<- n_runs + 1L,
    print = FALSE,
    where = asNamespace("seasonal")
  )
  z <- NULL
  expect_message(z <- series(m, "fct"), "To speed up")
  untrace("x13_run", where = asNamespace("seasonal"))

  # the batch mode of X-13 is kept, one run for all models
  expect_equal(n_runs, 1L)
  expect_equal(
    colnames(z),
    c("fdeaths.forecast", "fdeaths.lowerci", "fdeaths.upperci",
      "mdeaths.forecast", "mdeaths.lowerci", "mdeaths.upperci")
  )
  # the names of the series survive the re-evaluation
  expect_equal(names(m), c("fdeaths", "mdeaths"))
})


test_that("series() on a composite is unchanged", {
  m <- seas(cbind(mdeaths, fdeaths), composite = list(), series.comptype = "add")

  # the tables of the composite model, the total, not the components
  z <- series(m, "seats.seasonaladj")
  expect_s3_class(z, "ts")
  expect_false(inherits(z, "mts"))
  expect_equal(z, series(m$composite, "seats.seasonaladj"))

  ind <- NULL
  expect_message(ind <- series(m, "composite.indseasadj"), "To speed up")
  expect_s3_class(ind, "ts")
})


test_that("residuals() and predict() work on several models", {
  m <- seas(cbind(fdeaths, mdeaths), x11 = "")

  r <- residuals(m)
  expect_equal(colnames(r), c("fdeaths", "mdeaths"))
  # models start at different dates, cbind() pads with NA, as in final()
  r1 <- residuals(m$fdeaths)
  expect_equal(window(r[, "fdeaths"], start = start(r1)), r1)

  expect_equal(predict(m), final(m))
  expect_error(predict(m, newdata = fdeaths), "update")
})


test_that("fivebestmdl() does not silently return NULL", {
  m <- seas(cbind(fdeaths, mdeaths))
  expect_error(fivebestmdl(m))
  expect_s3_class(lapply(m, fivebestmdl)$fdeaths, "data.frame")
})
