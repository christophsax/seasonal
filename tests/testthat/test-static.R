# static() and update()

test_that("static() returns a call that reproduces the model", {
  skip_if_no_x13()

  m <- test_model("regressors")
  st <- static(m)

  expect_true(is.call(st))
  expect_equal(as.character(st[[1]]), "seas")

  # the automatic parts are now spelled out
  expect_true("arima.model" %in% names(as.list(st)))
  expect_true("regression.variables" %in% names(as.list(st)))
})

test_that("static(evaluate = TRUE) runs the call it builds", {
  skip_if_no_x13()

  m <- test_model("regressors")
  st <- static(m, evaluate = TRUE)

  expect_s3_class(st, "seas")
  expect_equal(final(st), final(m), tolerance = 1e-6)
})

test_that("static(coef = TRUE) fixes the coefficients", {
  skip_if_no_x13()

  st <- static(test_model("regressors"), coef = TRUE)
  l <- as.list(st)

  expect_true("regression.b" %in% names(l))
  # fixed coefficients carry an 'f' suffix
  expect_true(any(grepl("f$", eval(l$regression.b))))
})

test_that("static(x11.filter = TRUE) fixes the x11 filters", {
  skip_if_no_x13()

  st <- static(test_model("x11"), x11.filter = TRUE)
  l <- as.list(st)

  expect_true("x11.seasonalma" %in% names(l))
  expect_true("x11.trendma" %in% names(l))
})

test_that("static(test = FALSE) skips the verification run", {
  skip_if_no_x13()

  expect_true(is.call(static(test_model("seats"), test = FALSE)))
})

test_that("static() refuses anything that is not a model", {
  expect_error(static(AirPassengers), "must be of class 'seas'")
})

test_that("update() re-runs a model with changed arguments", {
  skip_if_no_x13()

  m <- test_model("x11")

  same <- update(m)
  expect_s3_class(same, "seas")
  expect_equal(final(same), final(m), tolerance = 1e-6)

  changed <- update(m, x11.seasonalma = "s3x9")
  expect_s3_class(changed, "seas")
  expect_equal(changed$spc$x11$seasonalma, "s3x9")
})
