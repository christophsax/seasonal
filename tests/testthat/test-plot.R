# Plot methods. These draw on a null device; the assertions are that the
# code paths run and that the documented errors are raised.

test_that("plot() draws original and adjusted series", {
  skip_if_no_x13()

  m <- test_model("seats")

  with_null_device({
    expect_no_error(plot(m))
    expect_no_error(plot(m, outliers = FALSE))
    expect_no_error(plot(m, trend = TRUE))
  })
})

test_that("plot() transforms to rates of change", {
  skip_if_no_x13()

  m <- test_model("x11")

  with_null_device({
    expect_no_error(plot(m, transform = "PC"))
    expect_no_error(plot(m, transform = "PCY"))
    expect_no_error(plot(m, transform = "PC", trend = TRUE))
  })

  expect_error(plot(m, transform = "nonsense"))
})

test_that("residplot() draws the regARIMA residuals", {
  skip_if_no_x13()

  m <- test_model("regressors")

  with_null_device({
    expect_no_error(residplot(m))
    expect_no_error(residplot(m, outliers = FALSE))
  })
})

test_that("monthplot() draws the seasonal and irregular components", {
  skip_if_no_x13()

  m <- test_model("x11")

  with_null_device({
    expect_no_error(monthplot(m))
    expect_no_error(monthplot(m, choice = "irregular"))
    expect_no_error(monthplot(m, main = "custom title"))
  })
})

test_that("monthplot() refuses a model without the component", {
  skip_if_no_x13()

  m <- test_model("x11")
  m$data <- m$data[, setdiff(colnames(m$data), c("seasonal", "irregular"))]

  expect_error(monthplot(m), "no seasonal component")
  expect_error(monthplot(m, choice = "irregular"), "no irregular component")
})

test_that("siratio() combines seasonal and irregular by transform function", {
  skip_if_no_x13()

  m <- test_model("x11")

  si <- siratio(m)
  expect_s3_class(si, "ts")
  expect_equal(frequency(si), 12)

  # with a log transform the two components multiply
  expect_equal(
    as.numeric(si),
    as.numeric(na.omit(m$data[, "irregular"] * m$data[, "seasonal"])),
    tolerance = 1e-8
  )

  # without the components, siratio() falls back to a constant
  m2 <- m
  m2$data <- m2$data[, setdiff(colnames(m2$data), c("seasonal", "irregular"))]
  expect_s3_class(siratio(m2), "ts")
})
