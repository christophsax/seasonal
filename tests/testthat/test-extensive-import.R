# The one import test that needs an X-13 run: it writes an spc file with
# user defined regression and transformation series, then reads it back.

test_that("a spc with user defined regression and transformation series is parsed", {
  skip_if_not_extensive()

  tdir <- tempfile()
  dir.create(tdir)

  seas(
    x = AirPassengers,
    xreg = cbind(
      a = genhol(cny, start = 1, end = 4, center = "calendar"),
      b = genhol(cny, start = -3, end = 0, center = "calendar")
    ),
    xtrans = cbind(sqrt(AirPassengers), AirPassengers^3),
    transform.function = "log",
    transform.type = "temporary",
    regression.aictest = "td",
    regression.usertype = "holiday",
    dir = tdir,
    out = TRUE
  )

  z <- import.spc(file.path(tdir, "iofile.spc"))
  expect_s3_class(z, "import.spc")
  expect_true(is.call(z$xreg))
  expect_true(is.call(z$xtrans))
})

