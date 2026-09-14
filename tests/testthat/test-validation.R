# Argument checking and the error messages users are most likely to meet

test_that("genhol() checks its arguments", {
  data(holiday, package = "seasonal", envir = environment())

  expect_error(genhol(1:10), "must be of class 'Date'")
  expect_error(genhol(easter, center = "nonsense"), "wrong center argument")
  expect_error(genhol(easter, start = 10, end = -10), "start cannot be after end")
  expect_error(genhol(easter, start = -200, end = 0), "must =< 150")
})

test_that("genhol() centers on the mean", {
  data(holiday, package = "seasonal", envir = environment())

  z <- genhol(easter, start = -10, end = -1, center = "mean")

  expect_s3_class(z, "ts")
  expect_equal(mean(z), 0, tolerance = 1e-8)
})

test_that("arima.model is checked before the run", {
  skip_if_no_x13()

  expect_error(seas(AirPassengers, arima.model = TRUE), "must be numeric or character")
  expect_error(
    seas(AirPassengers, arima.model = c(0, 1)),
    "only numeric vector of length 3 or 6 allowed"
  )
})

test_that("a non seasonal arima model is accepted as a length three vector", {
  skip_if_no_x13()

  m <- seas(AirPassengers, arima.model = c(0, 1, 1), regression.aictest = NULL)
  expect_s3_class(m, "seas")
})

test_that("a spec given as a string cannot also take dotted arguments", {
  skip_if_no_x13()

  expect_error(
    seas(AirPassengers, x11 = "notalist", x11.seasonalma = "s3x5"),
    "without a dot is not allowed"
  )
})

test_that("a spec argument is a spec and at most one argument", {
  skip_if_no_x13()

  expect_error(
    seas(AirPassengers, x11.one.two = ""),
    "spec and an optional argument"
  )
})

test_that("series() rejects a series it does not know", {
  skip_if_no_x13()

  expect_error(series(test_model("x11"), "nonsense"), "series not valid")
})

test_that("series() says when the spec is not activated", {
  skip_if_no_x13()

  # a SEATS model has no x11 tables
  expect_error(series(test_model("seats"), "d10"), "is not activated")
})

test_that("summary() prints the adjustment method", {
  skip_if_no_x13()

  x11 <- capture.output(print(summary(test_model("x11"))))
  expect_match(x11, "X11 adj.", all = FALSE)

  m <- test_model("x11")
  m$spc$x11 <- NULL
  m$spc$seats <- NULL
  expect_match(
    capture.output(print(summary(m))),
    "No adj.",
    all = FALSE
  )
})

test_that("summary() prints models without coefficients", {
  skip_if_no_x13()

  s <- summary(test_model("x11"))
  s$coefficients <- NULL

  expect_match(capture.output(print(s)), "No Coefficients", all = FALSE)
})

test_that("seas(dir = ) copies the output files", {
  skip_if_no_x13()

  target <- file.path(tempdir(), "seas-dir-test")
  unlink(target, recursive = TRUE)

  expect_message(
    seas(AirPassengers, x11 = "", dir = target),
    "output files have been copied"
  )
  expect_true(length(list.files(target)) > 0)
})

test_that("print() of an import.spc shows the calls", {
  z <- import.spc(fixture("spc", "Testairline.spc"))

  out <- capture.output(print(z))
  expect_match(out, "main call to 'seas'", all = FALSE)
  expect_match(out, "seas\\(", all = FALSE)
})

test_that("print() of an import.spc shows the input series too", {
  tdir <- tempfile()
  dir.create(tdir)
  writeLines(
    c(
      "series{",
      "  file = \"data.dta\"",
      "  format = \"datevalue\"",
      "}",
      "x11{}"
    ),
    file.path(tdir, "io.spc")
  )

  z <- import.spc(file.path(tdir, "io.spc"))
  out <- capture.output(print(z))

  expect_match(out, "import input series", all = FALSE)
})

test_that("import.ts() rejects a format it does not know", {
  expect_error(
    import.ts(fixture("spc", "free1.txt"), format = "nonsense"),
    "no valid format"
  )
})

test_that("import.ts() checks a tramo file", {
  bad <- tempfile(fileext = ".txt")
  writeLines(c("title", "1 2 3"), bad)

  expect_error(
    import.ts(bad, format = "tramo"),
    "line 2 must have 4 elements"
  )
})

test_that("parse_spc() wants an assignment in every argument", {
  expect_error(
    seasonal:::parse_spc(c("series{", "  nonsense", "}")),
    "expected '='"
  )
})

test_that("out() on a composite model shows the composite output", {
  skip_if_no_x13()

  m <- seas(
    cbind(mdeaths, fdeaths),
    composite = list(),
    series.comptype = "add"
  )

  expect_message(
    out(m, browser = NULL, some.arg = "ignored"),
    "are ignored"
  )

  f <- out(m, browser = NULL)
  expect_true(file.exists(f))
})

test_that("checkX13() reports a binary it cannot run", {
  skip_on_cran()

  broken <- file.path(tempdir(), "broken-x13")
  dir.create(broken, showWarnings = FALSE)
  # a file with the right name that is not a program. On Windows, checkX13()
  # only looks for names ending in .exe.
  bin <- file.path(
    broken,
    if (.Platform$OS.type == "windows") "x13ashtml.exe" else "x13ashtml"
  )
  writeLines("not a binary", bin)
  Sys.chmod(bin, "0644")

  with_x13_path(broken, {
    msg <- capture.output(
      suppressWarnings(checkX13()),
      type = "message"
    )
    expect_match(msg, "Error", all = FALSE)
  })
})
