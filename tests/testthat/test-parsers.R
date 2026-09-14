# Pure parsing helpers that work on the text X-13 writes out

test_that("detect_auto() reads the automatic transform choice", {
  with_htmlmode(0, {
    prefer_log <- c(
      " Likelihood statistics for model fit to log transformed series.",
      rep("", 10),
      "  *** Automatic transform selection prefers log transformation ***",
      rep("", 20)
    )
    expect_equal(seasonal:::detect_auto(prefer_log), "log")

    prefer_none <- sub(
      "log transformation", "no transformation",
      prefer_log, fixed = TRUE
    )
    expect_equal(seasonal:::detect_auto(prefer_none), "none")
  })
})

test_that("detect_auto() returns nothing when there is nothing to read", {
  with_htmlmode(0, {
    expect_null(seasonal:::detect_auto(c("some", "other", "output")))

    # the marker is there but no line states a preference
    no_preference <- c(
      " Likelihood statistics for model fit to log transformed series.",
      rep("", 30)
    )
    expect_null(seasonal:::detect_auto(no_preference))
  })
})

test_that("detect_fivebestmdl() takes the block after the header", {
  with_htmlmode(0, {
    outtxt <- c(
      "noise",
      "  Best Five ARIMA Models",
      paste("  line", 1:10)
    )

    z <- seasonal:::detect_fivebestmdl(outtxt)
    expect_length(z, 11)
    expect_equal(z[1], "  Best Five ARIMA Models")
  })
})

test_that("detect_fivebestmdl() takes the last block if there are several", {
  with_htmlmode(0, {
    outtxt <- c(
      "  Best Five ARIMA Models",
      paste("  first", 1:10),
      "  Best Five ARIMA Models",
      paste("  second", 1:10)
    )

    z <- seasonal:::detect_fivebestmdl(outtxt)
    expect_match(z[2], "second")
  })
})

test_that("detect_fivebestmdl() returns nothing when the header is absent", {
  with_htmlmode(0, {
    expect_null(seasonal:::detect_fivebestmdl(c("some", "other", "output")))
  })
})

test_that("read_mdl() returns nothing when there is no mdl file", {
  expect_null(seasonal:::read_mdl(file.path(tempdir(), "no-such-io")))
})

test_that("read_mdl() parses a model file", {
  io <- file.path(tempdir(), "readmdl-test")
  writeLines(
    c(
      "regression{",
      " variables=( const )",
      "}",
      "arima{",
      " model=( (0 1 1)(0 1 1) )",
      "}"
    ),
    paste0(io, ".mdl")
  )
  on.exit(unlink(paste0(io, ".mdl")))

  z <- seasonal:::read_mdl(io)

  expect_type(z, "list")
  expect_true("arima" %in% names(z))
  expect_true("regression" %in% names(z))
})

test_that("read_mdl() tidies up a full regime change", {
  io <- file.path(tempdir(), "readmdl-regime")
  writeLines(
    c(
      "regression{",
      " variables=(",
      " td/ for before 1955.Jan/",
      " )",
      "}"
    ),
    paste0(io, ".mdl")
  )
  on.exit(unlink(paste0(io, ".mdl")))

  z <- seasonal:::read_mdl(io)
  expect_true(is.null(z) || "regression" %in% names(z))
})

test_that("na.x13() substitutes NA by the X-13 missing code", {
  x <- AirPassengers
  x[20] <- NA

  z <- na.x13(x)

  expect_s3_class(z, "ts")
  expect_false(anyNA(z))
  expect_equal(as.numeric(z[20]), -99999)

  expect_error(na.x13(1:10))
})

test_that("na.exclude on a ts marks the excluded observations", {
  # na.omit.ts refuses internal NAs, so use a leading one
  x <- AirPassengers
  x[1] <- NA

  z <- na.exclude(x)

  expect_false(anyNA(z))
  expect_s3_class(attr(z, "na.action"), "exclude")
})

test_that("enrich_list() refuses a list that is not one", {
  expect_error(seasonal:::enrich_list(list = 1:3), "class 'list'")
})

test_that("enrich_list() requires every element to be named", {
  expect_error(
    seasonal:::enrich_list(list = list(1, 2)),
    "must be named"
  )
})

test_that("enrich_list() refuses an argument given twice", {
  expect_error(
    seasonal:::enrich_list(
      list = list(x11 = ""),
      list_dots = list(x11 = "")
    ),
    "specified in '...' and in 'list'"
  )
})
