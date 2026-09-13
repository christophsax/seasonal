# Import of X-13 data and spc files
#
# Reads a range of X-13 data formats as time series, and parses a range of
# actual X-13 spc files. Parsing is pure text work, so this does not need
# X-13 and runs in the normal suite.

test_that("datevalue files are read", {
  expect_s3_class(import.ts(fixture("spc", "datavalue1.dta")), "ts")
  expect_s3_class(import.ts(fixture("spc", "datavalue2.dta")), "ts")

  q <- import.ts(fixture("spc", "datavalue_q.dta"))
  expect_equal(frequency(q), 4)

  expect_s3_class(import.ts(fixture("spc", "datavalue_mult0.dta")), "ts")
  expect_s3_class(import.ts(fixture("spc", "datavalue_mult1.dta")), "ts")
})

test_that("datevaluecomma files are read", {
  x <- import.ts(fixture("spc", "datavaluecomma.dta"), format = "datevaluecomma")
  expect_s3_class(x, "ts")
})

test_that("free format files are read", {
  x1 <- import.ts(
    fixture("spc", "free1.txt"),
    format = "free", start = c(1949, 1), frequency = 12
  )
  x2 <- import.ts(
    fixture("spc", "free2.txt"),
    format = "free", start = c(1949, 1), frequency = 12
  )
  expect_equal(frequency(x1), 12)
  expect_equal(frequency(x2), 12)
  expect_equal(start(x1), c(1949, 1))
})

test_that("freecomma format files are read", {
  x1 <- import.ts(
    fixture("spc", "freecomma1.txt"),
    format = "freecomma", start = c(1949, 1), frequency = 12
  )
  x2 <- import.ts(
    fixture("spc", "freecomma2.txt"),
    format = "freecomma", start = c(1949, 1), frequency = 12
  )
  expect_equal(frequency(x1), 12)
  expect_equal(frequency(x2), 12)
})

test_that("x13save files are read", {
  m <- import.ts(fixture("spc", "x13save.dta"), format = "x13save")
  q <- import.ts(fixture("spc", "x13save_q.dta"), format = "x13save")
  expect_equal(frequency(m), 12)
  expect_equal(frequency(q), 4)
})

test_that("X-11 fixed column formats are read", {
  # 1l gives a harmless 'incomplete final line found' warning
  m1l <- suppressWarnings(
    import.ts(fixture("spc", "x11_m1l.dat"), format = "1l", frequency = 12)
  )
  expect_equal(frequency(m1l), 12)

  m2l <- import.ts(fixture("spc", "x11_m2l.dat"), format = "2l", frequency = 12)
  expect_equal(frequency(m2l), 12)

  m2l2 <- import.ts(fixture("spc", "x11_m2l2.dat"), format = "2l2", frequency = 12)
  expect_equal(frequency(m2l2), 12)

  m1r <- import.ts(fixture("spc", "x11_m1r.dat"), format = "1r", frequency = 12)
  expect_equal(frequency(m1r), 12)
})

test_that("quarterly X-11 fixed column formats are read", {
  q1r <- import.ts(fixture("spc", "x11_q1r.dat"), format = "1r", frequency = 4)
  expect_equal(frequency(q1r), 4)

  # two series in one file
  q2ser <- import.ts(
    fixture("spc", "x11_q1r_2ser.dat"), format = "1r", frequency = 4
  )
  expect_equal(frequency(q2ser), 4)
})

test_that("tramo files are read", {
  x <- import.ts(fixture("spc", "tramo1.txt"), format = "tramo")
  expect_s3_class(x, "ts")
})

test_that("the original X-13 example spc is parsed", {
  z <- import.spc(fixture("spc", "Testairline.spc"))
  expect_s3_class(z, "import.spc")
  expect_true(is.call(z$seas))
})

test_that("spc files with unit root tests are parsed", {
  expect_s3_class(import.spc(fixture("spc", "urtest0.spc")), "import.spc")
  expect_s3_class(import.spc(fixture("spc", "urtest1.spc")), "import.spc")
})

test_that("X-11 spc files are parsed", {
  expect_s3_class(
    import.spc(fixture("spc", "x11", "SHOERS1982.spc")), "import.spc"
  )
  expect_s3_class(
    import.spc(fixture("spc", "x11", "tdtest.spc")), "import.spc"
  )
  expect_s3_class(
    import.spc(fixture("spc", "x11", "iaiua.spc")), "import.spc"
  )
})

test_that("a composite spc is parsed but cannot be run", {
  # Mw1.spc includes a composite spec, so the resulting call cannot be
  # evaluated; parsing it must still work.
  z <- import.spc(fixture("spc", "x11", "Mw1.spc"))
  expect_s3_class(z, "import.spc")
})
