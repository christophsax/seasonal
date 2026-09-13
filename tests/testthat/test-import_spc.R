# Parsing problems in real world .spc files, reported in #338


test_that("import.spc() accepts the legacy 'x12save' format label", {

  txt <- '
    series{
      file="data.dta"
      format=x12save
      period=12
      start=2000.01
    }
    x11{}
  '

  spc <- import.spc(text = txt)
  expect_match(paste(deparse(spc$x), collapse = ""), 'format = "x13save"',
               fixed = TRUE)

})


test_that("import.ts() accepts the legacy 'x12save' format label", {

  file <- tempfile(fileext = ".dta")
  on.exit(unlink(file))

  writeLines(c(
    "date\tiofile.s10",
    "------\t-----------------------",
    "200001\t+0.100000000000000E+01",
    "200002\t+0.200000000000000E+01",
    "200003\t+0.300000000000000E+01"
  ), file)

  expect_equal(
    as.numeric(import.ts(file, format = "x12save")),
    c(1, 2, 3)
  )

})


test_that("import.spc() ignores lines that contain nothing but a tab", {

  txt <- "series{\n  data=(1 2 3)\n  start=2000.01\n}\nx11{\n  seasonalma=s3x5\n\t\n}\n"

  expect_no_error(import.spc(text = txt))

})


test_that("import.spc() does not put tabs into argument names", {

  txt <- "series{\n  data=(1 2 3)\n  start=2000.01\n}\ntransform{constant = 1e9 \n\tfunction=log}\n"

  spc <- import.spc(text = txt)

  args <- names(as.list(spc$seas))[-1]
  expect_equal(args, make.names(args))

})


test_that("import.spc() reads files with a non-UTF-8 comment", {

  file <- tempfile(fileext = ".spc")
  on.exit(unlink(file))

  # "am\xe9liore" is 'améliore' in latin1, as found in legacy .spc files
  writeLines(c(
    "# Changer ARIMA, am\xe9liore LBQ, mais pas FE",
    "series{",
    "  data=(1 2 3)",
    "  start=2000.01",
    "}",
    "x11{}"
  ), file, useBytes = TRUE)

  expect_no_error(import.spc(file))

})
