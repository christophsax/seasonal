# checkX13() and the paths it takes when the installation is broken

test_that("checkX13() reports a working installation", {
  skip_if_no_x13()

  expect_null(checkX13())
  expect_message(checkX13(), "X-13 installation test")
  expect_message(checkX13(), "seasonal test run successful")
})

test_that("checkX13() can skip the long checks", {
  skip_if_no_x13()

  expect_null(checkX13(fullcheck = FALSE, htmlcheck = FALSE))
  expect_null(checkX13(fullcheck = FALSE))
  expect_null(checkX13(htmlcheck = FALSE))
})

test_that("checkX13() complains about a path that does not exist", {
  with_x13_path(file.path(tempdir(), "no-such-directory"), {
    expect_error(
      suppressMessages(checkX13(fail = TRUE)),
      "Process terminated"
    )
    expect_message(try(checkX13(fail = TRUE), silent = TRUE), "specified but does not exists")

    # without fail = TRUE it is a startup message and a NULL return
    expect_null(suppressMessages(checkX13()))
  })
})

test_that("checkX13() complains when the binary is missing from the path", {
  empty <- file.path(tempdir(), "empty-x13-dir")
  dir.create(empty, showWarnings = FALSE)

  with_x13_path(empty, {
    expect_error(
      suppressMessages(checkX13(fail = TRUE)),
      "Process terminated"
    )
    expect_message(try(checkX13(fail = TRUE), silent = TRUE), "Binary executable file")
    expect_null(suppressMessages(checkX13()))
  })
})

test_that("get_x13_path() prefers the environment variable", {
  with_x13_path("/some/where", {
    expect_equal(seasonal:::get_x13_path(), "/some/where")
  })
})
