# Path to a file in tests/testthat/fixtures. Used by the fast tests and by
# the extensive ones.
fixture <- function(...) {
  testthat::test_path("fixtures", ...)
}

# Most of seasonal can be exercised without running X-13 again and again.
# These four models cover the branches the methods take: SEATS and X-11, a
# run with user regressors and fixed coefficients, and a quarterly series.
# They are built once and reused by every test that needs a model.
test_model <- local({
  cache <- list()
  function(which = c("seats", "x11", "regressors", "quarterly")) {
    which <- match.arg(which)

    if (!is.null(cache[[which]])) {
      return(cache[[which]])
    }

    m <- switch(
      which,
      seats = seas(AirPassengers),
      x11 = seas(AirPassengers, x11 = ""),
      regressors = seas(
        AirPassengers,
        transform.function = "log",
        regression.variables = c("td", "ao1951.May", "easter[8]"),
        regression.aictest = NULL,
        arima.model = "(0 1 1)(0 1 1)",
        outlier = NULL
      ),
      quarterly = seas(austres)
    )

    cache[[which]] <<- m
    m
  }
})

# The model based tests need X-13, but not the extensive gate: a handful of
# runs is a few seconds.
skip_if_no_x13 <- function() {
  testthat::skip_on_cran()
  testthat::skip_if(
    !x13binary::supportedPlatform(),
    "platform not supported by x13binary"
  )
}

# Run plotting code on a null device, so that nothing is written and no
# window opens.
with_null_device <- function(code) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(code)
}

# Run code with X13_PATH pointing somewhere else, then put it back.
with_x13_path <- function(path, code) {
  old <- Sys.getenv("X13_PATH", unset = NA)
  Sys.setenv(X13_PATH = path)
  on.exit(
    {
      if (is.na(old)) Sys.unsetenv("X13_PATH") else Sys.setenv(X13_PATH = old)
    },
    add = TRUE
  )
  force(code)
}

# Several parsing helpers branch on the 'htmlmode' option, which is set by
# checkX13() at load time. Set it explicitly when testing them directly.
with_htmlmode <- function(mode, code) {
  old <- getOption("htmlmode")
  options(htmlmode = mode)
  on.exit(options(htmlmode = old), add = TRUE)
  force(code)
}
