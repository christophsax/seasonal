# Helpers for the extensive tests
#
# These take several minutes, so they only run if asked for. Set
#
#   SEASONAL_EXTENSIVE_TESTS=true
#
# to run them. On CI, this is done by the 'extensive-tests' job, which runs
# after R CMD check has passed.

skip_if_not_extensive <- function() {
  testthat::skip_on_cran()
  testthat::skip_if(
    !x13binary::supportedPlatform(),
    "platform not supported by x13binary"
  )
  testthat::skip_if_not(
    identical(Sys.getenv("SEASONAL_EXTENSIVE_TESTS"), "true"),
    "set SEASONAL_EXTENSIVE_TESTS=true to run the extensive tests"
  )
}

# Reference value for the numerical regression tests in
# test-extensive-cases.R. A csv and not an rds, so that regenerating it shows
# up as a readable diff. Regenerate with
#
#   SEASONAL_REGENERATE_BENCHMARK=true Rscript noinst/extensive/generate.R
benchmark_final <- local({
  cache <- NULL
  function(i) {
    if (is.null(cache)) {
      flat <- utils::read.csv(fixture("extensive-benchmark.csv"))
      cache <<- lapply(split(flat, flat$case), function(d) {
        stats::ts(
          d$value,
          start = c(d$year[1], d$period[1]),
          frequency = d$frequency[1]
        )
      })
    }
    cache[[as.character(i)]]
  }
})

# Compare a model against the stored benchmark. Some runs carry an
# 'na.action' attribute recording which observations X-13 dropped; that is
# incidental to the numbers, and a csv cannot round trip it, so it goes.
expect_matches_benchmark <- function(m, i) {
  actual <- final(m)
  attr(actual, "na.action") <- NULL
  testthat::expect_equal(actual, benchmark_final(i), tolerance = 1e-6)
}

# Two way parsing: write the model to an spc file, read it back with
# import.spc() and check that the reconstructed call gives the same series.
expect_spc_roundtrip <- function(m) {
  env <- parent.frame()

  cl <- m$call
  tdir <- tempfile()
  dir.create(tdir)
  cl$out <- TRUE
  cl$dir <- tdir

  written <- suppressMessages(eval(cl, env))

  z <- import.spc(file.path(tdir, "iofile.spc"))

  # the data calls must be evaluated before the seas() call can use them
  renv <- new.env(parent = env)
  for (nm in c("x", "xreg", "xtrans")) {
    if (!is.null(z[[nm]])) assign(nm, eval(z[[nm]], renv), envir = renv)
  }
  reparsed <- eval(z$seas, renv)

  testthat::expect_equal(
    final(reparsed), final(written),
    tolerance = 1e-6
  )
}
