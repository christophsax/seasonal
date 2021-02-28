# These tests are extensive and need not to run on CRAN.


# for CRAN SUBMISSION:
# - is NEWS and DESCRIPTION up to date?

# - knit vignette
# - rhub test

if (!x13binary::supportedPlatform()) skip("platform not supported")
skip("skip extensive tests")


nocran_tests <- file.path("tests", "nocran")

test_that("extensive non CRAN tests succeed", {
  source(file.path(nocran_tests, "settings.R"))
  source(file.path(nocran_tests, "test-import.R"))
  source(file.path(nocran_tests, "test-numerical.R"))
  source(file.path(nocran_tests, "test-static.R"))
  source(file.path(nocran_tests, "test-update.R"))
  source(file.path(nocran_tests, "test-two-way-parsing.R"))
  source(file.path(nocran_tests, "test-multi.R"))
  source(file.path(nocran_tests, "test-issues.R"))
})

