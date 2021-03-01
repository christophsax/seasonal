# These tests are extensive and need not to run on CRAN.


# for CRAN SUBMISSION:
# - is NEWS and DESCRIPTION up to date?
# - rhub test

skip_on_cran()

if (!x13binary::supportedPlatform()) skip("platform not supported")

if (basename(normalizePath("~")) == "christoph") skip("skip extensive tests locally")

# https://stackoverflow.com/questions/24823353/testthat-in-r-sourcing-in-tested-files
test_that("nocran dir can be found", {
  nocran_tests <<- normalizePath(file.path("../nocran"))
  expect_type(nocran_tests, "character")
})

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

