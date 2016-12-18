# for CRAN SUBMISSION:
# - is NEWS and DESCRIPTION up to date?

# - remove test-all.R
# - remove img links in README.md

# - knit vignette
# - r-devel test

# travisdir <- "~/git/seasonal/travis"
# These tests are more extensive and only need to run on travis, not on CRAN.

seasonal::checkX13()
x13binary::checkX13binary()

if (Sys.getenv("TRAVIS") != ""){
  travisdir <- file.path(Sys.getenv("TRAVIS_BUILD_DIR"), "travis")
  source(file.path(travisdir, "settings.R"))
  source(file.path(travisdir, "test-import.R"))
  source(file.path(travisdir, "test-numerical.R"))
  source(file.path(travisdir, "test-static.R"))
  source(file.path(travisdir, "test-two-way-parsing.R"))
}
