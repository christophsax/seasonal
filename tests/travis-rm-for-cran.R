# for CRAN SUBMISSION:
# - remove travis-rm-for-cran.R
# - remove img links in README.md
# - is NEWS up to date?
# - r-devel test


# These tests are more extensive and only need to run on travis, not on CRAN.


seasonal::checkX13()
x13binary::checkX13binary()


if (Sys.getenv("TRAVIS") != ""){
  travisdir <- file.path(Sys.getenv("TRAVIS_BUILD_DIR"), "travis")
} else if (Sys.info()['sysname'] == "Darwin"){
  travisdir <- "~/git/seasonal/travis"  # local mac
} else {
  travisdir <- "~/seasonal/travis"      # ubuntu server
}


source(file.path(travisdir, "settings.R"))

source(file.path(travisdir, "test-import.R"))
source(file.path(travisdir, "test-numerical.R"))
source(file.path(travisdir, "test-static.R"))
source(file.path(travisdir, "test-two-way-parsing.R"))

