# CRAN SUBMISSION
# - remove test folder
# - remove img links in README.md

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

