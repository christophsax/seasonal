# for CRAN SUBMISSION:
# - is NEWS and DESCRIPTION up to date?

# - knit vignette
# - rhub test

# travisdir <- "~/git/seasonal/travis"
# These tests are more extensive and only need to run on travis, not on CRAN.


if (!x13binary::supportedPlatform()) skip()
if (!x13binary::supportedPlatform()) skip()



seasonal::checkX13()
x13binary::checkX13binary()


nocran_tests <- file.path(".", "nocran")
source(file.path(nocran_tests, "settings.R"))
source(file.path(nocran_tests, "test-import.R"))
source(file.path(nocran_tests, "test-numerical.R"))
source(file.path(nocran_tests, "test-static.R"))
source(file.path(nocran_tests, "test-update.R"))
source(file.path(nocran_tests, "test-two-way-parsing.R"))
source(file.path(nocran_tests, "test-multi.R"))
source(file.path(nocran_tests, "test-issues.R"))
