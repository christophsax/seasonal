wdir_create <- function() {
  # pat <- "x13__"  # this causes X13 to complain about a missing profiling file
  pat <- "x13"
  wd <- tempfile(pattern = pat)
  while (file.exists(wd)) {
    wd <- tempfile(pattern = pat)
  }
  dir.create(wd)
  wd
}

wdir_clean_up <- function(wdir) {
  unlink(wdir, recursive = TRUE)
}

