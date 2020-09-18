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

wdir_clean_up <- function() {
  td <- tempdir()
  dirs <- list.dirs(path = td, full.names = FALSE, recursive = FALSE)
  x13_dirs <- file.path(td, grep("^x13", dirs, value = TRUE))
  unlink(x13_dirs, recursive = TRUE)
}

