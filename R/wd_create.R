wdir_create <- function() {
  pat <- "__x13_"
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
  x13_dirs <- file.path(td, grep("^__x13_", dirs, value = TRUE))
  unlink(x13_dirs, recursive = TRUE)
}

