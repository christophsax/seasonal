#' A cleaner version of seas that only works with list. Goal is to call it form seas()
seas_list <- function(list, na.action = na.omit, out = FALSE, dir = NULL,
                      call = "dummycall", series.name = "x"){

  if (!inherits(list, "list")){
    stop("the 'list' argument mus be of class 'list'")
  }
  if (length(names(list)) != length(list)){
    stop("all spec.argument combinations in 'list' must be named")
  }

  # wdir and file name
  id <- "iofile"
  wdir <- wdir_create()
  iofile <- file.path(wdir, id)  # wdir can be derived from it

  spc <- x13_prepare(list = list, na.action = na.action, iofile = iofile)

  # run X13, either with full output or not
  x13_run(file = iofile, out = out)

  z <- x13_import(iofile, x = list[['x']], na.action = na.action, include_out = out)

  z$call <- call
  z$list <- list
  z$x <- list[['x']]
  z$spc <- spc

  # ### Save output files if 'dir' is specified
  if (!is.null(dir)){
    if (!file.exists(dir)){
      dir.create(dir)
    }
    file.copy(list.files(wdir, full.names = TRUE), dir, overwrite = TRUE)
    message("All X-13ARIMA-SEATS output files have been copied to '", dir, "'.")
  }

  if (!out) {
    wdir_clean_up()
  }

  z
}
