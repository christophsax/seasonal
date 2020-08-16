x13_prepare <- function(list, na.action = na.omit, iofile) {

  x <- list[['x']]
  xreg <- list[['xreg']]
  xtrans <- list[['xtrans']]

  # # save list with evaluated arguments  WHY???
  # list <- lapply(list, eval, envir = parent.frame())

  if (!inherits(x, "ts")){
    stop("'x' argument is not a time series.")
  }
  if (start(x)[1] <= 1000){
    stop("start year of 'x' must be > 999.")
  }

  # na action
  x.na <- na.action(x)

  # derived file names
  datafile <- paste0(iofile, ".dta")
  xreg.file <- paste0(iofile, "_xreg.dta")
  xtrans.file <- paste0(iofile, "_xtrans.dta")
  series.name <- basename(iofile)

  ### write data
  write_ts_dat(x.na, file = datafile)

  ### construct spclist (spclist fully describes the .spc file)
  spc <- list()
  class(spc) <- c("spclist", "list")

  # add data series
  spc$series$title <- paste0("\"", series.name, "\"")
  spc$series$file <- paste0("\"", datafile, "\"")
  spc$series$format <- "\"datevalue\""
  spc$series$period <- frequency(x)

  # add user defined options
  non_x13_args <- c("x", "xtrans", "xreg")
  spc <- mod_spclist(spc, list = list[!(names(list) %in% non_x13_args)])

  # remove double entries, adjust outputs
  spc <- consist_spclist(spc)

  ### user defined regressors
  if (!is.null(xreg)){
    if (frequency(xreg) != frequency(x)){
      stop('xreg and x must be of the same frequency.')
    }
    write_ts_dat(na.action(xreg), file = xreg.file)
    # user names either from input (single "ts"), or from colnames ("mts)
    if (is.null(dim(xreg))){
      if (inherits(substitute(xreg), "name")){
        user <- deparse(substitute(xreg))
      } else {
        user <- "xreg"
      }
    } else {
      user <- paste0("xreg", 1:NCOL(xreg))
      # user <- gsub("[\\(\\)]", "", colnames(xreg))
    }

    if (!is.null(spc$x11regression)){
      spc$x11regression$user <- user
      spc$x11regression$file <- paste0("\"", xreg.file, "\"")
      spc$x11regression$format <- "\"datevalue\""
    } else {
      spc$regression$user <- user
      spc$regression$file <- paste0("\"", xreg.file, "\"")
      spc$regression$format <- "\"datevalue\""
    }
  }

  if (!is.null(xtrans)){
    if (frequency(xtrans) != frequency(x)){
      stop('xtrans and x must be of the same frequency.')
    }
    write_ts_dat(na.action(xtrans), file = xtrans.file)
    # user names either from input (single "ts"), or from colnames ("mts)
    if (is.null(dim(xtrans))){
      if (inherits(substitute(xtrans), "name")){
        name <- deparse(substitute(xtrans))
      } else {
        name <- "xtrans"
      }
    } else {
      name <- paste0("xtrans", 1:NCOL(xtrans))
      # name <- gsub("[\\(\\)]", "", colnames(xtrans))
    }
    spc$transform$name = name
    spc$transform$file <- paste0("\"", xtrans.file, "\"")
    spc$transform$format <- "\"datevalue\""
  }

  ### write spc
  spctxt <- deparse_spclist(spc)
  writeLines(spctxt, con = paste0(iofile, ".spc"))
  invisible(spc)
}
