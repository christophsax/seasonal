#' A cleaner version of seas that only works with list. Goal is to call it form seas()
seas_list <- function(list, na.action = na.omit, out = FALSE, dir = NULL,
                      call = "dummycall", series.name = "x"){


  # validate input
  if (!inherits(list, "list")){
    stop("the 'list' argument mus be of class 'list'")
  }
  if (length(names(list)) != length(list)){
    stop("all spec.argument combinations in 'list' must be named")
  }

  # to mimick old behavior FIXME
  # - pass call
  # - pass series name

  # output object
  z <- list()
  z$call <- call
  z$list <- list

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

  # temporary working dir and filenames
  pat <- if (out) "x13out" else "x13"
  wdir <- tempfile(pattern = pat)
  while (file.exists(wdir)) {
    wdir <- tempfile(pattern = pat)
  }

  dir.create(wdir)

  # file names for
  iofile <- file.path(wdir, "iofile")      # inputs and outputs (w/o suffix)
  datafile <- file.path(wdir, "data.dta")  # series to adjust
  # user defined variables
  xreg.file <- file.path(wdir, "xreg.dta")
  xtrans.file <- file.path(wdir, "xtrans.dta")

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


  # # add the default options
  # spc$transform$`function` <- transform.function
  # spc$regression$aictest <- regression.aictest
  # spc$seats$noadmiss <- seats.noadmiss

  # spc <- mod_spclist(spc, list = list(outlier = outlier, automdl = automdl))


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

  ### Run X13, either with full output or not
  run_x13(iofile, out)

  flist <- list.files(wdir) # all files produced by X-13

  ### Save output files if 'dir' is specified
  if (!is.null(dir)){
    if (!file.exists(dir)){
      dir.create(dir)
    }
    file.copy(file.path(wdir, flist), dir, overwrite = TRUE)
    message("All X-13ARIMA-SEATS output files have been copied to '", dir, "'.")
  }

  ### Import from X13

  # check wether there is output at all.
  outfile <- if (getOption("htmlmode") == 1){
    paste(iofile, ".html", sep = "")
  } else {
    paste(iofile, ".out", sep = "")
  }
  if (!file.exists(outfile)){
    stop("no output has been generated")
  }


  # lookup table for output specification
  SPECS <- NULL
  data(specs, envir = environment(), package = "seasonal")  # avoid side effects
  SERIES_SUFFIX <- SPECS$short[SPECS$is.series]

  # add all series that have been produced and are specified in SERIES_SUFFIX
  file.suffix <- unlist(lapply(strsplit(flist, "\\."), function(x) x[[length(x)]]))
  is.series <- file.suffix %in% SERIES_SUFFIX

  series.list <- lapply(file.path(wdir, flist[is.series]), read_series,
                        frequency = frequency(x))
  names(series.list) <- file.suffix[is.series]
  z$series <- series.list

  # data tables (names depend on method, thus a separate call is needed)
  if (!is.null(spc$seats)){
    z$data <- read_data(method = "seats", file = iofile, frequency(x))
  } else if (!is.null(spc$x11)){
    z$data <- read_data(method = "x11", file = iofile, frequency(x))
  } else {
    z$data <- NULL
  }

  # errors/warnings
  z$err <- read_err(iofile)

  if (is.null(z$data)){
    drop_x13messages(z$err)
  } else {
    drop_x13messages(z$err, "Series has been generated, but X-13 returned an error\n\n", msgfun = warnings)
  }

  if (is.null(z$data) && any(c("x11", "seats") %in% names(spc))){
    drop_x13messages(z$err, msg = "X-13 has run but produced no data\n\n", ontype = "all")
  }


  # read .udg file
  z$udg <- read_udg(iofile)

  # read .log file
  if (getOption("htmlmode") != 1){
    z$log <-  readLines(paste0(iofile, ".log"), encoding = "UTF-8")
  }

  # read .est file
  z$est <- read_est(iofile)

  # read .mdl file

  mdl <- readLines(paste0(iofile, ".mdl"))

  # Workaround: in the .mdl output, full regime changes are returned weiredly.
  # E.g.
  # variables=(
  #  td/ for before 1955.Jan/
  # )
  is.r.change <- grepl("//?[ A-Za-z]", mdl)
  rch0 <- mdl[is.r.change]
  rch <- gsub("//[ A-Za-z].+ ", "//", rch0)
  rch <- gsub("/[ A-Za-z].+ ", "/", rch0)
  mdl[is.r.change] <- rch
  z$model <- try(parse_spc(mdl), silent = TRUE)

  is.r.change <- grepl("//?[ A-Za-z]", mdl)

  # fails for very complicated models, but is needed only for static()
  if (inherits(z$model, "try-error")){
    z$model <- NULL
  }

  # read .out file (will be returned only if out = TRUE)
  outtxt <-  readLines(outfile, encoding = "UTF-8")

  # always keep fivebestmdl
  z$fivebestmdl <- detect_fivebestmdl(outtxt)

  ### Checks

  # check if model choosen by seats is identical
  if (any(grepl("Model used for SEATS decomposition is different", z$err))){
    message(paste("Model used in SEATS is different:", z$udg['seatsmdl']))
  }



  # check if freq detection in read_series has worked
  if (!is.null(z$data)){
    ff <- frequency(z$data)
  } else if (length(series.list) > 0){
    ff <- unique(sapply(series.list[sapply(series.list, is.ts)], frequency))
  } else {
    ff <- NULL
  }

  if (!is.null(ff)){
    if (!as.numeric(z$udg['freq']) %in% ff){
      msg <- paste0("Frequency of imported data (", ff, ") is not equal to frequency of detected by X-13 (", as.numeric(z$udg['freq']), "). X-13 retured the addital messages: \n\n")
      drop_x13messages(z$err, msg = msg, ontype = "all")
    }
  }


  ### final additions to output
  if (!is.null(attr(x.na, "na.action"))){
    z$na.action <- attr(x.na, "na.action")
  }

  if (out){
    z$out <-  outtxt
  }

  z$x <- x
  z$spc <- spc
  z$wdir <- wdir

  # clean up
  if (!out){
    unlink(wdir, recursive = TRUE)
  }

  class(z) <- "seas"
  z
}
