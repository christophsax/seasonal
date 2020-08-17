# import seas object of a processed iofile
x13_import <- function(iofile, x = NULL, na.action, out = FALSE) {

  frequency_x <- if (is.null(x)) NULL else frequency(x)

  z <- list()

  wdir <- dirname(iofile)

  flist <- list.files(wdir, pattern = paste0("^", basename(iofile))) # all files produced by X-13

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
                        frequency = frequency_x)
  names(series.list) <- file.suffix[is.series]
  z$series <- series.list

  # read .udg file
  z$udg <- read_udg(iofile)

  is_seats <- length(grep("seats", names(z$udg))) > 0
  is_x11 <- length(grep("x11", names(z$udg))) > 0

  # data tables (names depend on method, thus a separate call is needed)
  if (is_seats){
    z$data <- read_data(method = "seats", file = iofile, frequency_x)
  } else if (is_x11){
    z$data <- read_data(method = "x11", file = iofile, frequency_x)
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
  x.na <- na.action(x)
  if (!is.null(attr(x.na, "na.action"))){
    z$na.action <- attr(x.na, "na.action")
  }

  if (out){
    z$out <-  outtxt
  }

  # z$x <- x
  # z$spc <- spc
  z$wdir <- wdir
  z$iofile <- iofile

  # # clean up
  # if (!out){
  #   unlink(wdir, recursive = TRUE)
  # }

  class(z) <- "seas"
  z
}
