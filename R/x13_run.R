x13_run <- function(file, out, meta = FALSE){
  # run X-13ARIMA-SEATS platform dependently
  #
  # file  character, full filename w/o suffix
  #
  # run X-13 as a side effect
  #
  # required by seas

  x13_path <- get_x13_path()

  # -n output suppression (no out file)
  # -s store additional output (.udg file)
  # -m use metafile
  flags <- if (out) "-s" else "-n -s"
  m_flag <- if (meta) "-m" else ""
  if (.Platform$OS.type == "windows"){
    if (getOption("htmlmode") == 1){
      x13.bin <- paste0("\"", normalizePath(file.path(x13_path, "x13ashtml.exe")), "\"")
    } else {
      x13.bin <- paste0("\"", normalizePath(file.path(x13_path, "x13as.exe")), "\"")
    }
    # change wd on win as X-13 writes `fort.6` to it
    owd <- getwd()
    on.exit(setwd(owd))
    setwd(dirname(file))

    msg <- shell(paste(x13.bin, m_flag, file, flags), intern = TRUE)
  } else {
    if (getOption("htmlmode") == 1){
      # ignore case on unix to avoid problems with different binary names
      fl <- list.files(x13_path)
      x13.bin <- file.path(x13_path, fl[grepl("^x13ashtml$", fl, ignore.case = TRUE)])
    } else {
      x13.bin <- file.path(x13_path, "x13as")
    }
    # change wd in meta mode, as it wirtes `fort.14` to it
    if (meta) {
      owd <- getwd()
      on.exit(setwd(owd))
      setwd(dirname(file))
    }

    msg <- system(paste(x13.bin, m_flag, file, flags), intern = TRUE, ignore.stderr = TRUE)
  }
  # error message if output contains the word ERROR
  if (inherits(msg, "character")){
    if (any(grepl("ERROR", msg))){
      if (file.exists(paste0(file, ".err"))){
        if (any(grepl("iofile_err", msg))){
          # read from separate file
          err <- read_err(file)
          drop_x13messages(err)
        } else {
          # fall back: parse message
          err <- detect_error(msg, htmlmode = 0)
          drop_x13messages(err)
        }
      } else {
        # another fall back: parse message
        err <- detect_error(msg, htmlmode = 0)
        drop_x13messages(err)
      }
    }
  }

  # error message on non-zero failing
  if (!is.null(attr(msg, "status"))){
    if (attr(msg, "status") > 0){
      msg <- system(paste(x13.bin, file, flags), intern = TRUE, ignore.stderr = FALSE)
      stop("X-13 has returned a non-zero exist status, which means that the current spec file cannot be processed for an unknown reason.", call. = FALSE)
    }
  }

  invisible(TRUE)

}


