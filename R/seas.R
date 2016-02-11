#' Seasonal Adjustment with X-13ARIMA-SEATS
#' 
#' Core function of the seasonal package. With the default options, \code{seas} calls the 
#' automatic procedures of X-13ARIMA-SEATS to perform a seasonal adjustment that
#' works well in most circumstances. Via the \code{...} argument, it is possible
#' to invoke almost all options that are available in X-13ARIMA-SEATS (see 
#' details). The default options of \code{seas} are listed as explicit arguments and are 
#' discussed in the arguments section.
#' 
#' It is possible to use the almost complete syntax of X-13ARIMA-SEAT via the 
#' \code{...} argument. The syntax of X-13ARIMA-SEATS uses \emph{specs} and 
#' \emph{arguments}, and each spec optionally contains some arguments. In 
#' \code{seas}, an additional spec-argument can be added by separating spec and 
#' argument by a dot (\code{.}) (see examples). Alternatvily, spec-argument 
#' combinations can be supplied as a named list, which is useful for programming.
#' 
#' Similarily, the 
#' \code{\link{series}} function can be used to read almost all series from 
#' X-13ARIMA-SEATS.
#' 
#' For a more extensive description, consider the vignette or the wiki page, 
#' which contains replications of almost all examples from the official 
#' X-13ARIMA-SEATS manual.
#' 
#' @param x   object of class \code{"ts"}: time series to seasonaly adjust.
#' @param xreg   (optional) object of class \code{"ts"}: one or several user 
#'   defined exogenous variables for regARIMA modelling, can be used both with 
#'   \code{regression} or \code{x11regression}.
#' @param xtrans   (optional) object of class \code{"ts"}: one or two user 
#'   defined exogenous variables for the \code{transform} spec. Can be specifed 
#'   together with \code{xreg}.
#' @param seats.noadmiss   spec 'seats' with argument \code{noadmiss = "yes"} 
#'   (default). Seasonal adjustment by SEATS, if SEATS decomposition is invalid,
#'   an alternative model is used (a message is returned). If \code{noadmiss =
#'   "no"}, no approximation is done. If the seats spec is removed 
#'   (\code{seats = NULL}), no seasonal adjustment is performed.
#' @param transform.function   spec \code{transform} with argument 
#'   \code{function = "auto"} (default). Automatic log transformation detection.
#'   Set equal to \code{"none"}, \code{"log"} or any value that is allowed by 
#'   X-13 to turn it off.
#' @param regression.aictest   spec \code{regression} with argument 
#'   \code{aictest = c("td", "easter")} (default). AIC test for trading days and
#'   Easter effects. Set equal to \code{NULL} to turn it off.
#' @param outlier   spec \code{outlier} without arguments (default). Automatic 
#'   oulier detection. Set equal to \code{NULL} to turn it off.
#' @param automdl   spec \code{automdl} without arguments (default). Automatic 
#'   model search with the automdl spec. Set equal to \code{NULL} to turn it off.
#' @param na.action  a function which indicates what should happen when the data
#'   contain NAs. \code{na.omit} (default), \code{na.exclude} or \code{na.fail}.
#'   If \code{na.action = na.x13}, NA handling is done by X-13, i.e. NA values 
#'   are substituted by -99999.
#' @param out   logical, should the X-13ARIMA-SEATS standard output be saved in 
#'   the \code{"seas"} object? (this increases object size substantially, it is 
#'   recommended to re-evaluate the model using the \code{\link{out}} function 
#'   instead.)
#' @param dir   character string with a user defined file path. If specified, 
#'   the X-13ARIMA-SEATS output files are copied to this folder. Useful for 
#'   debugging.
#' @param ...  additional spec-arguments options sent to X-13ARIMA-SEATS (see 
#'   details).
#' @param list  a named list with additional spec-arguments options. This is an
#'   alternative to the \code{...} argument. It is useful for programming.
#'
#' @return returns an object of class \code{"seas"}, essentially a list with the
#'   following components: 
#'   \item{series}{a list containing the output tables of X-13. To be accessed 
#'   by the \code{series} function.} 
#'   \item{data}{seasonally adjusted data, the 
#'   raw data, the trend component, the irregular component and the seasonal 
#'   component (deprecated).} 
#'   \item{err}{warning messages from X-13ARIMA-SEATS} 
#'   \item{udg}{content of the \code{.udg} output file} 
#'   \item{est}{content of the \code{.est} output file} 
#'   \item{lks}{content of the \code{.lks} output file} 
#'   \item{model}{list with the model specification, 
#'   similar to \code{"spc"}. It typically contains \code{"regression"}, which 
#'   contains the regressors and parameter estimates, and \code{"arima"}, which 
#'   contains the ARIMA specification and the parameter estimates.} 
#'   \item{fivebestmdl}{Best Five ARIMA Models (unparsed)} 
#'   \item{x}{input series}
#'   \item{spc}{object of class \code{"spclist"}, a list containing the content of the \code{.spc} file that is
#'   used by X-13ARIMA-SEATS. Each spec is on the first level, each 
#'   argument is on the second level.} 
#'   \item{call}{function call}
#'   \item{wdir}{temporary directory in which X-13ARIMA-SEATS has been run}
#'   
#'   The \code{final} function returns the final adjusted series, the 
#'   \code{plot} method shows a plot with the unadjusted and the adjusted 
#'   series. \code{summary} gives an overview of the regARIMA model. 
#'   
#' @seealso \code{\link{static}}, to return the static call that is needed to 
#'   replicate an automatic model
#' @seealso \code{\link{series}}, for universal X-13 table series import.
#' @seealso \code{\link{out}}, for the import of X-13 text files
#' @seealso \code{\link{inspect}}, to interactively inspect a seasonal 
#'   adjustment model.
#' @seealso \code{\link{plot.seas}}, for diagnostical plots.
#' @seealso \code{\link{qs}}, for diagnostical statistics.
#'   
#' @references Vignette with a more detailed description: 
#'   \url{http://www.seasonal.website/seasonal.html}
#'   
#'   Comprehensive list of R examples from the X-13ARIMA-SEATS manual: 
#'   \url{http://www.seasonal.website/examples.html}
#'   
#'   Official X-13ARIMA-SEATS manual: 
#'   \url{http://www.census.gov/ts/x13as/docX13AS.pdf}
#' @export
#' @import datasets
#' @import grDevices
#' @import graphics
#' @import utils
#' 
#' @examples
#' \dontrun{
#' m <- seas(AirPassengers) 
#' summary(m)
#' 
#' # invoke X-13ARIMA-SEATS options as 'spec.argument' through the ... argument
#' # (consult the X-13ARIMA-SEATS manual for many more options and the list of
#' # R examples for more examples)
#' seas(AirPassengers, regression.aictest = c("td"))  # no easter testing
#' seas(AirPassengers, force.type = "denton")  # force equality of annual values
#' seas(AirPassengers, x11 = "")  # use x11, overrides the 'seats' spec
#'
#' # 'spec.argument' combinations can also be supplied as a named list, which is
#' # useful for programming
#' seas(AirPassengers, list = list(regression.aictest = c("td"), outlier = NULL))
#' # constructing the list step by step
#' ll <- list()
#' ll[["x"]] <- AirPassengers
#' ll[["regression.aictest"]] <- "td"
#' ll["outlier"] <- list(NULL)  # assigning NULL to a list using single brackets
#' seas(list = ll)
#'
#' # options can be entered as vectors
#' seas(AirPassengers, regression.variables = c("td1coef", "easter[1]"))
#' seas(AirPassengers, arima.model = c(0, 1, 1, 0, 1, 1))
#' seas(AirPassengers, arima.model = "(0 1 1)(0 1 1)")     # equivalent
#' 
#' # turn off the automatic procedures
#' seas(AirPassengers, regression.variables = c("td1coef", "easter[1]", 
#' "ao1951.May"), arima.model = "(0 1 1)(0 1 1)", regression.aictest = NULL, 
#' outlier = NULL, transform.function = "log")
#' 
#' # static replication of 'm <- seas(AirPassengers)'
#' static(m)  # this also tests the equivalence of the static call
#' static(m, test = FALSE)  # no testing (much faster)
#' static(m, coef = TRUE)  # also fixes the coefficients
#' 
#' # specific extractor functions
#' final(m) 
#' original(m) 
#' resid(m) 
#' coef(m)
#' fivebestmdl(m)
#' out(m)                  # the X-13 .out file (see ?out, for details)
#' spc(m)                  # the .spc input file to X-13 (for debugging)
#' 
#' # universal extractor function for any X-13ARIMA-SEATS output (see ?series)
#' series(m, "forecast.forecasts")
#' 
#' # copying the output of X-13 to a user defined directory
#' seas(AirPassengers, dir = "~/mydir")
#' 
#' # user defined regressors (see ?genhol for more examples)
#' # a temporary level shift in R base
#' tls <- ts(0, start = 1949, end = 1965, freq = 12)
#' window(tls, start = c(1955, 1), end = c(1957, 12)) <- 1
#' seas(AirPassengers, xreg = tls, outlier = NULL)
#' # identical to a X-13ARIMA-SEATS specification of the the level shift
#' seas(AirPassengers, regression.variables = c("tl1955.01-1957.12"), 
#'      outlier = NULL)
#' 
#' # forecasting an annual series without seasonal adjustment
#' m <- seas(airmiles, seats = NULL, regression.aictest = NULL)
#' series(m, "forecast.forecasts")
#' 
#' # NA handling
#' AirPassengersNA <- window(AirPassengers, end = 1962, extend = TRUE)
#' final(seas(AirPassengersNA, na.action = na.omit))    # no NA in final series
#' final(seas(AirPassengersNA, na.action = na.exclude)) # NA in final series
#' # final(seas(AirPassengersNA, na.action = na.fail))    # fails
#' 
#' # NA handling by X-13 (works with internal NAs)
#' AirPassengersNA[20] <- NA 
#' final(seas(AirPassengersNA, na.action = na.x13))
#' 
#' # inspect tool
#' inspect(m)
#' }
#' 
seas <- function(x, xreg = NULL, xtrans = NULL, 
         seats.noadmiss = "yes", transform.function = "auto", 
         regression.aictest = c("td", "easter"), outlier = "", 
         automdl = "", na.action = na.omit,
         out = FALSE, dir = NULL, ..., list = NULL){
  
  # setX13Path()
  
  # intial checks
  checkX13(fail = TRUE, fullcheck = FALSE, htmlcheck = FALSE)
  
  # lookup table for output specification
  SPECS <- NULL 
  data(specs, envir = environment(), package = "seasonal")  # avoid side effects
  SERIES_SUFFIX <- SPECS$short[SPECS$is.series]
  
  # save series name
  series.name <- deparse(substitute(x))

  # remove quotes in series.name, they are not allowed in X-13as
  series.name <- gsub('[\'\\"]', '', series.name)

  # using the list argument instead of '...''
  if (is.null(list)){
    list <- list(...)
  } else {
    if (!inherits(list, "list")){
      stop("the 'list' argument mus be of class 'list'")
    }
    if (length(names(list)) != length(list)){
      stop("all spec.argument combinations in 'list' must be named")
    }
    # overwrite defaults if specified in the list
    dl <- names(list)[names(list) %in% names(formals(seas))]
    for (dli in dl){
      assign(dli, list[[dli]])
    }
    if ("list" %in% dl){
      stop("no 'list' argument inside the 'list' argument allowed")
    }
    if (length(list(...) > 0)){
      warning("if 'list' is specified, spec.argument combinations delivered to '...' are ignored.")
    }
    if ("x" %in% dl){
      series.name <- "ser"
    }
    # remove defaults from list
    list <- list[!(names(list) %in% dl)]   
  }
  

  # check series
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
  
  # add the default options
  spc$transform$`function` <- transform.function
  spc$regression$aictest <- regression.aictest
  spc$seats$noadmiss <- seats.noadmiss
  
  spc <- mod_spclist(spc, list = list(outlier = outlier, automdl = automdl))
  
  # add user defined options
  spc <- mod_spclist(spc, list = list)
  
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
    if (!is.null(spc$regression)){
      spc$regression$user <- user
      spc$regression$file <- paste0("\"", xreg.file, "\"")
      spc$regression$format <- "\"datevalue\""
    } else if (!is.null(spc$x11regression)){
      spc$x11regression$user <- user
      spc$x11regression$file <- paste0("\"", xreg.file, "\"")
      spc$x11regression$format <- "\"datevalue\""
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
  z <- list()  # output object
  
  # check wether there is output at all.
  outfile <- if (getOption("htmlmode") == 1){
    paste(iofile, ".html", sep = "")
  } else {
    paste(iofile, ".out", sep = "")
  }
  if (!file.exists(outfile)){
    stop("no output has been generated")
  }

  # add all series that have been produced and are specified in SERIES_SUFFIX
  file.suffix <- unlist(lapply(strsplit(flist, "\\."), function(x) x[[2]]))
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
  
  # browser()
  # read .est file
  z$est <- read_est(iofile)

  # read .lks file
  z$lks <- read_lks(iofile)

  # read .mdl file
  z$model <- try(parse_spc(readLines(paste0(iofile, ".mdl"))), silent = TRUE) 

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
  z$call <- match.call()
  z$wdir <- wdir

  # clean up
  if (!out){
    unlink(wdir, recursive = TRUE)
  }

  class(z) <- "seas"
  z
}

run_x13 <- function(file, out){
  # run X-13ARIMA-SEATS platform dependently
  # 
  # file  character, full filename w/o suffix
  #
  # run X-13 as a side effect
  #
  # required by seas
  
  env.path <- Sys.getenv("X13_PATH")
    
  # -n no tables
  # -s store additional output (.udg file)
  flags <- if (out) {"-s"} else {"-n -s"}
  if (.Platform$OS.type == "windows"){
    if (getOption("htmlmode") == 1){
      x13.bin <- paste0("\"", file.path(env.path, "x13ashtml.exe"), "\"")
    } else {
      x13.bin <- paste0("\"", file.path(env.path, "x13as.exe"), "\"")
    }
    # change wd on win as X-13 writes `fort.6` to it
    owd <- getwd()
    on.exit(setwd(owd))
    setwd(dirname(file))

    msg <- shell(paste(x13.bin, file, flags), intern = TRUE)
  } else {
    if (getOption("htmlmode") == 1){
      # ignore case on unix to avoid problems with different binary names
      fl <- list.files(env.path)
      x13.bin <- file.path(env.path, fl[grepl("^x13ashtml$", fl, ignore.case = TRUE)])
    } else {
      x13.bin <- file.path(env.path, "x13as")
    }
    msg <- system(paste(x13.bin, file, flags), intern = TRUE, ignore.stderr = TRUE)

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
      }
    }
  }

  # error message on non-zero failing
  if (!is.null(attr(msg, "status"))){
    if (attr(msg, "status") > 0){
      stop("X-13 has returned a non-zero exist status, which means that the current spec file cannot be processed for an unknown reason.", call. = FALSE)
    }
  }

}


