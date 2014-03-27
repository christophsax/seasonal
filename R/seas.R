#' Seasonal Adjustment with X-13ARIMA-SEATS
#' 
#' Core function of the seasonal package. By default, \code{seas} calls the 
#' automatic procedures of X-13ARIMA-SEATS to perform a seasonal adjustment that
#' works well in most circumstances. Via the \code{...} argument, it is possible
#' to invoke almost all options that are available in X-13ARIMA-SEATS (see 
#' details). The default options are specified as explicit arguments and are 
#' discussed in the arguments section.
#' 
#' It is possible to use the (almost) complete syntax of X-13ARIMA-SEAT via the 
#' \code{...} argument. The syntax of X-13ARIMA-SEATS uses \emph{specs} and 
#' \emph{arguments}, and each spec optionally contains some arguments. In 
#' \code{seas}, an additional spec-argument can be added by separating spec and 
#' argument by a dot (\code{.}) (see examples). Similarily, the 
#' \code{\link{series}} function can be used to read (almost) every output from 
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
#'   an alternative model is used (a warning is given).
#' @param transform.function   spec \code{transform} with argument 
#'   \code{function = "auto"} (default). Automatic log transformation detection.
#'   Set equal to \code{"none"}, \code{"log"} or any value that is allowed by 
#'   X-13 to turn off.
#' @param regression.aictest   spec \code{regression} with argument 
#'   \code{aictest = c("td", "easter")} (default). AIC test for trading days and
#'   Easter effects. Set equal to \code{NULL} to turn off.
#' @param outlier   spec \code{outlier} without arguments (default). Automatic 
#'   oulier detection. Set equal to \code{NULL} to turn off.
#' @param automdl   spec \code{automdl} without arguments (default). Automatic 
#'   model search with the automdl spec. Set equal to \code{NULL} to turn off.
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
#'   
#' @return returns an object of class \code{"seas"}, essentially a list with the
#'   following elements: \item{err}{warning messages from X-13ARIMA-SEATS} 
#'   \item{series}{a list containing the output tables of X-13. To be accessed 
#'   by the \code{series} function.} \item{data}{seasonally adjusted data, the 
#'   raw data, the trend component, the irregular component and the seasonal 
#'   component (deprecated).} \item{model}{list with the model specification, 
#'   similar to \code{"spc"}. It typically contains \code{"regression"}, which 
#'   contains the regressors and parameter estimates, and \code{"arima"}, which 
#'   contains the ARIMA specification and the parameter estimates.} 
#'   \item{estimates}{detailed information on the estimation} 
#'   \item{lkstats}{summary statistics} \item{transform.function}{character 
#'   string, applied transformation} \item{fivebestmdl}{five best models 
#'   according to BIC criterion} \item{qs}{QS statistics} \item{x}{input series}
#'   \item{spc}{object of class \code{"spclist"}, a list containing everything 
#'   that is send to X-13ARIMA-SEATS. Each spec is on the first level, each 
#'   argument is on the second level.} \item{call}{function call}
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
#'   \url{http://cran.r-project.org/web/packages/seasonal/vignettes/seas.pdf}
#'   
#'   Wiki page with a comprehensive list of R examples from the X-13ARIMA-SEATS 
#'   manual: 
#'   \url{https://github.com/christophsax/seasonal/wiki/Examples-of-X-13ARIMA-SEATS-in-R}
#'   
#'   Official X-13ARIMA-SEATS manual: 
#'   \url{http://www.census.gov/ts/x13as/docX13AS.pdf}
#' @export
#' 
#' @examples
#' \dontrun{
#' m <- seas(AirPassengers) 
#' summary(m)
#' 
#' # invoke X-13ARIMA-SEATS options as 'spec.argument' through the ... argument
#' # (consult the X-13ARIMA-SEATS manual for many more options and the wiki for
#' # for more examples)
#' seas(AirPassengers, regression.aictest = c("td"))  # no easter testing
#' seas(AirPassengers, force.type = "denton")  # force equality of annual values
#' seas(AirPassengers, x11 = list())  # use x11, overrides the 'seats' spec
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
                 regression.aictest = c("td", "easter"), outlier = list(), 
                 automdl = list(), na.action = na.omit,
                 out = FALSE, dir = NULL, ...){

  # intial checks
  checkX13(fail = TRUE, full = FALSE)
  if (!inherits(x, "ts")){
    stop("'x' is not a time series.")
  }

  # lookup table for output specification
  SPECS <- NULL 
  data(specs, envir = environment())  # avoid side effects
  SERIES_SUFFIX <- SPECS$short[SPECS$is.series]
  
  # save series name
  series.name <- deparse(substitute(x))
  # remove quotes, they are not allowed in X-13as
  series.name <- gsub('[\'\\"]', '', series.name)
  
  # na action
  x.na <- na.action(x)
  
  # temporary working dir and filenames
  wdir <- file.path(tempdir(), "x13")
  if (!file.exists(wdir)){
    dir.create(wdir)
  }
  file.remove(list.files(wdir, full.names = TRUE))

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
  spc$outlier <- outlier
  spc$automdl <- automdl
  spc$seats$noadmiss <- seats.noadmiss
  
  # add user defined options
  spc <- mod_spclist(spc, ...)

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
      user <- deparse(substitute(xreg))
    } else {
      user <- colnames(xreg)
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
      name <- deparse(substitute(xtrans))
    } else {
      name <- colnames(xtrans)
    }
    spc$transform$name = name
    spc$transform$file <- paste0("\"", xtrans.file, "\"")
    spc$transform$format <- "\"datevalue\""
  }
  
  ### write spc
  spctxt <- deparse_spclist(spc)
  writeLines(spctxt, con = paste0(iofile, ".spc"))

  ### Run X13
  run_x13(iofile)

  flist <- list.files(wdir)

  ### Save output files if 'dir' is specified
  if (!is.null(dir)){
    if (!file.exists(dir)){
      dir.create(dir)
    }
    file.copy(file.path(wdir, flist), dir, overwrite = TRUE)
    message("All X-13ARIMA-SEATS output files have been copied to '", dir, "'.")
  }
  
  ### Import from X13
  z <- list()
  class(spc) <- c("spclist", "list")
  
  # add all series that are specified in SERIES_SUFFIX
  file.suffix <- unlist(lapply(strsplit(flist, "\\."), function(x) x[[2]]))
  is.series <- file.suffix %in% SERIES_SUFFIX
  series.list <- lapply(file.path(wdir, flist[is.series]), read_series)
  names(series.list) <- file.suffix[is.series]
  z$series <- series.list
    
  # data tables (names depend on method, thus a separate call is needed)
  if (!is.null(spc$seats)){
    z$data <- read_data(method = "seats", file = iofile)
  } else if (!is.null(spc$x11)){
    z$data <- read_data(method = "x11", file = iofile)
  } 
  
  # read errors/warnings
  z$err <-  readLines(paste0(iofile, ".err"))
  
  # stop on error
  if (any(grepl("ERROR:", z$err)) | is.null(z$data)){
    cat("Error while reading the following .spc file:\n\n")
    print(spc)
    cat("\n\n\n")
    message(paste("Error messages generated by X-13ARIMA-SEATS:\n", 
              paste(z$err[-(1:4)], collapse = "\n")))
    if (is.null(z$data)){
      stop("no series has been generated")
    } else {
      warning("series has been generated")
    }
  }
  
  # read big output file...
  outtxt <-  readLines(paste0(iofile, ".out"), encoding = "UTF-8")
  
  # ...and warn if model choosen by seats is different
  if (any(grepl("Model used for SEATS decomposition is different", z$err))){
    warning(paste("Model used for SEATS decomposition is different:\n", 
                  detect_seatsmdl(outtxt)))
  }
  
  # read the log file
  z$log <-  readLines(paste0(iofile, ".log"), encoding = "UTF-8")
  class(z$log) <- "out"
  
  # check whether freq detection in read_series has worked.
  stopifnot(frequency(z$data) == frequency(x))

  # read additional output files
  z$model <- read_mdl(iofile)
  z$estimates <- read_est(iofile)
  z$lkstats <- read_lks(iofile)
  
  # additional information from outtxt 
  # (this is part of the output, outtxt is not kept by default)
  if (transform.function == "auto"){
    z$transform.function <- detect_auto(outtxt)
  } else {
    z$transform.function <- transform.function
  }
  z$fivebestmdl <- detect_fivebestmdl(outtxt)
  z$qs <- detect_qs(outtxt)
  

  ### final additions to output
  
  if (!is.null(attr(x.na, "na.action"))){
    z$na.action <- attr(x.na, "na.action")
  }
  
  if (out){
    outtxt[grepl("^\f", outtxt)] <- ""  # remove page breaks
    z$out <-  outtxt
    class(z$out) <- "out"
  }
    
  z$x <- x  # input series
  z$spc <- spc
  z$call <- match.call()
  class(z) <- "seas"
  z
}

run_x13 <- function(file){
  # run X-13ARIMA-SEATS platform dependently
  # 
  # file  character, full filename w/o suffix
  #
  # run X-13 as a side effect
  #
  # required by seas

  env.path <- Sys.getenv("X13_PATH")
  if (.Platform$OS.type == "windows"){
    x13.bin <- paste0("\"", file.path(env.path, "x13as.exe"), "\"")
    shell(paste(x13.bin, file), intern = TRUE)
  } else {
    x13.bin <- file.path(env.path, "x13as")
    system(paste(x13.bin, file), intern = TRUE)
  }
}


