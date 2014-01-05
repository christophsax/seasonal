#' Seasonal Adjustment with X-13ARIMA-SEATS
#' 
#' Core function of the seasonal package. By default, \code{seas} calls the 
#' automatic procedures of X-13ARIMA-SEATS to perform a seasonal adjustment that
#' works well in most circumstances.
#' 
#' Seasonal uses the same syntax as X-13ARIMA-SEATS. It is possible to invoke 
#' most options that are available in X-13ARIMA-SEATS. The X-13ARIMA-SEATS 
#' syntax uses specs and arguments, while each spec may contain some arguments. 
#' An additional spec-argument can be added to the \code{seas} function by 
#' separating spec and argument by a dot (\code{.}). For a more extensive 
#' description of the X-13ARIMA-SEATS in \code{seas}, consider the website on 
#' github (\url{https://github.com/christophsax/seasonal})
#' 
#' @param x   object of class \code{"ts"}": time series to seasonaly adjust.
#' @param xreg   (optional) object of class \code{"ts"}": one or several user 
#'   defined exogenous variables for regARIMA modelling.
#' @param seats.noadmiss   spec 'seats' with argument \code{noadmiss = "yes"} 
#'   (default). Seasonal adjustment by SEATS, if SEATS decomposition is invalid,
#'   an alternative model is used with a warning.
#' @param transform.function   spec \code{transform} with argument 
#'   \code{function = "auto"} (default). Automatic log transformation detection.
#'   Set equal to \code{"none"} or \code{"log"} to turn off.
#' @param regression.aictest   spec \code{regression} with argument 
#'   \code{aictest = c("td", "easter")} (default). AIC test for trading days and
#'   Easter effects. Set equal to \code{NULL} to turn off.
#' @param outlier   spec \code{outlier} without arguments (default). Automatic 
#'   oulier detection. Set equal to \code{NULL} to turn off.
#' @param automdl   spec \code{automdl} without arguments (default). Automatic 
#'   model search with the automodl module. Set equal to \code{NULL} to turn 
#'   off.
#' @param na.action  a function which indicates what should happen when the data
#'   contain NAs. \code{na.omit} (default), \code{na.exclude} or \code{na.fail}.
#' @param out   logical, should the X-13ARIMA-SEATS standard output be saved in 
#'   the \code{"seas"} object? (increases object size)
#' @param dir   character string with output path. If specified, the 
#'   X-13ARIMA-SEATS output files are copied to this folder.
#' @param ...  additional spec-arguments options (see details).
#'   
#' @return returns an object of class \code{"seas"}, essentially a list with the
#'   following elements: \item{err}{Warning messages from X-13ARIMA-SEATS.} 
#'   \item{data}{An object of class \code{"ts"}", containing the seasonally 
#'   adjusted data, the raw data, the trend component, the irregular component 
#'   and the seasonal component.} \item{mdl}{A list with the model 
#'   specification, similar to \code{"spc"}". It typically contains 
#'   \code{"regression"}", which contains the regressors and parameter 
#'   estimates, and \code{"arima"}", which contains the ARIMA specification and 
#'   the parameter estimates.} \item{est}{More detailed information on the 
#'   estimation.} \item{lkstats}{Summary statistics.} 
#'   \item{coefficients}{Coefficients of the regARIMA model.} \item{se}{Standard
#'   errors of the regARIMA model.} \item{spc}{An object of class 
#'   \code{"spclist"}, a list containing everything that is send to 
#'   X-13ARIMA-SEATS. Each spec is on the first level, each argument is on the 
#'   second level. Checking \code{"spc"}" is good start for debugging.} 
#'   \item{call}{Function call.}
#'   
#'   The \code{final} function returns the adjusted series, the \code{plot} 
#'   method shows a plot with the unadjusted and the adjusted series. 
#'   \code{summary} gives an overview of the regARIMA model. \code{static} 
#'   returns the static call from above that is needed to replicate an automatic
#'   seasonal adjustment procedure the model.
#'   
#' @references Github page with a more detailed description. 
#'   \url{https://github.com/christophsax/seasonal}
#'   
#'   Wiki page with R examples from the X-13ARIMA-SEATS: 
#'   \url{https://github.com/christophsax/seasonal/wiki/Examples-of-X-13ARIMA-SEATS-in-R}
#'   
#'   
#'   
#'   X-13ARIMA-SEATS manual: \url{http://www.census.gov/ts/x13as/docX13AS.pdf}
#'   
#' @examples
#' \dontrun{
#' x <- seas(AirPassengers) 
#' summary(x)
#' 
#' # invoke X-13ARIMA-SEATS options as 'spec.argument'
#' # (consult the X-13ARIMA-SEATS manual for many more options and the wiki for
#' # for more examples)
#' seas(AirPassengers, regression.aictest = c("td"))  # no easter testing
#' seas(AirPassengers, force.type = "denton")  # force equality of annual values
#' seas(AirPassengers, x11 = list())  # use x11, overrides the 'seats' spec
#' 
#' # turn off automatic procedures:
#' seas(x = AirPassengers, regression.variables = c("td1coef", "easter[1]", 
#' "ao1951.May"), arima.model = "(0 1 1)(0 1 1)", regression.aictest = NULL, 
#' outlier = NULL, transform.function = "log")
#' 
#' # static replication of the first call
#' static(x)  # this also tests the equivalence of the static call
#' static(x, test = FALSE)  # no testing (for debuging)
#' static(x, coef = TRUE)  # also fixes the coefficients
#' 
#' # extractor functions
#' final(x) 
#' original(x) 
#' resid(x) 
#' coef(x)
#' 
#' # five best models
#' fivebestmdl(x)
#' 
#' # replicating the default plots in Win X-13
#' plot(x) 
#' plot(x, trend = TRUE) 
#' monthplot(x) 
#' monthplot(x, choice = "irregular")
#' spectrum(diff(final(x)) )
#' spectrum(diff(original(x)))
#' residplot(x)
#' 
#' # user defined regressors:
#' # a temporary level shift in R base
#' tls <- ts(0, start = 1949, end = 1965, freq = 12)
#' window(tls, start = c(1955, 1), end = c(1957, 12)) <- 1
#' seas(AirPassengers, xreg = tls, outlier = NULL)
#' # identical to a X-13ARIMA-SEATS specification of the the level shift
#' seas(AirPassengers, regression.variables = c("tl1955.01-1957.12"), 
#'      outlier = NULL)
#' 
#' # analyzing X-13ARIMA-SEATS input and output files (for debuging)
#' spc(x)  # the .spc file, as generated by seas
#' mdl(x)  # the .mdl file, as received by seas
#' 
#' # out(x3)  # the full .out output from X-13ARIMA-SEATS (very long!)
#' 
#' # more components in a "seas" object
#' x$est$variance  
#' x$lkstats
#' 
#' # standard NA handling with na.action
#' AirPassengersNA <- window(AirPassengers, start = c(1948, 6), end = c(1961, 4), 
#'                           extend = TRUE)
#' final(seas(AirPassengersNA, na.action = na.omit))     # default
#' final(seas(AirPassengersNA, na.action = na.exclude)) 
#' # final(seas(AirPassengersNA, na.action = na.fail))   # fails
#' 
#' # inspection tool
#' inspect(AirPassengers)
#' }
#' 
#' @export
seas <- function(x, xreg = NULL, seats.noadmiss = "yes", transform.function = "auto", 
                 regression.aictest = c("td", "easter"), outlier = list(), 
                 automdl = list(), 
                 na.action = na.omit,
                 out = FALSE, dir = NULL, ...){
  
  # intial checks
  checkX13(fail = TRUE, confirmation = FALSE)
  stopifnot(inherits(x, "ts"))

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
  regfile <- file.path(wdir, "reg.dta")    # user defined regressors

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
    write_ts_dat(na.action(xreg), file = regfile)

    # user names either from input (single "ts"), or from colnames ("mts)
    if (is.null(dim(xreg))){
      user <- deparse(substitute(xreg))
    } else {
      user <- colnames(xreg)
    }
        
    if (!is.null(spc$regression)){
      spc$regression$user <- user
      spc$regression$file <- paste0("\"", regfile, "\"")
      spc$regression$format <- "\"datevalue\""
    } else if (!is.null(spc$x11regression)){
      spc$x11regression$user <- user
      spc$x11regression$file <- paste0("\"", regfile, "\"")
      spc$x11regression$format <- "\"datevalue\""
    } else {
      stop ("either 'regression' or 'x11regression' has to be specied if 'xreg' is present")
    }
  }
  
  ### write spc
  spctxt <- deparse_spclist(spc)
  writeLines(spctxt, con = paste0(iofile, ".spc"))

  ### Run X13
  run_x13(iofile)
  
  ### Save output files if 'dir' is specified
  if (!is.null(dir)){
    flist <- list.files(wdir, full.names = TRUE)
    if (!file.exists(dir)){
      dir.create(dir)
    }
    file.copy(flist, dir, overwrite = TRUE)
    message("All X-13ARIMA-SEATS output files have been copied to '", dir, "'.")
  }
  
  ### Import from X13
  z <- list()
  class(spc) <- c("spclist", "list")
  
  # read errors/warnings and stop on error
  z$err <-  readLines(paste0(iofile, ".err"))
  if (any(grepl("ERROR:", z$err))){
    stop(paste("(messages generated by X-13ARIMA-SEATS)\n", 
               paste(z$err[-(1:4)], collapse = "\n")))
  }

  # read big output file...
  outfile <-  readLines(paste0(iofile, ".out"), encoding = "UTF-8")
  
  # ...and warn if model choosen by seats is different
  if (any(grepl("Model used for SEATS decomposition is different", z$err))){
    warning(paste("Model used for SEATS decomposition is different:\n", 
                  detect_seatsmdl(outfile)))
  }
  
  # data tables (names depend on method, thus a separate call is needed)
  if (!is.null(spc$seats)){
    z$data <- read_data(method = "seats", file = iofile)
  } else if (!is.null(spc$x11)){
    z$data <- read_data(method = "x11", file = iofile)
  } 
  
  # read additional output files
  z$regressioneffects <- read_series(paste0(iofile, ".ref"))
  z$model <- read_mdl(iofile)
  z$estimates <- read_est(iofile)
  z$lkstats <- read_lks(iofile)
  
  # additional information from outfile 
  # (this is part of the output, outfile is not kept by default)
  z$is.log <- detect_log(outfile)
  z$fivebestmdl <- detect_fivebestmdl(outfile)
  

  ### final additions to output
  
  if (!is.null(attr(x.na, "na.action"))){
    z$na.action <- attr(x.na, "na.action")
  }
  
  if (out){
    outfile[grepl("^\f", outfile)] <- ""  # remove page breaks
    z$out <-  outfile
  }
    
  z$x <- x
  z$spc <- spc
  z$call <- match.call()
  class(z) <- "seas"
  z
}



mod_spclist <- function(x, ...){
  # Add one or several X-13ARIMA-SEATS specs/arguments to a spclist
  #
  # x  "spclist" object
  #
  # returns a "spclist"
  #
  # required by seas
  
  stopifnot(inherits(x, "spclist"))
  
  mod.list <- list(...)
  
  for (i in seq_along(mod.list)){
    content.i <- mod.list[[i]]
    names.i <- names(mod.list)[i]
    
    split.names.i <- strsplit(names.i, "\\.")
    
    stopifnot(length(split.names.i) == 1)
    
    spc.name <- split.names.i[[1]][1]
    
    if (is.null(x[[spc.name]])){
      x[[spc.name]] <- list()
    }
    
    if (length(split.names.i[[1]]) == 1){
      x[[spc.name]] <- content.i
    } else if (length(split.names.i[[1]]) == 2){
      spc.arg <- split.names.i[[1]][2]
      if (is.null(x[[spc.name]][[spc.arg]])){
        x[[spc.name]][[spc.arg]] <- list()
      }
      x[[spc.name]][[spc.arg]] <- content.i
    } else {
      stop("Arguments should contain a Spc and an optional Argument after the dot.")
    }
  }
  x
}

consist_spclist <-function(x){
  # ensure consistency of a spclist
  # 
  # removes exclusive spec/arguments. See 'priority rules' on website..
  # ensures the necessary output
  #
  # x  "spclist" object
  #
  # returns a "spclist"
  #
  # required by seas
  
  stopifnot(inherits(x, "spclist"))
  
  ### avoid mutually exclusive alternatives
  
  # priority: 1. arima, 2. pickmdl, 3. automdl (default)
  if (!is.null(x$automdl) & !is.null(x$arima)){
    x$automdl <- NULL
  }
  if (!is.null(x$pickmdl) & !is.null(x$arima)){
    x$pickmdl <- NULL
  }
  if (!is.null(x$pickmdl) & !is.null(x$automdl)){
    x$automdl <- NULL
  }
  
  # priority: 1. x11, 2. seats (default)
  if (!is.null(x$seats) & !is.null(x$x11)){
    x$seats <- NULL
  }
  
  # priority: 1. x11regression, 2. regression (default)
  if (!is.null(x$x11regression) & !is.null(x$regression)){
    x$regression <- NULL
  }
  
  
  ### ensure correct output
  
  # seats and x11 have different output tables
  if (!is.null(x$seats)){
    x <- mod_spclist(x, seats.save = c("s10", "s11", "s12", "s13", "s16", "s18"))
  } else if (!is.null(x$x11)){
    x <- mod_spclist(x, x11.save = c("d10", "d11", "d12", "d13", "d16", "e18"))
  } 
  
  if (!is.null(x$automdl)){
    x <- mod_spclist(x, automdl.print = "bestfivemdl")
  }
  
  
  # if force is present, return adjusted output
  if (!is.null(x$force$type)){
    x$force$save = "saa"
  }
  
  # always return estimate model
  always.add <- c("model", "estimates", "residuals", "lkstats", "regressioneffects")
  if (is.null(x$estimate$save)){
    x$estimate$save <- always.add
  } else {
    to.add <- always.add[!(always.add %in% x$estimate$save)]
    x$estimate$save <- c(x$estimate$save, to.add)
  }
  
  x
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
    x13.bin <- file.path(env.path, "x13as.exe")
    shell(paste(x13.bin, file), intern = TRUE)
  } else {
    x13.bin <- file.path(env.path, "x13as")
    system(paste(x13.bin, file), intern = TRUE)
  }
}


na.exclude.ts <- function(x){
  # time series method for na.exclude
  # 
  # does not exist in the base package
  # 
  # x  a "ts" object
  # 
  # returns a "ts" object
  #
  # required by seas

  z <- na.omit(x)
  attr(attr(z, "na.action"), "class") <- "exclude"
  z
}


