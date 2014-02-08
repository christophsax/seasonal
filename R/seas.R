#' Seasonal Adjustment with X-13ARIMA-SEATS
#' 
#' Core function of the seasonal package. By default, \code{seas} calls the 
#' automatic procedures of X-13ARIMA-SEATS to perform a seasonal adjustment that
#' works well in most circumstances. Through the \code{...} argument, it is 
#' possible to invoke almost all options that are available in X-13ARIMA-SEATS 
#' (see details). The default options are specified as explicit arguments and 
#' are discussed in the arguments section.
#' 
#' \emph{seasonal} uses a very similar syntax than the official X-13ARIMA-SEATS.
#' The X-13ARIMA-SEATS syntax uses \emph{specs} and \emph{arguments}, and each
#' spec optionally contains some arguments. In \code{seas}, an additional 
#' spec-argument can be added by separating spec and argument by a dot 
#' (\code{.}) (see examples).
#' 
#' For a more extensive description, consider the vignette or the wiki page,
#' which contains replications of almost all examples from the official
#' X-13ARIMA-SEATS manual.
#' 
#' @param x   object of class \code{"ts"}": time series to seasonaly adjust.
#' @param xreg   (optional) object of class \code{"ts"}": one or several user 
#'   defined exogenous variables for regARIMA modelling, can be used both with 
#'   \code{regression} or \code{x11regression}.
#' @param xtrans   (optional) object of class \code{"ts"}": one or two user 
#'   defined exogenous variables for the \code{transform} spec. Can be specifed 
#'   together with \code{xreg}.
#' @param seats.noadmiss   spec 'seats' with argument \code{noadmiss = "yes"} 
#'   (default). Seasonal adjustment by SEATS, if SEATS decomposition is invalid,
#'   an alternative model is used (a warning is given).
#' @param transform.function   spec \code{transform} with argument 
#'   \code{function = "auto"} (default). Automatic log transformation detection.
#'   Set equal to \code{"none"} or \code{"log"} to turn off.
#' @param regression.aictest   spec \code{regression} with argument 
#'   \code{aictest = c("td", "easter")} (default). AIC test for trading days and
#'   Easter effects. Set equal to \code{NULL} to turn off.
#' @param outlier   spec \code{outlier} without arguments (default). Automatic 
#'   oulier detection. Set equal to \code{NULL} to turn off.
#' @param automdl   spec \code{automdl} without arguments (default). Automatic 
#'   model search with the automdl spec. Set equal to \code{NULL} to turn 
#'   off.
#' @param na.action  a function which indicates what should happen when the data
#'   contain NAs. \code{na.omit} (default), \code{na.exclude} or \code{na.fail}.
#' @param out   logical, should the X-13ARIMA-SEATS standard output be saved in 
#'   the \code{"seas"} object? (this increases object size substantially, it is 
#'   recommended to re-evaluate the model using the \code{\link{out}} 
#'   function instead.)
#' @param dir   character string with a user defined file path. If specified, 
#'   the X-13ARIMA-SEATS output files are copied to this folder. Useful for 
#'   debugging.
#' @param ...  additional spec-arguments options sent to X-13ARIMA-SEATS (see 
#'   details).
#'   
#' @return returns an object of class \code{"seas"}, essentially a list with the
#'   following elements: 
#'   \item{err}{warning messages from X-13ARIMA-SEATS} 
#'   \item{data}{seasonally 
#'   adjusted data, the raw data, the trend component, the irregular component 
#'   and the seasonal component} 
#'   \item{regressioneffects}{regression variables multiplied by the vector of estimated regression coefficients} 
#'   \item{model}{list with the model 
#'   specification, similar to \code{"spc"}. It typically contains 
#'   \code{"regression"}, which contains the regressors and parameter 
#'   estimates, and \code{"arima"}, which contains the ARIMA specification and 
#'   the parameter estimates.} 
#'   \item{estimates}{detailed information on the 
#'   estimation} 
#'   \item{lkstats}{summary statistics} 
#'   \item{sfspans}{(optional) data from sliding spans analysis} 
#'   \item{transform.function}{character string, applied transformation}
#'   \item{fivebestmdl}{five best models according to BIC criterion}
#'   \item{qs}{QS statistics}
#'   \item{x}{input series}
#'   \item{spc}{object of class 
#'   \code{"spclist"}, a list containing everything that is send to 
#'   X-13ARIMA-SEATS. Each spec is on the first level, each argument is on the 
#'   second level.} 
#'   \item{call}{function call}
#'   
#'   The \code{final} function returns the final adjusted series, the
#'   \code{plot} method shows a plot with the unadjusted and the adjusted
#'   series. \code{summary} gives an overview of the regARIMA model.
#'   
#' @seealso \code{\link{static}}, to return the static call that is needed to 
#'   replicate an automatic model
#' @seealso \code{\link{inspect}}, to interactively inspect a seasonal 
#'   adjustment model.
#' @seealso \code{\link{plot.seas}}, for diagnostical plots.
#' @seealso \code{\link{qs}}, for diagnostical statistics.
#' @seealso \code{\link{out}}, for diagnostical re-evaluation
#'   
#' @references Vignette with a more detailed description: 
#'   \url{http://cran.r-project.org/web/packages/seasonal/vignettes/seas.pdf}
#'   
#'   Wiki page with a comprehensive list of R examples from the X-13ARIMA-SEATS 
#'   manual: 
#'   \url{https://github.com/christophsax/seasonal/wiki/Examples-of-X-13ARIMA-SEATS-in-R}
#'   
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
#' # invoke X-13ARIMA-SEATS options as 'spec.argument'
#' # (consult the X-13ARIMA-SEATS manual for many more options and the wiki for
#' # for more examples)
#' seas(AirPassengers, regression.aictest = c("td"))  # no easter testing
#' seas(AirPassengers, force.type = "denton")  # force equality of annual values
#' seas(AirPassengers, x11 = list())  # use x11, overrides the 'seats' spec
#' 
#' # turn off the automatic procedures
#' seas(AirPassengers, regression.variables = c("td1coef", "easter[1]", 
#' "ao1951.May"), arima.model = "(0 1 1)(0 1 1)", regression.aictest = NULL, 
#' outlier = NULL, transform.function = "log")
#' 
#' # static replication of the first call
#' static(m)  # this also tests the equivalence of the static call
#' static(m, test = FALSE)  # no testing (useful for debugging)
#' static(m, coef = TRUE)  # also fixes the coefficients
#' 
#' # extractor functions
#' final(m) 
#' original(m) 
#' resid(m) 
#' coef(m)
#' 
#' # five best models
#' fivebestmdl(m)
#' 
#' # replicating the default plots in Win X-13
#' plot(m) 
#' plot(m, trend = TRUE) 
#' monthplot(m) 
#' monthplot(m, choice = "irregular")
#' spectrum(diff(final(m)) )
#' spectrum(diff(original(m)))
#' residplot(m)
#' 
#' # user defined regressors
#' # a temporary level shift in R base
#' tls <- ts(0, start = 1949, end = 1965, freq = 12)
#' window(tls, start = c(1955, 1), end = c(1957, 12)) <- 1
#' seas(AirPassengers, xreg = tls, outlier = NULL)
#' # identical to a X-13ARIMA-SEATS specification of the the level shift
#' seas(AirPassengers, regression.variables = c("tl1955.01-1957.12"), 
#'      outlier = NULL)
#' 
#' # input and output from and to X-13ARIMA-SEATS
#' spc(m)  # analyzing the .spc file (for debugging)
#' out(m)  # display the .out file (see ?out, for details)
#' 
#' # NA handling
#' AirPassengersNA <- window(AirPassengers, end = 1962, extend = TRUE)
#' final(seas(AirPassengersNA, na.action = na.omit))    # no NA in final series
#' final(seas(AirPassengersNA, na.action = na.exclude)) # NA in final series
#' # final(seas(AirPassengersNA, na.action = na.fail))    # fails
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
  
  # check whether freq detection in read_series has worked.
  stopifnot(frequency(z$data) == frequency(x))
  
  # read additional output files
  z$regressioneffects <- read_series(paste0(iofile, ".ref"))
  z$model <- read_mdl(iofile)
  z$estimates <- read_est(iofile)
  z$lkstats <- read_lks(iofile)
  
  z$sfspans <- read_series(paste0(iofile, ".sfs"))
  z$sfspans[z$sfspans==-999] <- NA
  
  z$sarevisions <- read_series(paste0(iofile, ".sar"))
  z$sae <- read_series(paste0(iofile, ".sae"))
  
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
      stop("X-13ARIMA-SEATA options should contain a spec and an optional argument after the dot.")
    }
  }
  x
}

consist_spclist <-function(x){
  # ensure consistency of a spclist
  # 
  # removes exclusive spec.arguments. See 'priority rules' on website..
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
  
#   # priority: 1. x11regression, 2. regression (default)
#   if (!is.null(x$x11regression) & !is.null(x$regression)){
#     x$regression <- NULL
#   }
#   
  
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
  
  if (!is.null(x$history)){
    x <- mod_spclist(x, history.save = c("sarevisions", "saestimates"))
  }
  
  if (!is.null(x$slidingspans)){
    x <- mod_spclist(x, slidingspans.save = c("sfspans"))
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
  if (!is.null(attr(z, "na.action"))){
    attr(attr(z, "na.action"), "class") <- "exclude"
  }
  z
}


