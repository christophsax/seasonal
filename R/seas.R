#' Seasonal Adjustment with X13-ARIMA-SEATS
#' 
#' Core function of the seasonal package. By default, \code{seas} calls the 
#' automatic procedures of X-13ARIMA-SEATS to perform a seasonal adjustment that
#' works well in most circumstances.
#' 
#' Seasonal uses the same syntax as X-13ARIMA-SEATS. It is possible to invoke 
#' most options that are available in X-13ARIMA-SEATS. The X-13ARIMA-SEATS 
#' syntax uses specs and arguments, while each spec may contain some arguments. 
#' An additional spec/argument can be added to the \code{seas} function by 
#' separating spec and argument by a \code{.}. For a more extensive description 
#' of the X-13ARIMA-SEATS in \code{seas}, consider the Website on github 
#' (\url{www.github/christophsax/seasonal})
#' 
#' @param x   object of class "ts": time series to seasonaly adjust
#' @param reg   (optional) object of class "ts": one or several user defined 
#'   exogenous variables for regARIMA modelling.
#' @param seats   spec 'seats' without arguments (default). Seasonal adjustment 
#'   by SEATS.
#' @param seats   spec \code{transform} with argument \code{function = "auto"} 
#'   (default). Automatic transformation detection.
#' @param seats   spec \code{regression} with argument \code{chi2test = "yes"} 
#'   (default). Chi Square test for calendar effects.
#' @param seats   spec \code{outlier} without arguments (default). Automatic 
#'   oulier detection.
#' @param seats   spec \code{automdl} without arguments (default). Automatic 
#'   model search with the automodl module.
#' @param out   logical, should the standard output be saved in the "seas" 
#'   object? (increases object size)
#' @param dir   character string with output paht. If specified, the 
#'   X13-ARIMA-SEATS output files are copied to this folder.
#'   
#' @return returns an object of class \code{"seas"}, which is basically a list 
#'   with the following elements:
#'   \item{err}{Warning messages from X13-ARIMA-SEATS}
#'   \item{data}{An object of class "ts", containing the seasonally adjusted
#'   data, the raw data, the trend component, the irregular component and the
#'   seasonal component.}
#'   \item{mdl}{A list with the model specification, similar to "spc". It
#'   typically contains "regression", which contains the regressors and
#'   parameter estimates, and "arima", which contains the ARIMA specification
#'   and the parameter estimates.}
#'   \item{est}{More detailed information on the estimation}
#'   \item{lks}{Summary statistics}
#'   \item{coefficients}{Coefficients of the regARIMA model}
#'   \item{se}{Standard errors of the regARIMA model}
#'   \item{spc}{An object of class "spclist", a list containing everything
#'   that is send to X-13ARIMA-SEATS. Each spec is on the first level, each
#'   argument is on the second level. Checking "spc" is good start for
#'   debugging.}
#'   \item{call}{Function call.}
#'   
#'   The \code{final} function returns the adjusted series, the \code{plot} 
#'   method shows a plot with the unadjusted and the adjusted series. \code{summary}
#'   gives an overview of the regARIMA model. \code{static} returns the static call from above that is needed to replicate an automatic seasonal adjustment procedure 
#'   the model.
#'   
#' @examples
#' x <- seas(AirPassengers) 
#' summary(x)
#' 
#' x2 <- seas(x = AirPassengers, regression.aictest = c("td", "easter")) 
#' summary(x2) 
#' static(x2)
#' 
#' seas(x = AirPassengers, regression.variables = c("td1coef", "easter[1]", 
#' "ao1951.May"), arima.model = "(0 1 1)(0 1 1)", regression.chi2test = "no", 
#' outlier.types = "none", transform.function = "log")
#' #' final(x) 
#' original(x) 
#' resid(x) 
#' coef(x)
#' 
#' plot(x2) 
#' plot(x2, trend = T) monthplot(x2) monthplot(x2, choice = "irregular")
#' 
#' spectrum(final(x)) 
#' spectrum(original(x))
#' 
#' spc(x) mdl(x)
#' 
#' x3 <- seas(AirPassengers, out = T) 
#' out(x3)
#' 
#' x$est$variance 
#' x$lks
#' 
#' inspect(AirPassengers)
#' 
#' @export
seas <- function(x, xreg = NULL, seats = list(), transform.function = "auto", 
                 regression.chi2test = "yes", outlier = list(), 
                 automdl = list(), 
                 out = FALSE, dir = NULL, ...){
  
  stopifnot(inherits(x, "ts"))
  
  # temporary working dir and filenames
  wdir <- paste0(tempdir(), "/x13")
  
  if (!file.exists(wdir)){
    dir.create(wdir)
  }
  
  file.remove(list.files(wdir, full.names = TRUE))
  
  # file name for inputs and outputs (without ending)
  iofile <- paste0(wdir, "/iofile") 
  datafile <- paste0(wdir, "/data.dta")
  regfile <- paste0(wdir, "reg.dta")
  
  ### Write the Data
  write_ts_dat(x, file = datafile)
  
  ### Construct the spclist
  spc <- list()
  class(spc) <- c("spclist", "list")
  
  # add data series
  spc$series$title <- paste0("\"", deparse(substitute(x)), "\"")
  spc$series$file <- paste0("\"", datafile, "\"")
  spc$series$format <- "\"datevalue\""
  spc$series$period <- frequency(x)
  
  # add the default options
  spc$transform$`function` <- transform.function
  spc$regression$chi2test <- regression.chi2test
  spc$outlier <- outlier
  spc$automdl <- automdl
  spc$seats <- seats

  
  # add user defined options
  spc <- mod_spclist(spc, ...)

  # remove double entries, adjust outputs
  spc <- consist_check_spclist(spc)
  
  ### User defined Regressors
  if (!is.null(xreg)){
    write_ts_dat(xreg, file = regfile)

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
  
  
  ### Run X13
  run_x13(spc, file = iofile)
  
  
  ### Import from X13
  z <- list()
  class(spc) <- c("spclist", "list")
  
  # Read errors/warnings and stop on error
  z$err <-  readLines(paste0(iofile, ".err"))
  if (any(str_detect(z$err, "ERROR:"))){
    stop(paste("(messages generated by X13-ARIMA-SEATS)\n", 
               paste(z$err[-(1:4)], collapse = "\n")))
  }
  
  if (any(str_detect(z$err, "Model used for SEATS decomposition is different"))){
    stop("SEATS decomposition invalid. Choose another regARIMA model.")
  }

  # data tables have names that depend on the method, thus a separate call to
  # read_data is needed
  if (!is.null(spc$seats)){
    z$data <- read_data(method = "seats", file = iofile)
  } else if (!is.null(spc$x11)){
    z$data <- read_data(method = "x11", file = iofile)
  } else {
    warning("dont know what to read if neither 'seats' nor 'x11' are specified (TODO).")
  }
  
  z$mdl <- read_mdl(iofile)
  z$est <- read_est(iofile)
  z$lks <- read_lks(iofile)
  
  z$coefficients <- z$est$coefficients
  z$se <- z$est$se
  
  if (out){
    z$out <-  readLines(paste0(iofile, ".out"))
  }
  
  z$spc <- spc
  
  ### Save output files if 'dir' is specified
  
  if (!is.null(dir)){
    flist <- list.files(wdir, full.names = TRUE)
    file.copy(flist, dir, overwrite = TRUE)
    message("All X13-ARIMA-SEATS output files have been copied to '", dir, "'.")
  }
  
  ### Final transformations
  cndata <- colnames(z$data)
  z$data <- cbind(z$data, x)
  colnames(z$data) <- c(cndata, 'original')
  
  z$call <- match.call()
  class(z) <- "seas"
  z
}


mod_spclist <- function(x, ...){
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

consist_check_spclist <-function(x){
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
  
  # priority: 1. aictest, 2. chi2test (default)
  if (!is.null(x$x11regression) & !is.null(x$regression)){
    x$regression <- NULL
  }
  
  ### ensure correct output
  
  # seats and x11 have different output tables
  if (!is.null(x$seats)){
    x$seats <- list(save = c("s10", "s11", "s12", "s13", "s16", "s18"))
  } else if (!is.null(x$x11)){
    x$x11 <- list(save = c("d10", "d11", "d12", "d13", "d16", "e18"))
  } else {
    stop("wrong method.")
  }
  
  # if force is present, return adjusted output
  if (!is.null(x$force$type)){
    x$force$save = "saa"
  }
  
  # always return estimate model
  always.add <- c("model", "estimates", "residuals", "lkstats")
  if (is.null(x$estimate$save)){
    x$estimate$save <- always.add
  } else {
    to.add <- always.add[!(always.add %in% x$estimate$save)]
    x$estimate$save <- c(x$estimate$save, to.add)
  }
  
  x
}

run_x13 <- function(x, method = "seats", file){
  stopifnot(inherits(x, "spclist"))
  
  # ---------------------
  # if everything works, need to be platform independent
  
  x13dir <- system.file("exec", package = "seasonal")
  
#   x13dir <- "C:/Users/seco-sxh/github/seasonal/inst/"
  
#   x13dir <- "~/seasonal/inst/"
  # ---------------------
  
  txt <- parse_spclist(x)
  writeLines(txt, con = paste0(file, ".spc"))
  
  # platform dependent call to X13
  # The -p flag specifies that page breaks and headers will be suppressed in 
  # the main output file
  if (Sys.info()['sysname'] == "Darwin"){
    system(paste0(x13dir, "/x13as-mac ", file), intern = TRUE)
  } else if (Sys.info()['sysname'] == "Linux"){
    system(paste0(x13dir, "/x13as-linux ", file))
  } else if (.Platform$OS.type == "windows"){
#     system(paste0(x13dir, "x13/x13as.exe -p", file))
    shell(paste0(x13dir, "/x13as.exe ", file))
    #   shell(paste0(path, "x13/x13as.exe ", path, "io/", iofile, " ", path, "io/out/test")) 
  }
  
}


