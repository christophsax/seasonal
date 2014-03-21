#' Universal import of X-13ARIMA-SEATS output
#' 
#' With the \code{series} function, it is possible to import all tables that can
#' be saved in X-13ARIMA-SEATS (exception: composite spec).
#' 
#' If the save argument has not been specified in the model call, \code{series}
#' re-evaluates the call with the 'forecast' spec enabled. Note that
#' re-evaluation doubles the overall computational time. If you want to speed it
#' up, you have to be explicit about the output in the model call.
#' 
#' @param x  an object of class \code{"seas"}.
#' @param series  character, short or long names of an X-13ARIMA-SEATS save file. See the
#'   manual for a description. If a long name is specified, it needs to be
#'   combined with the spec name and separated by a dot (it is not unique,
#'   otherwise). More than one series can be specified. See examples.
#'   
#'   
#' @export
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' m <- seas(AirPassengers)
#' series(m, "fct")  # re-evaluate with the forcast activated 
#' 
#' # more than one series
#' series(m, c("d7", "d8"))
#' 
#' m <- seas(AirPassengers, forecast.save = "fct")
#' series(m, "fct")
#' 
#' # using long names
#' series(m, "forecast.forecasts")
#' 
#' 
#' # Some X-13ARIMA-SEATS functions can be replicated in R:
#' 
#' # X-13ARIMA-SEATS spectrum
#' plot(series(m, "spectrum.specorig")[,-1], t = "l")
#' # R equivalent: spectrum from stats
#' spectrum(diff(log(AirPassengers)), method = "ar")
#' 
#' # X-13ARIMA-SEATS pacf
#' x13.pacf <- series(m, "identify.pacf")
#' plot(x13.pacf[,1:2])
#' lines(x13.pacf[,3])
#' lines(-x13.pacf[,3])
#' 
#' # R equivalent: pacf from stats
#' pacf(AirPassengers, lag.max = 35)
#' 
#' }
#' 
series <- function(x, series, reeval = TRUE){
  stopifnot(inherits(x, "seas"))
  
  data(specs, envir = environment())  # avoid side effects
  
  is.dotted <- grepl("\\.", series)
  
  # check validiy of short or long names
  is.valid <- logical(length = length(series))
  is.valid[is.dotted] <- series[is.dotted] %in% SPECS$long[SPECS$is.series]
  is.valid[!is.dotted] <- series[!is.dotted] %in% SPECS$short[SPECS$is.series]

  if (any(!is.valid)){
    stop(paste0("\nseries not valid: ", paste(series[!is.valid], collapse = ", "), "\nsee ?series for a list of importable series "))
  }
  
  # unique short names
  series.short <- unique(c(series[!is.dotted], 
    merge(data.frame(long = series[is.dotted]), SPECS)$short))

  # reeval with non present output
  if (reeval){
    # check which series are already there
    if (is.null(x$series)){
      series.NA <- series.short
    } else {
      series.NA <- setdiff(series.short, names(x$series))
    }
    
    activated <- NULL
    reeval.dots <- list()
    for (i in seq_along(series.NA)){
      series.NA.i <- series.NA[i]
      spec.i <- as.character(SPECS[SPECS$short == series.NA.i & SPECS$is.series, ]$spec)
      if (length(spec.i) > 1) stop("not unique!!!!!!!!!!")
      if (!spec.i %in% names(x$spc)){
        activated <- c(activated, spec.i)
      }
      
      reeval.dots[[i]] <- series.NA.i
      names(reeval.dots)[i] <- paste0(spec.i, '.save')
    }
    
    if (length(activated) > 0){
      message(paste("specs have been added to the model:", 
                    paste(unique(activated), collapse = ", ")))
    }
    
    if (length(reeval.dots) > 0){
      x <- reeval(x, reeval.dots, out = FALSE)
    }
  }

  do.call(cbind, x$series[series.short])
}


