#' Import X-13ARIMA-SEATS Output Tables
#' 
#' With the exception the composite spec, the \code{series} function imports all
#' tables that can be saved in X-13ARIMA-SEATS.
#' 
#' If the save argument is not specified in the model call, \code{series} 
#' re-evaluates the call with the corresponding specs enabled (also returning a 
#' message). Note that re-evaluation doubles the overall computational time. If 
#' you want to accelerate the procedure, you have to be explicit about the 
#' output in the model call (see examples).
#' 
#' @param x  an object of class \code{"seas"}.
#' @param series  character vector, short or long names of an X-13ARIMA-SEATS 
#'   table. If a long name is specified, it needs to be combined with the spec 
#'   name and separated by a dot (it is not unique, otherwise). More than one 
#'   series can be specified (see examples).
#' @param reeval logical, if \code{TRUE}, the model is re-evaluated with the 
#'   corresponding specs enabled.
#' @param verbose logical, if \code{TRUE}, a message is returned if a spec is added
#'   during reevaluation.
#'   
#'   
#' @seealso \code{\link{seas}} for the main function.
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
#'   
#' @export
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' m <- seas(AirPassengers)
#' series(m, "fct")  # re-evaluate with the forcast activated 
#' 
#' # more than one series
#' series(m, c("d7", "d8", "fct"))
#' 
#' m <- seas(AirPassengers, forecast.save = "fct")
#' series(m, "fct") # no re-evaluation (much faster!)
#' 
#' # using long names
#' series(m, "forecast.forecasts")
#' 
#' # history spec
#' series(m, "history.trendestimates") 
#' series(m, "history.sfestimates") 
#' series(m, "history.saestimates") 
#' series(m, c("history.sfestimates", "history.trendestimates")) 
#' 
#' # slidingspans spec
#' series(m, "slidingspans.sfspans") 
#' series(m, "slidingspans.tdspans") 
#' 
#' ### Some X-13ARIMA-SEATS functions can be replicated in R:
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
#' # R equivalent: pacf from stats
#' pacf(AirPassengers, lag.max = 35)
#' 
#' 
#' ### advanced examples
#' # (for more examples, see the wiki.)
#' 
#' # trading day and easter adjustment w/o seasonal adjustment
#' summary(m)
#' re <- series(m, "estimate.regressioneffects")
#' ce <- re[, 'Trading.Day'] + re[, 'Holiday'] 
#' # be aware of the log transformation
#' AirPassengersWoTd <- exp(log(AirPassengers) - ce)
#' }
#' 
series <- function(x, series, reeval = TRUE, verbose = TRUE){
  stopifnot(inherits(x, "seas"))
  
  SPECS <- NULL 
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
    j <- 1  # flexible index to allow for an arbitrary number of requirements
    for (i in seq_along(series.NA)){
      series.NA.i <- series.NA[i]
      spec.i <- as.character(SPECS[SPECS$short == series.NA.i & SPECS$is.series, ]$spec)
      if (length(spec.i) > 1) stop("not unique!!!!!!!!!!")
      if (!spec.i %in% names(x$spc)){
        activated <- c(activated, spec.i)
      }
      requires.i <- as.character(SPECS[SPECS$short == series.NA.i & SPECS$is.series, ]$requires)

      if (length(requires.i) > 0){
        requires.list <- eval(parse(text = paste("list(", requires.i, ")")))
        reeval.dots <- c(reeval.dots, requires.list)
        j <- length(reeval.dots) + 1
      }
      reeval.dots[[j]] <- series.NA.i
      names(reeval.dots)[j] <- paste0(spec.i, '.save')
      j <- j + 1
    }

    if (verbose & length(activated) > 0){
      message(paste("specs have been added to the model:", 
                    paste(unique(activated), collapse = ", ")))
    }
    
    if (length(reeval.dots) > 0){
      x <- reeval(x, reeval.dots, out = FALSE)
    }
  }

  do.call(cbind, x$series[series.short])
}


