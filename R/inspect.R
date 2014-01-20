# avoid NOTE in R CMD check 
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("picker", "checkbox", "slider", "manipulate"))
}  

#' Inspection Function for Seasonal Adjustment (RStudio only)
#' 
#' Inspect the seasonal adjustment of a time series. \code{inspect} uses the
#' \code{manipulate} package from RStudio and can not be used without it.
#' 
#' @param x an object of class \code{"ts"}
#' @param ...  additional spec/arguments options
#'
#' @export
#' @examples
#' \dontrun{
#' inspect(AirPassengers)
#' 
#' # pass arbitrary spec/arguments to inspect:
#' inspect(AirPassengers, estimate.maxiter = 1000)  
#' }
#' 
inspect <- function(x, ...){
  stopifnot(inherits(x, "ts"))

  model <- NULL
  method <- NULL
  modelsearch <- NULL
  calendar <- NULL
  outlier.critical <- NULL
  view <- NULL
  is.static.call <- NULL
  
  require(manipulate)
  
  dotlist <- list(...)
  
  tsname <- deparse(substitute(x))
  
  firstrun <- seas(x, ...)

  
  fb <- unique(c(firstrun$model$arima$model, fivebestmdl(firstrun)[,1]))
  

  controls <- list(
    method = picker("SEATS", "X11", label = "Adjustment method"),
    model = picker(fb[1], fb[2], fb[3], fb[4], fb[5], fb[6], label = "model"),
    calendar = checkbox(TRUE, "AIC-test: trading days, easter"),
    outlier.critical = slider(2.5, 5, step = 0.1, initial = 4),
    view = picker("Series", "Seasonal component", "Irregular component", "Spectrum original", "Spectrum final", "Residuals of regARIMA", label = "View"),
    is.static.call = checkbox(FALSE, "Show static call")
  )
  
  manipulate({
    lc <- structure(list(quote(seas)), .Names = "")
    lc$x <- parse(text = tsname)[[1]]
    lc$outlier.critical <- outlier.critical
    
    lc$arima.model <- model
      
    if (method == "X11"){
      lc$x11 = list()
    }
     
    if (!calendar){
      lc['regression.aictest'] <- NULL
      names(lc['regression.aictest']) <- "regression.aictest"
    }
    
    if (length(dotlist) > 0){
      lc <- c(lc, dotlist)
    }

    call <- as.call(lc)
    
    SubPlot(x, tsname, view,
            call,
            is.static.call
            )
  }, controls)
  
}


SubPlot <- function(x, tsname, view,
                    call,
                    is.static.call
                    ){

  s <- eval(call)
  
  if (view == "Series"){
    plot(s)
  } else if (view == "Seasonal component"){
    monthplot(s)
  } else if (view == "Irregular component"){
    monthplot(s, choice = "irregular")
  } else if (view == "Spectrum original"){
    spectrum(original(s))
  } else if (view == "Spectrum final"){
    spectrum(final(s))
  } else if (view == "Residuals of regARIMA"){
    residplot(s)
  } else {
    stop("something wrong.")
  }
  
  print(summary(s))
  
  if (is.static.call){
    cat("\nStatic Call:\n")
    static(s, name = tsname, test = TRUE)
  }

}

