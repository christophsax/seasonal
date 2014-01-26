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
#' @method inspect seas
#' 
#' @examples
#' \dontrun{
#' inspect(AirPassengers)
#' 
#' # pass arbitrary spec/arguments to inspect:
#' inspect(AirPassengers, estimate.maxiter = 1000)  
#' }
#' 
inspect.seas <- function(x, ...){
#   stopifnot(inherits(x, "ts"))

  model <- NULL
  method <- NULL
  modelsearch <- NULL
  calendar <- NULL
  outlier.critical <- NULL
  view <- NULL
  is.static.call <- NULL
  
  require(manipulate)
  
  dotlist <- list(...)
  
#   tsname <- deparse(substitute(x))
  
#   firstrun <- seas(x, ...)

  
  fb <- unique(c(x$model$arima$model, fivebestmdl(x)[,1]))
  

  controls <- list(
    method = picker("SEATS", "X11", label = "adjustment method"),
    model = picker(fb[1], fb[2], fb[3], fb[4], fb[5], fb[6], label = "model"),
    calendar = checkbox(TRUE, "AIC-test: trading days, easter"),
    outlier.critical = slider(2.5, 5, step = 0.1, initial = 4),
    view = picker("unadjusted and adjusted series", 
                  "seasonal component, SI ratio", 
                  "residuals of regARIMA", "residual partial autocorrelation", "sliding spans", "revisions", label = "view"),
    is.static.call = checkbox(FALSE, "show static call")
  )
  
  manipulate({
    lc <- as.list(x$call)
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
    
    SubPlot(view,
            call,
            is.static.call
            )
  }, controls)
  
}


SubPlot <- function(view,
                    call,
                    is.static.call
                    ){

  s <- eval(call)
  
  if (view == "unadjusted and adjusted series"){
    plot(s)
  } else if (view == "seasonal component, SI ratio"){
    monthplot(s)
  } else if (view == "residuals of regARIMA"){
    residplot(s)
  } else if (view == "residual partial autocorrelation"){
    pacf(resid(s), main = "residual partial autocorrelation", ylab = "")
  } else if (view == "sliding spans"){
    plot(slidingspans(s))
  } else if (view == "revisions"){
    # the revision history delivers strange results 
    # drop for R2
    plot(revisions(s))
    
#     # DPKJ version as a temporary workaround (slow)
#     require(tstools)
#     print(recursive(s))
  } else {
    stop("something wrong.")
  }
  
  print(summary(s))
  
  if (is.static.call){
    cat("\nStatic Call:\n")
    static(s, test = TRUE)
  }

}

#' @export
inspect <- function(x) UseMethod("inspect")


#' @method inspect ts
#' @export
inspect.ts <- function(x){
  message("inspect method for 'ts' objects is deprecated. Use it on inspect(seas(x)) instead.")
}