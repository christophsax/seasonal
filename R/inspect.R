# avoid NOTE in R CMD check 
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("picker", "checkbox", "slider", "manipulate"))
}  

#' Interactively Inspect a Seasonal Adjustment Model (RStudio only)
#' 
#' Interactively inspect a \code{"seas"} object. \code{inspect} uses the 
#' \code{manipulate} package and can only be used with RStudio. The goal of 
#' \code{inspect} is to summarize all relevant options, plots and statistics 
#' that should be usually considered.
#' 
#' The \code{inspect} function opens an interactive window that allows for the 
#' manipulation of a number of arguments. It offers several views to analyze the
#' series graphically. With each change, the adjustment process and the 
#' visualizations are recalculated. Summary statics are shown in the R console.
#' 
#' With the \code{Show static call} option, a replicable static call is also
#' shown in the console. Note that this option will double the time for
#' recalculation, as the static function also tests the static call each time
#' (this is a beta feature of seasonal, which allows intensive testing; it may
#' be disabled in future versions).
#' 
#' @param x an object of class \code{"seas"}
#' 
#' @seealso \code{\link{seas}} for the main function of seasonal.
#' 
#' @references R Studio IDE: \url{http://www.rstudio.com/ide/}
#'   
#' @examples
#' \dontrun{
#' 
#' m <- seas(AirPassengers)
#' 
#' inspect(m)
#' 
#' # pass arbitrary spec-arguments to inspect:
#' m2 <- seas(AirPassengers, estimate.maxiter = 1000)
#' inspect(m2)
#' }
#' 
#' 
#' @export
inspect <- function(x) UseMethod("inspect")

#' @rdname inspect
#' @method inspect seas
#' @export
inspect.seas <- function(x){
  
  model <- NULL
  method <- NULL
  modelsearch <- NULL
  calendar <- NULL
  outlier.critical <- NULL
  view <- NULL
  is.static.call <- NULL
  logtrans <- NULL
  
  require(manipulate)

  fb <- unique(c(x$model$arima$model, fivebestmdl(x)[,1]))
  
  if (x$transform.function == "log"){
    start.log <- TRUE
  } else {
    start.log <- FALSE
  }

  controls <- list(
    method = picker("SEATS", "X11", label = "adjustment method"),
    model = picker(fb[1], fb[2], fb[3], fb[4], fb[5], fb[6], label = "model"),
    calendar = checkbox(TRUE, "AIC-test: trading days, easter"),
    logtrans = checkbox(start.log, "log transformation"),
    outlier.critical = slider(2.5, 5, step = 0.1, initial = 4),
    view = picker("unadjusted and adjusted series", 
                  "seasonal component, SI ratio", 
                  "residuals of regARIMA", "residual partial autocorrelation", 
                  "sliding spans", 
                  "revisions", 
                  label = "view"),
    is.static.call = checkbox(FALSE, "show static call")
  )
  
  # reduce model list if only 5 available
  if (length(na.omit(fb) == 5)){
    controls$model = picker(fb[1], fb[2], fb[3], fb[4], fb[5], label = "model")
  }
  
  manipulate({
    lc <- as.list(x$call)
    lc$outlier.critical <- outlier.critical
    lc$arima.model <- model
      
    if (method == "X11"){
      lc$x11 = list()
    }
    
    if (logtrans){
      lc$transform.function = "log"
    } else {
      lc$transform.function = "none"
    }
    
    if (calendar){
      lc$regression.aictest <- c("td", "easter")
    } else {
      lc['regression.aictest'] <- NULL
      names(lc['regression.aictest']) <- "regression.aictest"
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
    plot(revisions(s))
  } else {
    stop("something wrong.")
  }
  
  print(summary(s))
  
  if (is.static.call){
    cat("\nStatic Call:\n")
    static(s, test = TRUE)
  }

}


#' @method inspect ts
#' @export
inspect.ts <- function(x){
  message("The 'inspect' method for 'ts' objects is deprecated.\nUse inspect(seas(x)) instead. See ?inspect for details.")
}
