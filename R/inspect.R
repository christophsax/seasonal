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
#' Inspect may be customized via the \code{fun} argument. One or several plot 
#' functions may be supplied. The plot functions should have a \code{"seas"} 
#' object as their only argument. Several functions must be wrapped in a list
#' (see examples).
#' 
#' @param x an object of class \code{"seas"}
#' @param fun a function or a list of functions (see details)
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
#' 
#' ### customizing inspect
#' 
#' # a single function
#' fc <- function(m){
#'   ts.plot(series(m, "fct", verbose = FALSE))
#' }
#' inspect(m, fc)
#' 
#' # more than one function collected in a list
#' myfun <- list()
#' myfun[['Spectum X-13']] <- function(m){
#'   plot(series(m, "spectrum.specorig", verbose = FALSE)[,-1], t = "l")
#' }
#' myfun[['Spectum R']] <- function(m){
#'   spectrum(diff(log(AirPassengers)), method = "ar")
#' }
#' inspect(m, myfun)
#' 
#' # and a bonus example
#' spaghetti <- function(m, back = 10){
#' ser <- original(m)
#' tx <- time(ser)[(length(ser) - back):length(ser)]
#' z <- NULL
#' for (txi in tx){
#'   assign(as.character(m$call$x), window(ser, end = txi))
#'   z <- cbind(z, final(eval(m$call)))
#' }
#' ts.plot(window(z, start = time(ser)[(length(ser) - back- 15)]), 
#'         col = rainbow(back + 1))
#' }
#' inspect(m, spaghetti)
#' 
#' }
#' @export
inspect <- function(x, fun = NULL){
  require(manipulate)
  
  model <- NULL
  method <- NULL
  modelsearch <- NULL
  calendar <- NULL
  outlier.critical <- NULL
  view <- NULL
  is.static.call <- NULL
  logtrans <- NULL
  
  vl <- list()
  vl[['unadjusted and adjusted series']] <- plot
  vl[['seasonal component, SI ratio']] <- monthplot
  
  vl[['residuals of regARIMA']] <- residplot
  vl[['residual partial autocorrelation']] <- function(x){
    pacf(resid(x), main = "residual partial autocorrelation", ylab = "")
  }
  vl[['sliding spans']] <- function(x){
    dta <- series(x, "slidingspans.sfspans", verbose = FALSE)
    dta <- dta[, -dim(dta)[2]]  # remove last column
    nc <- NCOL(dta)
    ncol <- rainbow(nc)
    ts.plot(dta, col = ncol, main = "slidingspans: seasonal component")
    legend("topleft", colnames(dta), lty = 1, col = ncol, bty = "n", horiz = TRUE)
  }
  vl[['history']] <- function(x){
    dta <- series(x, "history.saestimates", verbose = FALSE)
    nc <- NCOL(dta)
    ncol <- rainbow(nc)
    ts.plot(dta, col = ncol, main = "history: adjusted series")
    legend("topleft", colnames(dta), lty = 1, col = ncol, bty = "n", horiz = TRUE)
  }
  
  if (!is.null(fun)){
    if (is.function(fun)){
      vl[[deparse(substitute(fun))]] <- fun
    } else if (is.list(fun)){
      for (i in 1:length(fun)){
        if (is.function(fun[[i]])){
          if (is.null(names(fun)[i])){  # allways use a name
            names(fun)[i] <- paste("User Plot", i)
          } else if (is.na(names(fun)[i])){
            names(fun)[i] <- paste("User Plot", i)
          }
          vl[[names(fun)[i]]] <- fun[[i]]
        } else {
          warning(paste(names(fun)[i], "is not a function, skipping it."))
        }
      }
    } else {
      stop("fun argument must be either a function or a list of functions.")
    }
  }
  
  fb <- unique(c(x$model$arima$model, fivebestmdl(x)[,1]))
  fb.list <- as.list(fb)
  names(fb.list) <- fb
  
  if (x$transform.function == "log"){
    start.log <- TRUE
  } else {
    start.log <- FALSE
  }
  
  control.list <- list(
    method = picker("SEATS", "X11", label = "adjustment method"),
    model = picker(fb.list, label = "model"),
    calendar = checkbox(TRUE, "AIC-test: trading days, easter"),
    logtrans = checkbox(start.log, "log transformation"),
    outlier.critical = slider(2.5, 5, step = 0.1, initial = 4),
    view = picker(vl, label = "view"),
    is.static.call = checkbox(FALSE, "show static call")
  )
  
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
    
    mod <- eval(as.call(lc))
    view(mod)
    print(summary(mod))
    
    if (is.static.call){
      cat("\nStatic Call:\n")
      static(mod, test = TRUE)
    }
    
  }, control.list)
  
}

