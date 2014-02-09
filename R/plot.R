#' Seasonal Adjustment Plots
#' 
#' Functions to graphically analyze a \code{"seas"} object. More graphs can be 
#' plotted with standard R functions (see examples). For diagnostical 
#' statistics, see \code{\link{qs}}.
#' 
#' \code{plot} calls the plot method for class \code{"seas"}. It plots the 
#' adjusted and unadjusted series, as well as the outliers. Optionally draws the
#' trend series.
#' 
#' \code{residplot} plots the residuals and the outliers.
#' 
#' \code{monthplot} calls the monthplot method for class \code{"seas"}. It plot 
#' the seasonal and SI component periodwise. Like the default method `monthplot`
#' can be used for all frequencies.
#' 
#' \code{plot.slidingspans} calls the plot method for objects of class
#' \code{"slidingspans"}. It draws the seasonal component for the analyzed
#' spans.
#' 
#' \code{plot.revisions} calls the plot method for objects of class
#' \code{"revisons"}. It draws concurrent and the latest estimation of the
#' seasonal adjusted series.
#' 
#' @param x  an object of class \code{"seas"}, usually, a result of a call to
#'   \code{\link{seas}}.
#' @param outliers   logical, should the oultiers be drawn.
#' @param trend      logical, should the trend be drawn.
#' @param choice     character string, \code{"seasonal"} (default) or
#'   \code{"irregular"}.
#' @param main    character string, title of the graph.
#' @param \dots   further arguments passed to the plotting functions.
#'   
#' @return All plot functions returns a plot as their side effect.
#'   
#' @seealso \code{\link{seas}}, for the main function.
#' @seealso \code{\link{qs}}, for diagnostical statistics.
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
#' @method plot seas
#'   
#' @examples
#' \dontrun{
#' 
#' m <- seas(AirPassengers)
#' 
#' plot(m)  
#' plot(m, outliers = FALSE)  
#' plot(m, trend = TRUE) 
#' 
#' residplot(m)
#' residplot(m, outliers = FALSE)  
#' 
#' monthplot(m)
#' 
#' plot(slidingspans(m))
#' plot(revisions(m))
#' 
#' # use R functions to analyze "seas" models
#' pacf(resid(m))
#' spectrum(diff(resid(m)))
#' plot(density(resid(m)))
#' qqnorm(resid(m))
#' }
plot.seas <- function(x, outliers = TRUE, trend = FALSE, main = "unadjusted and seasonally adjusted series",...){

  ts.plot(cbind(original(x), final(x)), 
          col = c("black", "red"), 
          lwd = c(1, 2),
          main = main, ...
  )
  
  if (identical(trend, TRUE)){
    lines(x$data[, 'trend'], col = "blue", lty = "dashed")
  }
  
  if (identical(outliers, TRUE)){
    ol.ts <- outlier(x)
    sym.ts <- ol.ts
    sym.ts[!is.na(sym.ts)] <- 3
    points(final(x), pch=as.numeric(sym.ts))
    text(final(x), labels=ol.ts, pos=3, cex=0.75, offset=0.4)
  }
}

#' @rdname plot.seas
#' @export
residplot <- function(x, outliers = TRUE, ...){
  ts.plot(resid(x), ylab = "value",
          main = "residuals of regARIMA", ...
  )
  
  if (identical(outliers, TRUE)){
    ol.ts <- outlier(x)
    sym.ts <- ol.ts
    sym.ts[!is.na(sym.ts)] <- 3
    points(resid(x), pch=as.numeric(sym.ts))
    text(resid(x), labels=ol.ts, pos=3, cex=0.75, offset=0.4)
  }
}


#' @rdname plot.seas
#' @export
#' @method monthplot seas
monthplot.seas <- function(x, choice = "seasonal", ...){
  if (choice == "seasonal"){
    monthplot(x$data[,'seasonal'], ylab = "", lwd = 2, col = "red", main = "seasonal component, SI ratio", ...)
    monthplot(siratio(x), col = "blue", type = "h", add = TRUE)
  }
  if (choice == "irregular"){
    if (is.null(main)){
      main <- "irregular component"
    }
    monthplot(x$data[,'irregular'], ylab = "", main = main)
  }
}



siratio <- function(x){
  if (x$transform.function == "log"){
    z <- x$data[, 'irregular'] * x$data[, 'seasonal']
  } else {
    z <- x$data[, 'irregular'] + x$data[, 'seasonal']
  }
  na.omit(z)
}



#' @rdname plot.seas
#' @method plot slidingspans
#' @export
plot.slidingspans <- function(x, main = "sliding spans", ...){
  ser <- x$sfspans
  nser <- dim(ser)[2]
  col = rev(rainbow(nser-1))
  ts.plot(ser[, -nser], col = col, lty = 1, main = main, ...)
  legend("topleft", colnames(ser)[-nser], lwd = 2, lty = 1, col = col, bty = "n", horiz = TRUE)
}


#' @rdname plot.seas
#' @method plot revisions
#' @export
plot.revisions <- function(x, main = "revisions", ...){
  ts.plot(x$sae, col = c("black", "red"), main = main, ...)
  
  legend("topleft", c("concurrent estimation", "final estimation"), lty = 1, col = c("black", "red"), bty = "n", horiz = TRUE)
}
