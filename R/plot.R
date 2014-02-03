#' Plot Functions for Seasonal Adjustment Models
#' 
#' Functions to graphically analyze a seasonal adjustment model.
#' 
#' \code{plot} calls the plot method for class \code{"seas"}. It plots the adjusted and unadjusted
#' series, as well as the outliers. Optionally draws the trend series.
#' 
#' \code{monthplot} calls the monthplot method for class \code{"seas"}. It plot the seasonal and SI component periodwise. Like the default method `monthplot` can be used for all frequencies.

#' \code{residplot} plots the residuals and the outliers.
#' 
#' \code{plot.slidingspans} calls the plot method for models of class \code{"slidingspans"}. It draws the seasonal component for the analyzed spans.
#' 
#' \code{plot.revisions} calls the plot method for models of class \code{"revisons"}. It draws concurrent and the latest estimation of the seasonal component.
#' 
#' \code{hist.revisions} calls the hist method for models of class \code{"seas"}. Plots a histogram of the residuals and compares it to a normal distribution.
#' 
#' 
#' @param x  an object of class \code{"seas"}, usually, a result of a 
#'   call to \code{\link{seas}}.
#' @param outliers   logical, should the oultiers be drawn
#' @param trend   logical, should the trend be drawn
#' @param choice   character string, \code{"seasonal"} (default) or \code{"irregular"}.
#' @param \dots   further arguments passed to other methods.
#'   
#' @return All plot functions returns a plot as their side effect.
#'   
#' @seealso \code{\link{seas}} for the main function.
#'   
#' @export
#' @method plot seas
#'   
#' @examples
#' \dontrun{
#' x <- seas(AirPassengers)
#' 
#' plot(x)  
#' plot(x, outliers = FALSE)  
#' plot(x, trend = TRUE) 
#' 
#' residplot(x)
#' residplot(x, outliers = FALSE)  
#' 
#' monthplot(x)
#' 
#' plot(slidingspans(x))
#' 
#' plot(revisions(x))
#' 
#' hist(x)
#' 
#' # Using R base functions for analyzing "seas" models.
#' pacf(resid(x))
#' spectrum(diff(resid(x)))
#' 
#' }
plot.seas <- function(x, outliers = TRUE, trend = FALSE, main = "unadjusted and seasonally adjusted series", 
                      ylab = "value",...){

  ts.plot(cbind(original(x), final(x)), 
          col = c("black", "red"), 
          lwd = c(1, 2),
          ylab = "value",
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
    monthplot(x$data[,'irregular'], ylab = ylab, main = main)
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
#' @method hist seas
#' @export
hist.seas <- function(x, ...){
  residuals <- resid(x)
  h <- hist(residuals, ...)
  
  xfit<-seq(min(residuals),max(residuals),length=40) 
  yfit<-dnorm(xfit,mean=mean(residuals),sd=sd(residuals)) 
  yfit <- yfit*diff(h$mids[1:2])*length(residuals) 
  lines(xfit, yfit, col = "red", lwd = 2)
}

