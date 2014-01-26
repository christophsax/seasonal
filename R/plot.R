#' Plot the Adjusted and Unadjusted Series
#' 
#' \code{plot} method for class \code{"seas"}. Plot the adjusted and unadjusted
#' series, as well as the outliers. Optionally draw the trend series.
#' 
#' @param x  an object of class \code{"seas"}, usually, a result of a 
#'   call to \code{\link{seas}}.
#' @param outliers   logical, should the oultiers be drawn
#' @param trend   logical, should the trend be drawn
#' @param \dots   further arguments passed to \code{ts.plot}.
#'   
#' @return returns a plot as its side effect.
#'   
#' @seealso \code{\link{seas}} for the main function.
#'   
#' @export
#' @method plot seas
#'   
#' @examples
#' \dontrun{
#' x <- seas(AirPassengers, regression.aictest = c("td", "easter"))
#' plot(x)  
#' plot(x, outliers = FALSE)  
#' plot(x, trend = TRUE) 
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

#' Plot the Residuals of an X13 regARIMA model
#' 
#' Plot the residuals of an X13 regARIMA model, as well as the outliers.
#' Optionally draw the trend series.
#' 
#' @param x  an object of class \code{"seas"}, usually, a result of a call to 
#'   \code{\link{seas}}.
#' @param outliers   logical, should the oultiers be drawn.
#' @param \dots   further arguments passed to \code{ts.plot}.
#'   
#' @return returns a plot as its side effect.
#'   
#' @seealso \code{\link{seas}} for the main function.
#'   
#' @export
#'   
#' @examples
#' \dontrun{
#' x <- seas(AirPassengers, regression.aictest = c("td", "easter"))
#' residplot(x)  
#' residplot(x, outliers = FALSE)  
#' }
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


#' Plot Seasonal or Irregular Factors
#' 
#' \code{monthplot} method for class \code{"seas"}. Plot seasonal or irregular factors. 
#' 
#' @param x  an object of class \code{"seas"}, usually, a result of a call to 
#'   \code{\link{seas}}.
#' @param choice  character string, either \code{"seasonal"} or \code{"irregular"}.
#' @param \dots   further arguments passed to \code{monthplot.default}.
#'   
#' @return returns a plot as its side effect.
#'   
#' @seealso \code{\link{seas}} for the main function.
#'   
#' @export
#' @method monthplot seas
#'   
#' @examples
#' \dontrun{
#' x <- seas(AirPassengers, regression.aictest = c("td", "easter"))
#' monthplot(x)  
#' monthplot(x, choice = "irregular")  
#' }
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

#' @rdname spc
#' @export
siratio <- function(x){
  # TODO: make sure you get the right information wether mult or add
  if (x$transform.function == "log"){
    z <- x$data[, 'irregular'] * x$data[, 'seasonal']
  } else {
    z <- x$data[, 'irregular'] + x$data[, 'seasonal']
  }
  na.omit(z)
}



#' @export
hist.seas <- function(x, ...){
  residuals <- resid(x)
  h <- hist(residuals, ...)
  
  xfit<-seq(min(residuals),max(residuals),length=40) 
  yfit<-dnorm(xfit,mean=mean(residuals),sd=sd(residuals)) 
  yfit <- yfit*diff(h$mids[1:2])*length(residuals) 
  lines(xfit, yfit, col = "red", lwd = 2)
}

