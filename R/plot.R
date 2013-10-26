#' @export
plot.seas <- function(x, outliers = TRUE, trend = FALSE){
  ts.plot(cbind(x$data[,'original'], final(x)), 
          col = c("black", "red"), 
          lwd = c(1, 2),
          ylab = "value",
          main = "unadjusted and seasonally adjusted series"
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

#' @export
residplot <- function(x, outliers = TRUE){
  ts.plot(resid(x), ylab = "value",
          main = "residuals of regARIMA"
  )
  
  if (identical(outliers, TRUE)){
    ol.ts <- outlier(x)
    sym.ts <- ol.ts
    sym.ts[!is.na(sym.ts)] <- 3
    points(resid(x), pch=as.numeric(sym.ts))
    text(resid(x), labels=ol.ts, pos=3, cex=0.75, offset=0.4)
  }
}


#' @export
#' @method monthplot seas
monthplot.seas <- function(x, choice = "seasonal", ...){
  if (choice == "seasonal"){
    monthplot(x$data[,'adjustfac'], ylab = "factor", main = "seasonal component", ...)
  }
  if (choice == "irregular"){
    monthplot(x$data[,'irregular'], ylab = "factor", main = "irregular component", ...)
  }
}





