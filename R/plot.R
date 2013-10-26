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
    ol <- x$mdl$regression$variables[str_detect(x$mdl$regression$variables, "\\.")]
    ol.ts <- OutlierTimeSeries(final(x), ol)
    sym.ts <- ol.ts
    sym.ts[!is.na(sym.ts)] <- 3
    points(final(x), pch=as.numeric(sym.ts))
    text(final(x), labels=ol.ts, pos=3, cex=0.75, offset=0.4)
  }
}

monthplot.seas <- function(x, choice = "seasonal", ...){
  if (choice == "seasonal"){
    monthplot(x$data[,'adjustfac'], ylab = "factor", main = "seasonal component", ...)
  }
  if (choice == "irregular"){
    monthplot(x$data[,'irregular'], ylab = "factor", main = "irregular component", ...)
  }
}





