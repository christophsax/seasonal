#' @export
plot.seas <- function(x, outliers = TRUE, trend = FALSE){
  ts.plot(cbind(x$data[,'original'], predict(x)), 
          col = c("black", "red"), 
          lwd = c(1, 2),
          main = "Unadjusted and seasonally adjusted series"
  )
  
  if (identical(trend, TRUE)){
    lines(z$data[, 'trend'], col = "blue", lty = "dashed")
  }
  
  if (identical(outliers, TRUE)){
    ol <- x$mdl$regression$variables[str_detect(x$mdl$regression$variables, "\\.")]
    ol.ts <- OutlierTimeSeries(predict(x), ol)
    sym.ts <- ol.ts
    sym.ts[!is.na(sym.ts)] <- 3
    points(predict(x), pch=as.numeric(sym.ts))
    text(predict(x), labels=ol.ts, pos=3, cex=0.75, offset=0.4)
  }
  grid()
}





