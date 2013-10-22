#' @export
plot.seas <- function(x){
  ts.plot(cbind(x$data[,'original'], predict(x)), 
          col = c("black", "red"), 
          lwd = c(1, 2),
          main = "Unadjusted and seasonally adjusted series"
  )
  outlier <- x$mdl$regression$variables[str_detect(x$mdl$regression$variables, "\\.")]
  outlier.ts <- OutlierTimeSeries(predict(x), outlier)
  symbol.ts <- outlier.ts
  symbol.ts[!is.na(symbol.ts)] <- 3
  points(predict(x), pch=as.numeric(symbol.ts))
  text(predict(x), labels=outlier.ts, pos=3, cex=0.75, offset=0.4)
  grid()
}




#' @export
predict.seas <- function(x){
  x$data[,'final']
}

#' @export
ViewOut <- function(){
  path = "C:/Users/seco-sxh/github/seasonal/inst/"
  file = paste0(path, "io/out/", "test", ".out")
  View(readLines(file))
}

#' @export
static <- function(x){
  stopifnot(inherits(x, "seas"))
  if (!is.null(x$spc$force)) {
    opt.force <- paste0('  force.type = "', x$spc$force$type, '",\n')
  } else {
    opt.force <- ""
  }
  z <- paste0('seas(', x$call[[2]], ',\n',
              '  regression.variables = c("', paste(x$mdl$regression$variables, collapse = '", "'), '"),\n',
              '  arima.model = "', x$mdl$arima$model, '",\n',
              opt.force,
              '  regression.aictest = NULL, outlier.types = "none"\n)'
  )
  cat(z)
}

