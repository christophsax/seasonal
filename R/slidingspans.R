#' @rdname out
#' @export
slidingspans <- function(x, view = FALSE, ...){
  ldots <- list(...)
  if (length(ldots) == 0){
    ldots$slidingspans = list()
  } else {
    if (!any(grepl("slidingspans", names(ldots)))){
      ldots$slidingspans = list()
    }
  }
  z <- reeval(x, ldots)
  
  z$slidingspans <- z$out[grep("  Sliding spans analysis", z$out)[1]:length(z$out)]
  class(z$slidingspans) <- "out"
  class(z) <- "slidingspans"
  z
}

#' @rdname plot.seas
#' @method plot slidingspans
#' @export
plot.slidingspans <- function(x, main = "sliding spans", ylab = "Seasonal Component", xlab = "", ...){
  ser <- x$sfspans
  nser <- dim(ser)[2]
  col = rev(rainbow(nser-1))
  ts.plot(ser[, -nser], col = col, lty = 1, lwd = 2, main = main, ylab = ylab, xlab = "", ...)
  legend("topleft", colnames(ser)[-nser], lwd = 2, lty = 1, col = col, bty = "n", horiz = TRUE)
#   mae <- formatC(mean(ser[, nser], na.rm = TRUE), 
#           digits = max(3, getOption("digits") - 3))
#   legend("bottomleft", paste("MAE:", mae), bty = "n", horiz = TRUE)
}



#' @export
#' @method print slidingspans
print.slidingspans <- function(x){
  print(x$slidingspans)
}


