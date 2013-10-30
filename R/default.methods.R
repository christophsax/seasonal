#' @import stats
#' @method print seas
#' @export
print.seas <- function(x, ...){
  # calls print.lm
  #
  # Args:
  #   x:           an object of class "seas"
  #   ...:         further arguments, not used
  #
  # Returns: 
  #   prints the object with print.lm as a side effect
  print.lm(x, ...)
  
  cat("\n\nX13-ARIMA-SEATS messages:", x$err[-c(1:5)], sep = "\n")
}


#' @method print spclist
#' @export
print.spclist <- function(x, ...){
  cat(parse_spclist(x))
}


#' @method residuals seas
#' @export
residuals.seas <- function(object, ...){
  object$data[,'residuals']
}

