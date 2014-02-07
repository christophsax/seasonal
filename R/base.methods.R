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
  
  x$coefficients <- coef(x)  # make default method for coef working
  
  # convert to class "lm" and use the corresponding method
  attr(x, "class") <- "lm"
  print(x, ...)
  
  if (length(x$err) > 5){
    cat("X13-ARIMA-SEATS messages:", x$err[-c(1:5)], sep = "\n")
  } 
}


#' @method print spclist
#' @export
print.spclist <- function(x, ...){
  cat(deparse_spclist(x))
}




#' @export
#' @method coef seas
coef.seas <- function(object, ...){
  object$estimates$coefficients
}


#' @export
#' @method nobs seas
nobs.seas <- function(x, ...){
  x$lkstats['nobs']
}
