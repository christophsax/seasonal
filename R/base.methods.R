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
  if (is.null(x$err)){
    cat("\n")
    print(x$err)
  }
}


#' @method print spclist
#' @export
print.spclist <- function(x, ...){
  z <- deparse_spclist(x)
  cat(z, "\n")
  invisible(z)
}




#' @export
#' @method coef seas
coef.seas <- function(object, ...){
  object$est$coefficients
}


#' @export
#' @method nobs seas
nobs.seas <- function(object, ...){
  object$lkstats['nobs']
}
