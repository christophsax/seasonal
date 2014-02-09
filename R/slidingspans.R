#' @rdname out
#' @export
slidingspans <- function(x, ...){
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




#' @export
#' @method print slidingspans
print.slidingspans <- function(x, ...){
  print(x$slidingspans)
}


