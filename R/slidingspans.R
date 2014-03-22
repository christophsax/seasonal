#' @rdname out
#' @export
slidingspans <- function(x, ...){
  ldots <- list(...)
  
  reeval.dots <- list(
    slidingspans.save = c("saspans", "sfspans", "tdspans", "chngspans", 
                          "ychngspans")
  )
  reeval.dots <- c(reeval.dots, ldots)
  z <- reeval(x, reeval.dots)
  
  n.line <- grep("  Sliding spans analysis", z$out)
  if (length(n.line) == 0){
    warning("No history analysis found in the output. Returning the standard output.")
  } else {
    z$slidingspans <- z$out[n.line[1]:length(z$out)]
    class(z$slidingspans) <- "out"
  }
  class(z) <- "slidingspans"
  z
}




#' @export
#' @method print slidingspans
print.slidingspans <- function(x, ...){
  print(x$slidingspans)
}


