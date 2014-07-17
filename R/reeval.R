reeval <- function(x, ldots, out = FALSE){
  lc <- as.list(x$call)
  lc <- c(lc, ldots)
  if (out){
    lc$out <- TRUE
  }
    z <- eval(as.call(lc), envir = globalenv())
  z
}


