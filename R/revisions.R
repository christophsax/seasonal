#' @rdname out
#' @export
revisions <- function(x, ...){
  
  .Deprecated("series", package=NULL, "Function is deprecated and will be removed soon. Use the more univeral series function. See examples in ?series.",
              old = as.character(sys.call(sys.parent()))[1L])
  
  
  ldots <- list(...)
  
  reeval.dots <- list(
    history.estimates = c("sadj", "sadjchng", "trend", "trendchng", "seasonal"),
    history.save = c("saestimates", "sfestimates", "trendestimates", 
                     "chngestimates", "sarevisions")
  )
  reeval.dots <- c(reeval.dots, ldots)
  z <- reeval(x, reeval.dots)

  n.line <- grep("  History analysis", z$out)
  if (length(n.line) == 0){
    warning("No history analysis found in the output. Returning the standard output.")
  } else {
    z$revisions <- z$out[n.line[1]:length(z$out)]
    class(z$revisions) <- "out"
  }
  class(z) <- "revisions"
  z
}


#' @export
#' @method print revisions
print.revisions <- function(x, ...){
  print(x$revisions)
}






