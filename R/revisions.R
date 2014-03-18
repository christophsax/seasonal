
#' @rdname out
#' @export
revisions <- function(x, ...){
  ldots <- list(...)
  
  reeval.dots <- list(
    history.estimates = c("sadj", "sadjchng", "trend", "trendchng", "seasonal"),
    history.save = c("saestimates", "chngestimates", "sarevisions", "sfestimates", "trendestimates")
    )
  
  reeval.dots <- c(reeval.dots, ldots)

  z <- reeval(x, reeval.dots)

  n.line <- grep("history analysis", z$out)
  if (length(n.line) == 0){
    warning("No history analysis found in the output. Returning the standard output.")
  } else {
    z$revisions <- z$out[grep("  History analysis", z$out)[1]:length(z$out)]
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



#' @rdname plot.seas
#' @method plot revisions
#' @export
plot.revisions <- function(x, series = c("saestimates", "chngestimates", "sarevisions", "sfestimates", "trendestimates")){
  series <- match.arg(series)
  
  class(x) <- "seas"
  dta <- series(x, paste0("history.", series), reeval = FALSE)
  
  nc <- NCOL(dta)
  ncol <- rainbow(nc)
  ts.plot(dta, col = ncol, main = series)
  
  if (nc > 1){
    legend("topleft", colnames(dta), lty = 1, col = ncol, bty = "n", horiz = TRUE)
  }
}



