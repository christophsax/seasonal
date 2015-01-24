
#' Defunct Functions
#' 
#' Functions are defunct. Use the more univeral series function.
#' @param x object of class \code{"seas"}
#' @param ...  further arguments
#' @rdname seasonal-defunct
#' @seealso \code{\link{series}}, for universal X-13 output extraction.
#' @examples
#' \dontrun{
#' # history spec (replaces the defunct 'revisions' function)
#' series(m, "history.trendestimates") 
#' series(m, "history.sfestimates") 
#' series(m, "history.saestimates") 
#' series(m, c("history.sfestimates", "history.trendestimates")) 
#' 
#' # slidingspans spec (replaces the defunct 'slidingspans' function)
#' series(m, "slidingspans.sfspans") 
#' series(m, "slidingspans.tdspans") 
#' 
#' # regressioneffects
#' series(m, "estimate.regressioneffects")
#' }
#' @export
slidingspans <- function(x, ...){
  ldots <- list(...)
  
  .Defunct("series", package=NULL, "Function is defunct. Use the more univeral series function. See examples in ?series.")
  
  reeval.dots <- list(
    slidingspans.save = c("saspans", "sfspans", "tdspans", "chngspans", 
                          "ychngspans")
  )
  reeval.dots <- c(reeval.dots, ldots)
  z <- reeval(x, reeval.dots, out = TRUE)
  
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

#' @rdname seasonal-defunct
#' @export
regressioneffects <- function(x){
  .Defunct("series", package=NULL, "Function is defunct. Use the more univeral series function. See examples in ?series.")

  series(x, "estimate.regressioneffects") 
}

#' @rdname seasonal-defunct
#' @export
revisions <- function(x, ...){
  
  .Defunct("series", package=NULL, "Function is removed. Use the more univeral series function. See examples in ?series.")

  ldots <- list(...)
  
  reeval.dots <- list(
    history.estimates = c("sadj", "sadjchng", "trend", "trendchng", "seasonal"),
    history.save = c("saestimates", "sfestimates", "trendestimates", 
                     "chngestimates", "sarevisions")
  )
  reeval.dots <- c(reeval.dots, ldots)
  z <- reeval(x, ldots = reeval.dots, out = TRUE)
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




#' @rdname seasonal-defunct
#' @method plot revisions
#' @export
plot.revisions <- function(x, ...){
  series <- match.arg(series)
  
  class(x) <- "seas"
  dta <- series(x, paste0("history.", "saestimates"), reeval = FALSE)
  
  nc <- NCOL(dta)
  ncol <- rainbow(nc)
  ts.plot(dta, col = ncol, main = series)
  
  if (nc > 1){
    legend("topleft", colnames(dta), lty = 1, col = ncol, bty = "n", horiz = TRUE)
  }
}

#' @rdname seasonal-defunct
#' @method plot slidingspans
#' @export
plot.slidingspans <- function(x, ...){
  series <- match.arg(series)
  
  class(x) <- "seas"
  dta <- series(x, paste0("slidingspans.", series), reeval = FALSE)
  
  # 0.40.0 behavior, do not show Max Diff
  if (series == "sfspans"){
    dta <- dta[, -dim(dta)[2]]
  }
  
  nc <- NCOL(dta)
  ncol <- rainbow(nc)
  ts.plot(dta, col = ncol, main = "sfspans")
  
  if (nc > 1){
    legend("topleft", colnames(dta), lty = 1, col = ncol, bty = "n", horiz = TRUE)
  }
}




