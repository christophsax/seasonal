#' Diagnostical Re-Evaluation
#' 
#' For diagnostical purposes, these functions re-evaluate an object of class 
#' seas and capture the full content or parts of the .out file from 
#' X-13ARIMA-SEATS.
#' 
#' The \code{out} function shows the full content of the \code{.out} file form
#' X-13ARIMA-SEATS. The \code{slidingspans} and \code{revisions} function call
#' the \code{slidingspans} and \code{history} spec of X-13ARIMA-SEATS and show
#' the respective parts of the \code{.out} file. Note that against the
#' convention, the \code{history} spec is called by the function
#' \code{revision}, in order to avoid a naming collision with the function from
#' the \code{utils} pacakge.
#' 
#' For how to enter spec-arguments options, see the details in 
#' \code{"\link{seas}"}. In the \code{out} function, \code{...} are useful to 
#' add adtional outputs to the \code{.out} file. In the \code{slidingspans} and 
#' \code{revisions} function, \code{...} are used to add aditional options to 
#' the \code{slidingspans} and \code{history} spec.
#' 
#' @param x an object of class "seas" to re-evaluate
#'   
#' @param ... aditional spec-arguments options (see details).
#'   
#' @return invisible, the full content or parts of the .out file as a (large) 
#'   character vector. If \code{viewer = TRUE}, the content of the character 
#'   vector is shown as a side effect.
#'   
#' @export
out <- function(x, viewer = TRUE, line = 1, n = 500, search = NULL, ...){
  ldots <- list(...)
  z <- reeval(x, ldots)
  if (viewer){
    viewer(z, line = line, n = n, search = search)
  }
  return(invisible(z))
}



#' @export
revisions <- function(x, ...){
  ldots <- list(...)
  if (length(ldots) == 0){
    ldots$history = list()
  } else {
    if (!any(grepl("history", names(ldots)))){
      ldots$history = list()
    }
  }
  z <- reeval(x, ldots)
  z <- z[grep("History", z)[1]:length(z)]
  viewer(z, line = 1, n = length(z))
  return(invisible(z))
}



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
  z <- z[grep("[Ss]liding spans", z)[1]:length(z)]
  viewer(z, line = 1, n = length(z))
  return(invisible(z))
}



reeval <- function(x, ldots){
  lc <- as.list(x$call)
  lc <- c(lc, ldots)
  lc$out <- TRUE
  z <- eval(as.call(lc), envir = globalenv())
  z$out
}


viewer <- function(x, line = 1, n = 25, search = NULL){
  stopifnot(inherits(x, "character"))
  
  if (!is.null(search)){
    search.res <- which(grepl(search, x))
    if (length(search.res > 0)){
      line <- search.res[1]
    } else {
      message("string not found")
      return(NULL)
    }
  }
  
  l <- line
  status <- 1
  while (status == 1){
    
    if (l >= (length(x) - n + 1)){
      l <- length(x) - n + 1
      status <- 0
    }
    if (l < 0){
      l <- 1
    }

    
    cat("\014")
    
    z <- x[l:(l + n - 1)]
    cat(z, sep = "\n")
    cat("\nline ", l, " to ", l + n - 1, " (of ", length(x), ")")
    
    if (status == 1){
      cat("\nnext [enter]  previous [p]  to line [number]  quit [q]",sep = "")
      
      inp <- readline()
      if (inp == ""){
        l <- l + n
      } else if (inp == "p"){
        l <- l - n
      } else if (grepl("\\d+", inp)){
        l <- as.numeric(inp)
      } else if (inp == "q"){
        status <- 0
      }
    }

  }
}


