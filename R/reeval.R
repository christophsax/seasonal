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
#' the \code{utils} pacakge. For a description of the \code{slidingsspan} and 
#' \code{history} spec, consider the X-13ARIMA-SEATS manual.
#' 
#' For how to enter spec-arguments options, see the details in 
#' \code{"\link{seas}"}. In the \code{out} function, \code{...} are useful to 
#' add adtional outputs to the \code{.out} file. In the \code{slidingspans} and 
#' \code{revisions} function, \code{...} are used to add aditional options to 
#' the \code{slidingspans} and \code{history} spec.
#' 
#' In the current version of X-13ARIMA-SEATS, the \code{history} spec delivers 
#' some implausible results. It should be used carefully.
#' 
#' @param x an object of class \code{"seas"} to re-evaluate.
#' @param line  starting line of the content.
#' @param n  number of lines to show at once.
#' @param search   chracter string. If \code{view = TRUE}, the \code{.out}
#'   content is searched for the first occurence of the string (see examples).
#' @param ... aditional spec-arguments options (see details).
#'   
#' @return invisible, the full content or parts of the .out file as a (large) 
#'   character vector. If \code{viewer = TRUE}, the content of the character 
#'   vector is shown as a side effect.
#'   
#' @seealso \code{\link{seas}} for the main function of seasonal.
#' 
#' @references Vignette with a more detailed description: 
#'   \url{http://cran.r-project.org/web/packages/seasonal/vignettes/seas.pdf}
#'   
#'   Wiki page with a comprehensive list of R examples from the X-13ARIMA-SEATS 
#'   manual: 
#'   \url{https://github.com/christophsax/seasonal/wiki/Examples-of-X-13ARIMA-SEATS-in-R}
#'   
#'   Official X-13ARIMA-SEATS manual: 
#'   \url{http://www.census.gov/ts/x13as/docX13AS.pdf}
#'   
#' @export
#' 
#' @examples
#' 
#' \dontrun{
#' m <- seas(AirPassengers) 
#' 
#' # exit from the viewer with [q]
#' out(m)  
#' out(m, search = "regARIMA model residuals")
#' 
#' slidingspans(m)
#' revisions(m)
#' 
#' # plot method for slingspans and revisions (see ?plot.seas)
#' plot(slidingspans(m))
#' plot(revisions(m))
#' 
#' }
#' 
out <- function(x, line = 1, n = 100, search = NULL, ...){
  ldots <- list(...)
  z <- reeval(x, ldots)$out
  
  # print attributes
  attr(z, "line") <- line
  attr(z, "n") <- n
  attr(z, "search") <- search
  
  z
}





reeval <- function(x, ldots){
  lc <- as.list(x$call)
  lc <- c(lc, ldots)
  lc$out <- TRUE

  z <- eval(as.call(lc), envir = globalenv())
  z
}


#' @export
#' @method print out
print.out <- function(x, line = 1, n = 500, search = NULL, ...){
  
  # use print parameters if present
  if (!is.null(attr(x, "line"))){
    line <- attr(x, "line")
  }
  if (!is.null(attr(x, "n"))){
    n <- attr(x, "n")
  }
  if (!is.null(attr(x, "search"))){
    search <- attr(x, "search")
  }
  
  if (length(x) < n){
    n <- length(x)
  }

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
    cat("\nX-13-as output: line ", l, " to ", l + n - 1, " (of ", length(x), ")", sep = "")
    
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


