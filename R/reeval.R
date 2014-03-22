#' Diagnostical Re-Evaluation
#' 
#' For diagnostical purposes, these functions re-evaluate an object of class 
#' \code{"seas"} and capture the full content or parts of the .out file from 
#' X-13ARIMA-SEATS.
#' 
#' The \code{out} function shows the full content of the \code{.out} file form 
#' X-13ARIMA-SEATS. The \code{slidingspans} and \code{revisions} function call 
#' the 'slidingspans' and 'history' spec of X-13ARIMA-SEATS and show the 
#' respective parts of the \code{.out} file. Note that against the convention, 
#' the 'history' spec is called by the function \code{revision}, in order to 
#' avoid a naming collision with the function from the preloaded \code{utils} 
#' package. For a description of the 'slidingsspans' and 'history' spec,
#' consider the X-13ARIMA-SEATS manual.
#' 
#' For how to enter spec-arguments options, see the details in 
#' \code{"\link{seas}"}. In the \code{out} function, \code{...} are useful to 
#' add additional outputs to the \code{.out} file. In the \code{slidingspans} 
#' and \code{revisions} function, \code{...} are used to add additional options 
#' to the 'slidingspans' and 'history' spec.
#' 
#' @param x an object of class \code{"seas"} to re-evaluate.
#' @param line  starting line of the content.
#' @param n  number of lines to show at once.
#' @param search   chracter string. If specified, the content is searched for 
#'   the first occurence of the string (see examples).
#' @param ... aditional spec-arguments options (see details).
#'   
#' @return an object of class \code{"out"}, \code{"revisions"} or
#'   \code{"slidingspans"}, which is basically a \code{"seas"} object that
#'   contatins the \code{out} element from the X-13ARIMA-SEATS \code{.out} file.
#'   \code{"revisions"} and \code{"slidingspans"} objects also contain additional spec specific time series
#'   time series that can be plotted with the \code{plot} method.
#'   
#'   If printed, the objects show the content of the \code{.out} file.
#'   
#'   There are \code{plot} methods for \code{"revisions"} and
#'   \code{"slidingspans"} that visualize the spec.
#'   
#' @seealso \code{\link{seas}} for the main function of seasonal.
#'   
#' @seealso \code{\link{plot.seas}}, for plot methods for \code{slidingspans} 
#'   and \code{revisions}.
#'   
#' @references Vignette with a more detailed description: 
#'   \url{http://cran.r-project.org/web/packages/seasonal/vignettes/seas.pdf}
#'   
#'   Wiki page with a comprehensive list of R examples from the X-13ARIMA-SEATS 
#'   manual: 
#'   \url{https://github.com/christophsax/seasonal/wiki/Examples-of-X-13ARIMA-SEATS-in-R}
#'   
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
#' # passing options
#' revisions(m, history.start = "1959.Aug")
#' slidingspans(m, slidingspans.fixreg = "td")
#' 
#' # plot method for revisions and slidingspans (see ?plot.seas)
#' rr <- revisions(m)
#' plot(rr)
#' plot(rr, series = "trendestimates")
#' 
#' ss <- slidingspans(m)
#' plot(ss)
#' plot(ss, series = "chngspans")
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





reeval <- function(x, ldots, out = TRUE){
  lc <- as.list(x$call)
  lc <- c(lc, ldots)
  if (out){
    lc$out <- TRUE
  }

  z <- eval(as.call(lc), envir = globalenv())
  z
}


#' @export
#' @method print out
print.out <- function(x, line = 1, n = 600, search = NULL, ...){
  
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


