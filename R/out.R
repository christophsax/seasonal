#' Import X-13ARIMA-SEATS Text Output
#' 
#' The \code{out} function shows the full content of the \code{.out}, the 
#' \code{.log} or the \code{.err} file form X-13ARIMA-SEATS.
#' 
#' To keep the size of \code{"seas"} objects small, "seas" does not save 
#' the content of the \code{.out} by default. Instead, the \code{out} 
#' function re-evaluates the model.
#' 
#' @param x an object of class \code{"seas"}.
#' @param line  starting line of the content.
#' @param n  number of lines to show on a page.
#' @param search   regular expression chracter string. If specified, the content
#'   is searched for the first occurence (see examples).
#' @param file \code{"out"} or \code{"log"}, which file to show.
#'   
#' @return an object of class \code{"out"}, essentially a character vector with 
#'   attributes. The print method for \code{"out"} objects is adapted to the 
#'   large size of the \code{.out} output. It allows for pagination and search
#'   (see examples).
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
#' out(m, search = "Normality Statistics for regARIMA")
#' 
#' m <- seas(AirPassengers, slidingspans = list()) 
#' out(m, search = "Sliding spans analysis", n = 300)
#' 
#' # showing the log file
#' out(m, file = "log")
#' 
#' # showing the error file
#' out(m, file = "err")
#' }
#' 
out <- function(x, line = 1, n = 100, search = NULL, file = c("out", "log", "err")){
  file <- match.arg(file)
  if (file == "out"){
    if (is.null(x$out)){
      z <- reeval(x, ldots = list(), out = TRUE)$out
    } else {
      z <- x$out
    }
  } else if (file == "log"){
    z <- x$log
  } else {
    z <- x$err
  }
  # attributes for the print method
  attr(z, "line") <- line
  attr(z, "n") <- n
  attr(z, "search") <- search
  class(z) <- c("out", "character")
  z
}


#' @export
#' @method print out
print.out <- function(x, ...){
  if (!is.null(attr(x, "line"))){
    line <- attr(x, "line")
  } else {
    line <- 1
  }
  if (!is.null(attr(x, "n"))){
    n <- attr(x, "n")
  } else {
    n <- 100
  }
  if (!is.null(attr(x, "search"))){
    search <- attr(x, "search")
  } else {
    search <- NULL
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



