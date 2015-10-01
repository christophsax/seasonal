#' Display X-13ARIMA-SEATS Output
#' 
#' The \code{out} function shows the full content of the X-13ARIMA-SEATS output.
#' If you are using the HTML version of X-13 (recommended), the output is 
#' displayed in the browser. If you are using the non-HTML version, the output 
#' is shown in the console.
#' 
#' To keep the size of \code{"seas"} objects small, \code{seas} does not save 
#' the output by default. Instead, \code{out} re-evaluates the model.
#' 
#' @param x an object of class \code{"seas"}.
#' @param browser browser to be used, passed on to 
#'   \code{\link{browseURL}} (ignored in the non-HTML version).
#' @param line  starting line of the content (ignored in the HTML version).
#' @param n  number of lines to show on a page (ignored in the HTML version).
#' @param search   regular expression chracter string. If specified, the content
#'   is searched for the first occurence (ignored in the HTML version).
#' @param ... additional spec-arguments options sent to X-13ARIMA-SEATS during 
#'   re-evaluation. See \code{\link{seas}}.
#'   
#' @return displays the output as a side effect.
#'   
#' @seealso \code{\link{seas}} for the main function of seasonal.
#'   
#' @references Vignette with a more detailed description: 
#'   \url{http://www.seasonal.website/seasonal.html}
#'   
#'   Comprehensive list of R examples from the X-13ARIMA-SEATS manual: 
#'   \url{http://www.seasonal.website/examples.html}
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
#' out(m) 
#' out(m, automdl.print = "autochoicemdl")
#' 
#' arguments ignored in the HTML version
#' out(m, search = "regARIMA model residuals")
#' out(m, search = "Normality Statistics for regARIMA")
#' }
out <- function(x, browser = getOption("browser"), line = 1, n = 100, 
                search = NULL, ...){
  if (getOption("htmlmode") == 0){
    return(outTxt(x, line = line, n = n, search = search, ldots = list(...)))
  }
  # clean remainings from previous out runs
  unlink(list.files(tempdir(), pattern = "^x13out", full.names = TRUE), recursive = TRUE)
  
  m <- reeval(x, ldots = list(...), out = TRUE)
  browseURL(url = file.path(m$wdir, "iofile.html"), browser = browser)
}


outTxt <- function(m, line, n, search, ldots){
  if (is.null(m$out)){
    x <- reeval(m, ldots = ldots, out = TRUE)$out
  } else {
    x <- m$out
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



