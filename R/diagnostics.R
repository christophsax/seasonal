#' \code{.spc} File Content
#' 
#' Access the content of the \code{.spc} file that governs the behavior of 
#'   X-13ARIMA-SEATS.
#' 
#' @param x  object of class \code{"seas"}
#'   
#' @return returns an object of class \code{"spclist"}, essentially a list that 
#' contains the information that is sent to X-13ARIMA-SEATS. The corresponding 
#' \code{print} method displays the content of the list as written to the 
#' \code{.spc} file.
#' 
#' @seealso \code{\link{seas}} for the main function.
#' @seealso \code{\link{series}}, for universal X-13 output extraction.
#' @seealso \code{\link{plot.seas}}, for diagnostical plots.
#' @seealso \code{\link{out}}, for accessing the full output of X-13ARIMA-SEATS.
#' 
#' @references Vignette with a more detailed description: 
#'   \url{http://www.seasonal.website/seasonal.html}
#'   
#'   Comprehensive list of R examples from the X-13ARIMA-SEATS manual: 
#'   \url{http://www.seasonal.website/examples.html}
#'   
#'   Official X-13ARIMA-SEATS manual: 
#'   \url{https://www.census.gov/ts/x13as/docX13ASHTML.pdf}
#'   
#' @examples
#' \dontrun{
#' 
#' m <- seas(AirPassengers)
#' spc(m)
#' }
#' @export
spc <- function(x){
  x$spc
}




#' Five Best ARIMA Models
#'
#' Returns the five best models as chosen by the BIC criterion. It needs the
#' \code{automdl} spec to be activated (default). If it is not activated, the
#' function tries to re-evaluate the model with the \code{automdl} spec
#' activated.
#' 
#' @param x  object of class \code{"seas"}
#'   
#' @seealso \code{\link{seas}} for the main function.
#' @seealso \code{\link{series}}, for universal X-13 output extraction.
#' @seealso \code{\link{plot.seas}}, for diagnostical plots.
#' @seealso \code{\link{out}}, for accessing the full output of X-13ARIMA-SEATS.
#' 
#' @references Vignette with a more detailed description: 
#'   \url{http://www.seasonal.website/seasonal.html}
#'   
#'   Comprehensive list of R examples from the X-13ARIMA-SEATS manual: 
#'   \url{http://www.seasonal.website/examples.html}
#'   
#'   Official X-13ARIMA-SEATS manual: 
#'   \url{https://www.census.gov/ts/x13as/docX13ASHTML.pdf}
#'   
#' @examples
#' \dontrun{
#' 
#' m <- seas(AirPassengers)
#' fivebestmdl(m)
#' }
#' @export
fivebestmdl <- function(x){
  if (!is.null(x$fivebestmdl)){
    if (getOption("htmlmode") == 1){
      txt <- x$fivebestmdl[4:8]

      arima.st <- regexpr("Model \\#  \\d : ", txt) + 13
      arima.en <- regexpr(" \\(<abbr", txt) - 1
      arima <- substr(txt, start = arima.st, stop = arima.en)
      
      bic.st <- regexpr("</abbr> = ", txt) + 10
      bic.en <- regexpr("\\) <br> ", txt) - 1
      bic <- as.numeric(substr(txt, start = bic.st, stop = bic.en))
      z <- data.frame(arima, bic, stringsAsFactors = FALSE)
    } else {
      txt <- x$fivebestmdl[3:7]
      arima <- substr(txt, start = 19, stop = 32)
      arima <- gsub(" *$", "", arima) 
      bic <- as.numeric(substr(txt, start = 51, stop = 56))
      z <- data.frame(arima, bic, stringsAsFactors = FALSE)
    }
  } else if (is.null(x$reeval)) {
    # if no fivebestmdl, try reevaluating with automdl
    lc <- as.list(x$call)
    lc$automdl <- list()
    lc$arima.model <- NULL
    rx <- eval(as.call(lc), envir = globalenv())
    rx$reeval <- TRUE  # avoid infinite looping
    z <- fivebestmdl(rx)
  } else {
    z <- NULL
  }
  z
}



#' Applied Transformation
#' 
#' Returns the transform function that has been applied.
#' 
#' @param x  object of class \code{"seas"}
#'   
#' @seealso \code{\link{seas}} for the main function.
#' @seealso \code{\link{series}}, for universal X-13 output extraction.
#' @seealso \code{\link{plot.seas}}, for diagnostical plots.
#' @seealso \code{\link{out}}, for accessing the full output of X-13ARIMA-SEATS.
#' 
#' @references Vignette with a more detailed description: 
#'   \url{http://www.seasonal.website/seasonal.html}
#'   
#'   Comprehensive list of R examples from the X-13ARIMA-SEATS manual: 
#'   \url{http://www.seasonal.website/examples.html}
#'   
#'   Official X-13ARIMA-SEATS manual: 
#'   \url{https://www.census.gov/ts/x13as/docX13ASHTML.pdf}
#'   
#' @examples
#' \dontrun{
#' 
#' m <- seas(AirPassengers)
#' transformfunction(m)
#' }
#' @export
transformfunction <- function(x){
  stopifnot(inherits(x, "seas"))
  if (is.null(x$spc$transform$`function`)){
    stop("no transform function, investigate!")
  }
  
  if (x$spc$transform$`function` == "auto"){
    if (grepl("Log", udg(x, 'aictrans'))){
      z <- "log"
    } else {
      z <- "none"
    }
  } else {
    z <- x$spc$transform$`function`
  }
  z
}


