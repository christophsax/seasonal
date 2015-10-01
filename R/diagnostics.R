#' Diagnostical Statistics
#' 
#' Functions to access some specific diagnostical statistics in a \code{"seas"} object.  
#' For universal import of X-13ARIMA-SEATS tables, use the \code{\link{series}} function. For 
#' accessing the \code{.out} file of X-13ARIMA-SEATS, use the \code{\link{out}} 
#' function. For diagnostical plots, see \code{\link{plot.seas}}.
#' 
#' @param x  object of class \code{"seas"}
#'   
#' @return  \code{qs} returns the QS statistics for seasonality of input and
#'   output series and the corresponding p-values.
#'   
#' @return \code{spc} returns the content of the \code{.spc} file, i.e. the 
#'   specification as it is sent to X-13ARIMA-SEATS. Analyzing the \code{spc} 
#'   output is useful for debugging.
#'   
#' @return \code{fivebestmdl} returns the five best models as chosen by the BIC 
#'   criterion. It needs the \code{automdl} spec to be activated (default). If it is not 
#'   activated, the function tries to re-evaluate the model with 
#'   the \code{automdl} spec activated.
#'   
#' @return \code{arimamodel} returns the structure of a the ARIMA model, a 
#'   numerical vector of the form \code{(p d q)(P D Q)}, containing the 
#'   non-seasonal and seasonal part of the ARIMA model.
#'   
#' @return \code{transformfunction} returns the transform function that has been applied.
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
#'   \url{http://www.census.gov/ts/x13as/docX13AS.pdf}
#'   
#' @examples
#' \dontrun{
#' 
#' m <- seas(AirPassengers)
#' 
#' qs(m)
#' spc(m)
#' fivebestmdl(m)
#' arimamodel(m)
#' transformfunction(m)
#' 
#' # if no automdl spec is present, the model is re-evaluated
#' m2 <- seas(AirPassengers, arima.model = "(0 1 1)(0 1 1)")
#' spc(m2)           # arima overwrites the automdl spec
#' fivebestmdl(m2)   # re-evaluation with automdl
#' 
#' # universal output extraction (see ?series)
#' series(m, "identify.pacf")
#' 
#' # accessing the .out file (see ?out)
#' out(m)
#' }
#' @export
qs <- function(x){
  qs.var <- c("qsori", "qsorievadj", "qsrsd", "qssadj", "qssadjevadj", "qsirr",  "qsirrevadj", "qssori", "qssorievadj", "qssrsd", "qsssadj", "qsssadjevadj",  "qssirr", "qssirrevadj")
  z0 <- x$udg[names(x$udg) %in% qs.var]
  z <- read.table(text = z0, colClasses = "numeric")
  rownames(z) <- names(z0)
  colnames(z) <- c("qs", "p-val")
  z
}


#' @rdname qs
#' @export
spc <- function(x){
  x$spc
}


#' @rdname qs
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



#' @rdname qs
#' @export
arimamodel <- function(x){
  stopifnot(inherits(x, "seas"))
  str <- x$model$arima$model
  str <- gsub("[ \\(\\)]", "", str)
  z <- c(substr(str, 1, 1),
         substr(str, 2, 2),
         substr(str, 3, 3),
         substr(str, 4, 4),
         substr(str, 5, 5),
         substr(str, 6, 6)
  )
  as.numeric(z)
}



#' @rdname qs
#' @export
transformfunction <- function(x){
  stopifnot(inherits(x, "seas"))
  if (is.null(x$spc$transform$`function`)){
    stop("no transform function, investigate!")
  }
  
  if (x$spc$transform$`function` == "auto"){
    if (grepl("Log", x$udg['aictrans'])){
      z <- "log"
    } else {
      z <- "none"
    }
  } else {
    z <- x$spc$transform$`function`
  }
  z
}

