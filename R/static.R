#' Static Call of a seas Object
#' 
#' In a static call, automatic procedures are substituted by the spec-argument
#' options that have been selcted by the automatic procedures.
#' 
#' The call can be copy/pasted to a script and used for further manipulations or
#' future evaluation of the same model.
#' 
#' By default, the static call is tested. It is executed and compared to the 
#' input call. If the final series is not identical, a message is returned.
#' 
#' If \code{coef = TRUE}, the coefficients are fixed as well. If 
#' \code{x11.filter = TRUE}, the X-11 moving averages are fixed as well.
#' 
#' @param x an object of class \code{seas}.
#' @param coef  logical. If \code{TRUE}, the coefficients are treated as fixed, 
#'   instead of beeing estimated.
#' @param x11.filter logical. X-11 only. if \code{TRUE}, the X-11 moving 
#'   averages will be fixed as well. This leads to different filters at
#'   different stages, and the resulting series can be are slightly different.
#'   If \code{test = TRUE}, this may cause a warning  message.
#' @param test logical. By default the static call is executed and compared to 
#'   the input call. If the final series is not identical, a message is 
#'   returned. If \code{FALSE}, no test is performed (faster).
#' @param fail logical, if \code{TRUE}, differences will cause an error. Ignored 
#'   if \code{test = FALSE}.
#' @param eval logical, should be call be evaluated.
#' @return Object of class \code{"call"}. Static call of an object of class
#'   \code{seas}. Can be copy/pasted into an R script.
#'   
#' @seealso \code{\link{seas}} for the main function of seasonal.
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
#' @export
#' @examples
#' \dontrun{
#' 
#' m <- seas(AirPassengers)
#' static(m)
#' static(m, test = FALSE)  # much faster
#' static(m, eval = TRUE)   # returns an object of class "seas"
#' 
#' m <- seas(AirPassengers, x11 = "")
#' static(m, x11.filter = TRUE)
#' }
static <- function(x, coef = FALSE, x11.filter = FALSE, test = TRUE, 
                   fail = FALSE, eval = FALSE){

  if (!inherits(x, "seas")){
    stop("first argument must be of class 'seas'")
  }
  
  lc <- as.list(x$call)  

  if ("list" %in% names(lc)){
    stop("static does not work with the 'list' argument in seas")
  }

  lc$regression.variables <- x$model$regression$variables
  lc$arima.model <- x$model$arima$model

  # Turn off outomatic procedures:

  # remove all arguments to the auto specs
  lc <- lc[!grepl("^automdl", names(lc))]
  lc <- lc[!grepl("^outlier", names(lc))]
  lc <- lc[!grepl("^pickmdl", names(lc))]

  # To assign NULL instead of removing the element, do this trick
  lc['regression.aictest'] <- NULL
  names(lc['regression.aictest']) <- "regression.aictest"
  
  lc['outlier'] <- NULL
  names(lc['outlier']) <- "outlier"

  lc$transform.function = transformfunction(x)

  # substitute X-11 filters
  if (x11.filter){
    if (!is.null(x$spc$x11)) {
      if (is.null(lc$x11.trendma) || lc$x11.trendma == ""){
        tma <- udg(x, "finaltrendma", fail = FALSE)
        lc$x11.trendma <- as.numeric(unname(tma))
      }
      if (is.null(lc$x11.seasonalma) || lc$x11.seasonalma == ""){
        sma <- udg(x, "sfmsr", fail = FALSE)
        if (!is.null(sma)) sma <- paste0("s", sma)
        lc$x11.seasonalma <- unname(sma)
      }
    }
  }

  if (coef){
    if (!is.null(x$model$regression$b)) {
      lc$regression.b = c(add_f(x$model$regression$b))
    }
    if (!is.null(x$model$arima$ma)) {
      lc$arima.ma = add_f(x$model$arima$ma)
    } 
    if (!is.null(x$model$arima$ar)) {
      lc$arima.ar = add_f(x$model$arima$ar)
    }
  }

  z <- as.call(lc)
  
  if (test){
    # testing the static call
    x.static <- eval(z, envir = globalenv())
    test <- (all.equal(log(final(x.static)), log(final(x)), tolerance = 1e-05))
    if (!isTRUE(test)){
      (if (fail) stop else message)(paste("Static series is different.", test))
    }
  }

  if (eval){
    return(eval(z, envir = sys.frame(-1)))
  }

  z
}



add_f <- function(x){
  # Make coefficents 'fixed'
  #
  # Put a "f" at the end of number, if not already there
  #
  # x <- c("2342f", "324234")
  # SubFixed(x)
  
  z <- paste0(x, "f")
  gsub("f+", "f", z)
}
