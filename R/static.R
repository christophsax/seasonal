#' Static Call of a seas Object
#' 
#' A static call is a static replication of a call. Automatic procedures are 
#' substituted by the automatically chosen spec-argument options. The call can
#' be copy/pasted to a script and used for further manipulations or future 
#' evaluation of the same model.
#' 
#' By default, the static call is tested. It is executed and compared to the 
#' input call. If the final series is not identical, a message is returned.
#' 
#' If \code{coef = TRUE}, the coefficients are fixed as well.
#' 
#' @param x an object of class \code{seas}.
#' @param coef  logical, if \code{TRUE}, the coefficients are treated as fixed, 
#'   instead of beeing estimated.
#' @param test logical. By default the static call is executed and compared to 
#'   the input call. If the final series is not identical, a warning is 
#'   returned. If \code{FALSE}, the option is disabled.
#' @param verbose logical, if \code{TRUE}, droped and kept series are listed.
#' @param fail logical, if \code{TRUE}, differences will cause an error. Ignored 
#'   if \code{test = FALSE}.
#' 
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
#'   \url{http://www.census.gov/ts/x13as/docX13AS.pdf}
#'   
#' @export
#' @examples
#' \dontrun{
#' 
#' m <- seas(AirPassengers)
#' static(m)
#' static(m, test = FALSE)
#' }
static <- function(x, coef = FALSE, test = TRUE, verbose = FALSE, fail = FALSE){
  
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
  
  if (verbose){
    cat("Droped:", paste(names(as.list(x$call)[!(names(as.list(x$call)) %in% names(lc))]), collapse=", "), "\n")
    cat("Kept:", paste(names(as.list(x$call)[(names(as.list(x$call)) %in% names(lc))]), collapse=", "), "\n")
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
