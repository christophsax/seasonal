#' Static Call of a seas Object
#' 
#' A static call is a static replication of a call. Automatic procedures are
#' subistuted by the automatically selected spec/argument options.
#' 
#' @param x an object of class \code{seas}
#' @param coef  logical, if \code{TRUE}, the coefficients are treated as fixed,
#'   instead of beeing estimated.
#' @param name character string, optionally specify the name of the input time 
#'   series
#' @param test logical. By default the static call is executed and compared to
#'   the input call. If the final series is not identical, an error is returned.
#'   If \code{FALSE}, the option is disabled (useful for debugging)
#'   
#' @return Static call of an object of class \code{seas}. Can be copy/pasted 
#'   into an R script.
#' @export
#' @examples
#' \dontrun{
#' x <- seas(AirPassengers)
#' static(x)
#' static(x, name = "ArbitrayName", test = FALSE)
#' }
static <- function(x, coef = FALSE, name = NULL, test = TRUE){
  
  stopifnot(inherits(x, "seas"))
  
  lc <- as.list(x$call)  
  
  # keep all arguments that do not interfer with input/output or the automatic
  # options of seasonl
  keep <- c("", "x", "xreg",
            "estimate.exact", "estimate.maxiter",
            
            "force.lambda", "force.mode", "force.rho", "force.round", 
            "force.start", "force.target", "force.type", "force.usefcst", 
            "force.indforce",
            
            "forecast.exclude", "forecast.lognormal", "forecast.maxback",
            "forecast.maxlead", "forecast.probability",
            
            "regression.chi2test", "regression.chi2testcv", 
            "regression.variables",
            
            "seats.hpcycle", "seats.qmax", "seats.signifsc", "seats.statseas",
            "seats.bias", "seats.centerir", "seats.epsiv", "seats.epsphi", 
            "seats.epsphi", "seats.maxbias", "seats.maxit", "seats.noadmiss",
            "seats.rmod", "seats.xl",
            
            "series.type",
            
            "transform.adjust",
            
            "x11", "x11.mode", "x11.trendma", "x11.sigmalim", "x11.appendfcst", 
            "x11.appendbcst", "x11.final", "x11.type",
            
            "x11regression.variables", "x11regression.tdprior", 
            "x11regression.usertype"
  )
  
  lc <- lc[names(lc) %in% keep]
  
  if (!is.null(name)){
    lc$x = parse(text = name)[[1]]
  }
  
  lc$regression.variables <- x$model$regression$variables
  lc$arima.model <- x$model$arima$model
  
  # Turn off outomatic procedures:
  # To assign NULL instead of removing the element
  lc['regression.aictest'] <- NULL
  names(lc['regression.aictest']) <- "regression.aictest"
  
  lc['outlier'] <- NULL
  names(lc['outlier']) <- "regression.aictest"
  
  lc$transform.function = x$transform.function
  
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

  cat(deparse(z), sep = "\n")
  
  if (test){
    # testing the static call
    x.static <- eval(z, envir = globalenv())
    test <- (all.equal(final(x.static), final(x), tolerance = 1e-05))
    if (inherits(test, "character")){
      warning(paste("Final Series of static and provided model differ.", test))
    }
  }

  invisible(z)
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
