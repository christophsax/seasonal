#' Static Call of a seas Object
#' 
#' @param x an object of class \code{seas}
#' @param name character string, optionally specify the name of the input time
#'   series
#'   
#' @return Static call of an object of class \code{seas}. Can be copy/pasted 
#'   into an R script.
#' @export
#' @examples
#' x <- seas(AirPassengers)
#' static(x)
#' static(x, name = "ArbitrayName")
static <- function(x, coef = FALSE, name = NULL, test = TRUE){
  
  stopifnot(inherits(x, "seas"))
  
  lc <- as.list(x$call)  
  
  # keep arguments if they are in this vector
  keep <- c("", "x", "xreg",
            
            "force.type", 
            
            "x11", "x11.mode", "x11.trendma", "x11.sigmalim", "x11.appendfcst", 
            "x11.appendbcst", "x11.final"
  )
  
  lc <- lc[names(lc) %in% keep]
  
  if (!is.null(name)){
    lc$x = parse(text = name)[[1]]
  }
  
  lc$regression.variables <- x$mdl$regression$variables
  lc$arima.model <- x$mdl$arima$model
  lc$regression.chi2test <- "no"
  lc$outlier.types <- "none"
  
  lc$transform.function = "log"
  
  if (coef){
    if (!is.null(x$mdl$regression$b)) {
      lc$regression.b = c(SubFixed(x$mdl$regression$b))
    }
    if (!is.null(x$mdl$arima$ma)) {
      lc$arima.ma = SubFixed(x$mdl$arima$ma)
    } 
    if (!is.null(x$mdl$arima$ar)) {
      lc$arima.ar = SubFixed(x$mdl$arima$ar)
    }
  }
  

  
  z <- as.call(lc)

  if (test){
    # testing the static call
    x.static <- eval(z)
    test <- (all.equal(final(x.static), final(x), tolerance = 1e-06))
    if (inherits(test, "character")){
      warning(paste("Final Series of static and provided model differ.", test))
    }
  }

  cat(deparse(z), sep = "\n")
  invisible(z)
}


# x <- c("2342f", "324234")
# SubFixed(x)
SubFixed <- function(x){
  z <- paste0(x, "f")
  str_replace_all(z, "f+", "f")
}
