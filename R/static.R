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
static <- function(x, static.coeff = FALSE, name = NULL, test = TRUE){
  
  stopifnot(inherits(x, "seas"))
  
  if (is.null(name)){
    name <- x$call[[2]]
  }
  
  if (!is.null(x$spc$force)) {
    opt.force <- paste0(", force.type = ", x$spc$force$type)
  } else {
    opt.force <- ""
  }
  
  if (!is.null(x$spc$x11)) {
    opt.x11 <- paste0(", x11 = list()")
  } else {
    opt.x11 <- ""
  }
  
  if (static.coeff){
    if (!is.null(x$mdl$regression$b)) {
      opt.b <- paste(", regression.b =", deparse(SubFixed(x$mdl$regression$b)))
    } else {
      opt.b <- ""
    }
    if (!is.null(x$mdl$arima$ma)) {
      opt.ma <- paste(", arima.ma =", deparse(SubFixed(x$mdl$arima$ma)))
    } else {
      opt.ma <- ""
    }
    
    # ar
    if (!is.null(x$mdl$arima$ar)) {
      opt.ar <- paste(", arima.ar =", deparse(SubFixed(x$mdl$arima$ar)))
    } else {
      opt.ar <- ""
    }
    
    opt.coeff <- paste0(opt.b, opt.ar, opt.ma, ", transform = NULL")
  } else {
    opt.coeff <- ""
  }
  
  z <- paste0("seas(", name, 
              ", regression.variables = ", deparse(x$mdl$regression$variables,
                                                   width.cutoff = 500), 
              ", arima.model = ", deparse(x$mdl$arima$model, 
                                          width.cutoff = 500),
              opt.coeff, opt.force, opt.x11,
              ", regression.aictest = NULL, outlier.types = \"none\")"
  )

  if (test){
    # testing the static call
    x.static <- eval(parse(text = z))
    test <- (all.equal(final(x.static), final(x)))
    if (inherits(test, "character")){
      warning(paste("Final Series of static and provided model differ.", test))
    }
  }

  class(z) <- "static"
  z
}


print.static <- function(x){
  cat(x)
}

# x <- c("2342f", "324234")
# SubFixed(x)
SubFixed <- function(x){
  z <- paste0(x, "f")
  str_replace_all(z, "f+", "f")
}
