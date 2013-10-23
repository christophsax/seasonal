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
#' 
static <- function(x, name = NULL){
  stopifnot(inherits(x, "seas"))
  
  if (is.null(name)){
    name <- x$call[[2]]
  }

  if (!is.null(x$spc$force)) {
    opt.force <- paste0('  force.type = "', x$spc$force$type, '",\n')
  } else {
    opt.force <- ""
  }
  
  z <- paste0('seas(', name, ',\n',
              '  regression.variables = c("', 
              paste(x$mdl$regression$variables, collapse = '", "'), '"),\n',
              '  arima.model = "', x$mdl$arima$model, '",\n',
              opt.force,
              '  regression.aictest = NULL, outlier.types = "none"\n)'
  )
  cat(z)
}
