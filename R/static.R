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
static <- function(x, static.coeff = FALSE, name = NULL){
  stopifnot(inherits(x, "seas"))
  
  if (is.null(name)){
    name <- x$call[[2]]
  }

  if (!is.null(x$spc$force)) {
    opt.force <- paste0('  force.type = "', x$spc$force$type, '",\n')
  } else {
    opt.force <- ""
  }
  
  if (static.coeff){
    
    opt.b <- paste0('  regression.b = c("', 
              paste(SubFixed(x$mdl$regression$b), collapse = '", "'), '"),\n'
                        )
    
    # ma
    if (!is.null(x$mdl$arima$ma)) {
      opt.ma <- paste0('  arima.ma = c("', 
                    paste(SubFixed(x$mdl$arima$ma), collapse = '", "'), '"),\n'
      )
    } else {
      opt.ma <- ""
    }
    
    # ar
    if (!is.null(x$mdl$arima$ar)) {
      opt.ar <- paste0('  arima.ar = c("', 
                       paste(SubFixed(x$mdl$arima$ar), collapse = '", "'), '"),\n'
      )
    } else {
      opt.ar <- ""
    }
    
    opt.coeff <- paste0(opt.b, opt.ar, opt.ma, '  transform = NULL,\n')
  } else {
    opt.coeff <- ""
  }
  
  z <- paste0('seas(', name, ',\n',
              '  regression.variables = c("', 
              paste(x$mdl$regression$variables, collapse = '", "'), '"),\n',
              '  arima.model = "', x$mdl$arima$model, '",\n',
              opt.coeff,
              opt.force,
              '  regression.aictest = NULL, outlier.types = "none"\n)'
  )
  cat(z)
}

# x <- c("2342f", "324234")
# SubFixed(x)
SubFixed <- function(x){
  z <- paste0(x, "f")
  str_replace_all(z, "f+", "f")
}



