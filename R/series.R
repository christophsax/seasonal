#' Universal import of X-13 data series 
#' 
#' @param x  an object of class \code{"seas"}.
#' @param series  short name of an X-13 data table. 
#'  
#' @export
#' 
#' @examples
#' 
#' \dontrun{

#' 
#' }
#' 
series <- function(x, series, reeval = TRUE){
  stopifnot(inherits(x, "seas"))
  data(specs)
  
  is.valid <- series %in% SPECS$short[SPECS$is.series]
  
  if (any(!is.valid)){
    stop(paste("series not allowed:", paste(series[!is.valid], collapse = ", ")))
  }
  
  # reeval with non present output
  if (reeval){
    # check which series are already there
    if (is.null(x$series)){
      series.NA <- series
    } else {
      series.NA <- setdiff(series, names(x$series))
    }
    
    reeval.dots <- list()
    for (i in seq_along(series.NA)){
      series.NA.i <- series.NA[i]
      
      spec.i <- as.character(SPECS[SPECS$short == series.NA.i, ]$spec)
      
      if (!spec.i %in% names(x$spc)){
        warning(paste0("\'", spec.i, "\' spec activated"))
      }
      
      reeval.dots[[i]] <- series.NA.i
      names(reeval.dots)[i] <- paste0(spec.i, '.save')
    }
    x <- reeval(x, reeval.dots)
  }


  do.call(cbind, x$series[series])
}


