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
  
  is.dotted <- grepl("\\.", series)
  
  # check validiy of short or long names
  is.valid <- logical(length = length(series))
  is.valid[is.dotted] <- series[is.dotted] %in% SPECS$long[SPECS$is.series]
  is.valid[!is.dotted] <- series[!is.dotted] %in% SPECS$short[SPECS$is.series]
  
  if (any(!is.valid)){
    stop(paste0("\nseries not valid: ", paste(series[!is.valid], collapse = ", "), ".\nsee ?series for all importable series. "))
  }
  
  # unique short names
  series.short <- unique(c(series[!is.dotted], 
    merge(data.frame(long = series[is.dotted]), SPECS)$short))

  # reeval with non present output
  if (reeval){
    # check which series are already there
    if (is.null(x$series)){
      series.NA <- series.short
    } else {
      series.NA <- setdiff(series.short, names(x$series))
    }
    
    activated <- NULL
    reeval.dots <- list()
    for (i in seq_along(series.NA)){
      series.NA.i <- series.NA[i]
      
      spec.i <- as.character(SPECS[SPECS$short == series.NA.i, ]$spec)
      
      if (!spec.i %in% names(x$spc)){
        activated <- c(activated, spec.i)
      }
      
      reeval.dots[[i]] <- series.NA.i
      names(reeval.dots)[i] <- paste0(spec.i, '.save')
    }
    x <- reeval(x, reeval.dots)
  }

  if (length(activated) > 0){
    message(paste("specs have been added to the model:", 
                  paste(unique(activated), collapse = ", ")))
  }
  do.call(cbind, x$series[series])
}


