

#' Inspection Tool
#' 
#' @param x an object of class \code{"ts"}
#' 
#' @export
#' @examples
#' inspect(AirPassengers)
inspect <- function(x){
  stopifnot(inherits(x, "ts"))
  require(manipulate)
  
  tsname <- deparse(substitute(x))

  controls <- list(
    outlier.critical = slider(2.5, 5, initial = 4),
    
    outliers = checkbox(TRUE),
    trend = checkbox(FALSE)
  )
  
  manipulate({
    SubPlot(x, tsname,
            outlier.critical,
            outliers, trend)
  }, controls)
  
}


SubPlot <- function(x, tsname,
                    outlier.critical,
                    outliers, trend
                    ){
  s <- seas(x, outlier.critical = outlier.critical)
  plot(s, outliers, trend)
  cat("\nStatic Call:\n")
  static(s, name = tsname)
}

