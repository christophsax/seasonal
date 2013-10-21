#' @export
#' @examples
#' seasonalyze(AirPassengers)
seasonalyze <- function(x){
  stopifnot(inherits(x, "ts"))
  require(manipulate)
  
  manipulate(
    SubPlot(x, outlier.critical), 
    outlier.critical = slider(2.5, 5, initial = 4)
  )
}


SubPlot <- function(x, outlier.critical){
  s <- seas(x, outlier.critical = outlier.critical)
  plot(s)
  cat("\nStatic Call:\n")
  static(s)
}

plot(seas(AirPassengers))

plot