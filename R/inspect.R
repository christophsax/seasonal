

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
    view = picker("Series", "Seasonal component", "Irregular component", "Spectrum original", "Spectrum final", label = "View"),
    method = picker("SEATS", "X11", label = "Method"),
    outlier.critical = slider(2.5, 5, initial = 4),
    static.call = checkbox(FALSE, "Show static call")
  )
  
  manipulate({
    SubPlot(x, tsname, view,
            method,
            outlier.critical,
            static.call
            )
  }, controls)
  
}


SubPlot <- function(x, tsname, view,
                    method,
                    outlier.critical,
                    static.call
                    ){
  if (method == "X11"){
    s <- seas(x, outlier.critical = outlier.critical, x11 = list())
  } else {
    s <- seas(x, outlier.critical = outlier.critical)
  }
  
  
  if (view == "Series"){
    plot(s)
  } else if (view == "Seasonal component"){
    monthplot(s, choice = "seasonal")
  } else if (view == "Irregular component"){
    monthplot(s, choice = "irregular")
  } else if (view == "Spectrum original"){
    spec.ar(original(s))
  } else if (view == "Spectrum final"){
    spec.ar(final(s))
  } else {
    stop("something wrong.")
  }
  
  if (static.call){
    cat("\nStatic Call:\n")
    static(s, name = tsname, test = TRUE)
  }

}

