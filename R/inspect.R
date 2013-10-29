

#' Inspection Tool
#' 
#' @param x an object of class \code{"ts"}
#' 
#' @export
#' @examples
#' inspect(AirPassengers)
inspect <- function(x, ...){
  stopifnot(inherits(x, "ts"))
  
  dotlist <- list(...)
  
  require(manipulate)
  
  tsname <- deparse(substitute(x))

  controls <- list(
    view = picker("Series", "Seasonal component", "Irregular component", "Spectrum original", "Spectrum final", "Residuals of regARIMA", label = "View"),
    method = picker("SEATS", "X11", label = "Method"),
    modelsearch = picker("automdl", "pickmdl", label = "Model"),
    calendar = picker("chi2test", "aictest", label = "Calendar"),
    outlier.critical = slider(2.5, 5, initial = 4),
    is.static.call = checkbox(FALSE, "Show static call")
  )
  
  manipulate({
    lcall <- structure(list(quote(seas)), .Names = "")
    lcall$x <- parse(text = tsname)[[1]]
    lcall$outlier.critical <- outlier.critical
    
    if (method == "X11"){
      lcall$x11 = list()
    }
    
    if (modelsearch == "pickmdl"){
      lcall$pickmdl = list()
    }
    
    if (calendar == "aictest"){
      lcall$regression.aictest = c("td", "easter")
    }
    
    if (length(dotlist) > 0){
      lcall <- c(lcall, dotlist)
    }

    call <- as.call(lcall)
    
    SubPlot(x, tsname, view,
            call,
            is.static.call
            )
  }, controls)
  
}


SubPlot <- function(x, tsname, view,
                    call,
                    is.static.call
                    ){

  s <- eval(call)
  
  if (view == "Series"){
    plot(s)
  } else if (view == "Seasonal component"){
    monthplot(s, choice = "seasonal")
  } else if (view == "Irregular component"){
    monthplot(s, choice = "irregular")
  } else if (view == "Spectrum original"){
    spectrum(original(s))
  } else if (view == "Spectrum final"){
    spectrum(final(s))
  } else if (view == "Residuals of regARIMA"){
    residplot(s)
  } else {
    stop("something wrong.")
  }
  
  print(summary(s))
  
  if (is.static.call){
    cat("\nStatic Call:\n")
    static(s, name = tsname, test = TRUE)
  }

  
}

