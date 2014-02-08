#' Time Series from a Seasonal Adjustment Model
#' 
#' These functions extract time series from a \code{"seas"} object.
#' 
#' @param object  an object of class \code{"seas"}.
#' @param ...  not used. For compatibility with the generic.
#'   
#' @return returns a \code{"ts"} object, depending on the function.
#' 
#' @return \code{regressioneffects} returns a \code{"mts"} object, containing the regression variables multiplied by the vector of estimated 
#' regression coefficients
#'   
#' @seealso \code{\link{seas}} for the main function of seasonal.
#' 
#' @references Vignette with a more detailed description: 
#'   \url{http://cran.r-project.org/web/packages/seasonal/vignettes/seas.pdf}
#'   
#'   Wiki page with a comprehensive list of R examples from the X-13ARIMA-SEATS 
#'   manual: 
#'   \url{https://github.com/christophsax/seasonal/wiki/Examples-of-X-13ARIMA-SEATS-in-R}
#'   
#'   Official X-13ARIMA-SEATS manual: 
#'   \url{http://www.census.gov/ts/x13as/docX13AS.pdf}
#'   
#' @export
#' 
#' @examples
#' \dontrun{
#' 
#' m <- seas(AirPassengers)
#' 
#' final(m)
#' original(m)
#' irregular(m)
#' trend(m)
#' regressioneffects(m)
#' 
#' # trading day and easter adjustment w/o seasonal adjustment
#' summary(m)
#' re <- regressioneffects(m)
#' ce <- re[, 'Trading.Day'] + re[, 'Holiday'] 
#' # be aware of the log transformation
#' AirPassengersWoTd <- exp(log(AirPassengers) - ce)
#' 
#' # NA handling
#' AirPassengersNA <- window(AirPassengers, end = 1962, extend = TRUE)
#' final(seas(AirPassengersNA, na.action = na.omit))    # no NA in final series
#' final(seas(AirPassengersNA, na.action = na.exclude)) # NA in final series
#' # final(seas(AirPassengersNA, na.action = na.fail))    # fails
#' }
#' @export
final <- function(object){
  extract_w_na_action(object, 'final')
}

#' @rdname final
#' @export
original <- function(object){
  object$x
}

#' @rdname final
#' @export
trend <- function(object){
  extract_w_na_action(object, 'trend')
}

#' @rdname final
#' @export
irregular <- function(object){
  extract_w_na_action(object, 'irregular')
}


#' @export
#' @rdname final
regressioneffects <- function(object){
  object$regressioneffects
}


#' @method residuals seas
#' @rdname final
#' @export
residuals.seas <- function(object, ...){
  if ('residuals' %in% colnames(object$data)){
    z <- na.omit(object$data[,'residuals'])
  } else {
    z <- NULL
  }
  z
}


extract_w_na_action <- function(x, name){
  # extract a data series and applies na_action according to the attribute
  #
  # x "seas" object
  # 
  # returns a "ts" object
  # 
  # used by: time series extractor functions
  #
  z <- na.omit(x$data[, name])
  if (!is.null(x$na.action)){
    if (attr(x$na.action, "class") == "exclude") {
      z <- ts(napredict(x$na.action, z))
      tsp(z) <- tsp(original(x))
    }
  }
  z
}







