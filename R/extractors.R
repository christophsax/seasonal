#' Time Series of a Seasonal Adjustment Model
#'
#' Functions to extract the main time series from a `"seas"` object. For
#' universal import of X-13ARIMA-SEATS tables, use the [series()]
#' function.
#'
#' These functions support R default NA handling. If `na.action =
#' na.exclude` is specified in the call to `seas`, the time series will
#' also contain NAs.
#'
#' @param object  an object of class `"seas"`.
#' @param ...  not used. For compatibility with the generic.
#'
#' @return returns a `"ts"` object, depending on the function.
#'
#' @seealso [seas()] for the main function of seasonal.
#' @seealso [series()], for universal X-13 output extraction.
#'
#' @references Vignette with a more detailed description:
#'   <http://www.seasonal.website/seasonal.html>
#'
#'   Comprehensive list of R examples from the X-13ARIMA-SEATS manual:
#'   <http://www.seasonal.website/examples.html>
#'
#'
#'
#'   Official X-13ARIMA-SEATS manual:
#'   <https://www2.census.gov/software/x-13arima-seats/x13as/windows/documentation/docx13as.pdf>
#'
#' @export
#'
#' @examples
#' \donttest{
#'
#' m <- seas(AirPassengers)
#'
#' final(m)
#' original(m)
#' irregular(m)
#' trend(m)
#'
#' # NA handling
#' AirPassengersNA <- window(AirPassengers, end = 1962, extend = TRUE)
#' final(seas(AirPassengersNA, na.action = na.omit))    # no NA in final series
#' final(seas(AirPassengersNA, na.action = na.exclude)) # NA in final series
#' final(seas(AirPassengersNA, na.action = na.x13))     # NA filled by x13
#' # final(seas(AirPassengersNA, na.action = na.fail))    # fails
#' }
#' @export
final <- function(object){
  if (!inherits(object, "seas") && is.list(object)) {
    return(do.call(cbind, lapply(object, extract_w_na_action, name = 'final')))
  }
  extract_w_na_action(object, 'final')

}

#' @rdname final
#' @export
original <- function(object){
  if (!inherits(object, "seas") && is.list(object)) {
    return(do.call(cbind, lapply(object, function(e) e$x)))
  }
  object$x
}

#' @rdname final
#' @export
trend <- function(object){
  if (!inherits(object, "seas") && is.list(object)) {
    return(do.call(cbind, lapply(object, extract_w_na_action, name = 'trend')))
  }
  extract_w_na_action(object, 'trend')
}

#' @rdname final
#' @export
irregular <- function(object){
  if (!inherits(object, "seas") && is.list(object)) {
    return(do.call(cbind, lapply(object, extract_w_na_action, name = 'irregular')))
  }
  extract_w_na_action(object, 'irregular')
}


#' @method residuals seas
#' @rdname final
#' @export
residuals.seas <- function(object, ...){
  series(object, "rsd")
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
  if (is.null(x$data)) return (NULL)
  z <- na.omit(x$data[, name])
  if (!is.null(x$na.action)){
    if (attr(x$na.action, "class") == "exclude") {
      z <- ts(napredict(x$na.action, z))
      tsp(z) <- tsp(original(x))
    }
  }
  z
}







