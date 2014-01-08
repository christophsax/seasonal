#' Extractor Functions for seas Elements and Series
#' 
#' These functions extract elements or series from a \code{"seas"} object. The 
#' extractor functions for time series also deal with the treatment of NA 
#' values.
#' 
#' @param x  an object of class \code{"seas"}.
#'   
#' @return return an element or a series, depending on the function
#'   
#' @seealso \code{\link{seas}} for the main function.
#' @seealso \code{\link{regressioneffects}}, for how to extract the regression effects
#'   of the regARIMA model.
#' @seealso \code{\link{fivebestmdl}}, for how to extract the five best ARIMA models.
#'   
#' @export
#' 
#' @examples
#' \dontrun{
#' x <- seas(AirPassengers)
#' 
#' final(x)
#' original(x)
#' irregular(x)
#' trend(x)
#' 
#' # QS seasonality statistics
#' qs(x)
#' 
#' spc(x)  # X13-ARIMA-SEATS .spc file
#' mdl(x)  # X13-ARIMA-SEATS .mdl file
#' }
#' 
spc <- function(x){
  x$spc
}

#' @rdname spc
#' @export
final <- function(x){
  extract_w_na_action(x, 'final')
}

#' @rdname spc
#' @export
original <- function(x){
  x$x
}

#' @rdname spc
#' @export
trend <- function(x){
  extract_w_na_action(x, 'trend')
}

#' @rdname spc
#' @export
irregular <- function(x){
  extract_w_na_action(x, 'irregular')
}

#' Show regressioneffects
#' 
#' matrix of regression variables multiplied by the vector of estimated 
#' regression coefficients
#' 
#' @param x  object of class "seas"
#' 
#' @export
#' @examples
#' \dontrun{
#' # trading day and easter adjustment w/o seasonal adjustment:
#' x <- seas(AirPassengers)
#' 
#' summary(x)
#' re <- regressioneffects(x)
#' ce <- re[, 'Trading.Day'] + re[, 'Holiday'] 
#'  # be aware of log transformation
#' AirPassengersWoTd <- exp(log(AirPassengers) - ce)
#' }
regressioneffects <- function(x){
  x$regressioneffects
}




#' Extract the five best ARIMA models
#' 
#' if the \code{automdl} spec is activated (as in the default), the function
#' \code{fivebestmdl} returns the five best models as chosen by the BIC
#' criterion.
#' 
#' @param x  object of class "seas"
#'   
#' @export
#' @examples
#' \dontrun{
#' x <- seas(AirPassengers)
#' fivebestmdl(x)
#' }
#' @export
fivebestmdl <- function(x){
  if (!is.null(x$spc$automdl)){
    txt <- x$fivebestmdl[3:7]
    arima <- substr(txt, start = 19, stop = 32)
    bic <- as.numeric(substr(txt, start = 51, stop = 56))
    z <- data.frame(arima, bic, stringsAsFactors = FALSE)
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



#' QS Statistic For Seasonality
#' 
#' Null Hypothetis: No Seasonality
#' 
#' @param x  object of class \code{"seas"}
#' @export
qs <- function(x){
  x$qs
}

