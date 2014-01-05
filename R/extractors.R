#' Extractor Functions for seas Elements and Series
#' 
#' These functions extract elements or series from a \code{"seas"} object.
#' 
#' @param x  an object of class \code{"seas"}.
#'   
#' @return return an element or a series, depending on the function
#'   
#' @seealso \code{\link{seas}} for the main function.
#'   
#' @export
#' @examples
#' \dontrun{
#' x <- seas(AirPassengers)
#' 
#' final(x)
#' original(x)
#' irregular(x)
#' trend(x)
#' 
#' spc(x)  # X13-ARIMA-SEATS .spc file
#' mdl(x)  # X13-ARIMA-SEATS .mdl file
#' 
#' # out(x)  # returns an error
#' x2 <- seas(AirPassengers, out = TRUE)
#' out(x2)    # this works: X13-ARIMA-SEATS .out file
#' }
#' 
spc <- function(x){
  x$spc
}

#' @rdname spc
#' @export
mdl <- function(x){
  x$mdl
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
#' 
#' # trading day and easter adjustment w/o seasonal adjustment:
#' x <- seas(AirPassengers)
#' 
#' summary(x)
#' re <- regressioneffects(x)
#' ce <- re[, 'Trading.Day'] + re[, 'Holiday'] 
#'  # be aware of log transformation
#' AirPassengersWoTd <- exp(log(AirPassengers) - ce)
#' 
regressioneffects <- function(x){
  x$regressioneffects
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



#' @rdname seas
#' @export
fivebestmdl <- function(x){
  if (is.null(x$spc$automdl)){
    stop("only works with 'automdl' spc")
  }
  cat(paste(x$fivebestmdl, collapse = "\n"))
}


#' @rdname seas
#' @export
qs <- function(x){
  x$qs
}

