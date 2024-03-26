#' Handle Missing Values by X-13
#'
#' Utility function to substitute NA values by -99999. Useful as a value for the
#' `na.action` argument in [seas()].
#'
#' @param x an object of class `"ts"`
#'
#' @return a time series, with NA values substituted by -99999.
#'
#' @export
#' @examples
#' \donttest{
#' AirPassengersNA <- AirPassengers
#' AirPassengersNA[20] <- NA
#' na.x13(AirPassengersNA)
#'
#' seas(AirPassengersNA, na.action = na.x13)
#' }
#'
na.x13 <- function(x){
  stopifnot(inherits(x, "ts"))
  x[is.na(x)] <- -99999
  x
}

#' @method na.exclude ts
na.exclude.ts <- function(object, ...){
  # time series method for na.exclude
  #
  # does not exist in the base package
  #
  # object  a "ts" object
  #
  # returns a "ts" object
  #
  # required by seas
  z <- na.omit(object)
  if (!is.null(attr(z, "na.action"))){
    attr(attr(z, "na.action"), "class") <- "exclude"
  }
  z
}
