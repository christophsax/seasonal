#' Handle Missing Values by X-13
#' 
#' Utility function to substitute NA values by -99999. Useful as a value for the
#' \code{na.action} argument in \code{\link{seas}}.
#' 
#' @param x an object of class \code{"ts"}
#' 
#' @return a time series, with NA values substituted by -99999.
#' 
#' @export
#' @examples
#' \dontrun{
#' #' AirPassengersNA <- AirPassengers
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


na.exclude.ts <- function(x){
  # time series method for na.exclude
  # 
  # does not exist in the base package
  # 
  # x  a "ts" object
  # 
  # returns a "ts" object
  #
  # required by seas
  z <- na.omit(x)
  if (!is.null(attr(z, "na.action"))){
    attr(attr(z, "na.action"), "class") <- "exclude"
  }
  z
}