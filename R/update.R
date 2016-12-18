# this would be the 'update' function 
# (which could have a different name, of course)

# rerunning with new data or new arguments
# m2 <- update(m, x = sqrt(AirPassengers))
# m3 <- update(m, xreg = sqrt(tls))
# m4 <- update(m, x11 = "")


#' Update Method
#' @param object   an object of class \code{"seas"}, usually, a result of a 
#'     call to \code{\link{seas}}.
#' @param ...  spec-arguments options sent to X-13ARIMA-SEATS (see seas) 
#' @export
#' @importFrom stats update
#' @method update seas
update.seas <- function(object, ...){
  ml <- object$list
  ldots <- list(...)
  # overwrite args in existing list
  ml <- ml[!names(ml) %in% names(ldots)]
  seas(list = c(ml, ldots))
}

#' Predict Method
#' @param object   an object of class \code{"seas"}, usually, a result of a 
#'                    call to \code{\link{seas}}.
#' @param newdata   an object of class \code{"ts"}
#' @param ...   not used
#' @export
#' @importFrom stats predict
#' @method predict seas
predict.seas <- function(object, newdata, ...){
  if (missing("newdata")) return(final(object))
  final(update(object, x = newdata))
}
