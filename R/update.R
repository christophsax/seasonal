#' Update and Re-evaluate a Seasonal Adjustment Model
#' 
#' Method to update and re-evaluate an object of class \code{"seas"}. 
#' 
#' Contrary to the default method of \code{\link{update}}, the  \code{"seas"}
#' method uses the evaluated call, rather than the actual call for re-
#' evaluation. This means you can savely use it in other functions, which 
#' is usuful with \code{\link{lapply}} and friends (see examples.)
#' 
#' @param object   an object of class \code{"seas"}, usually, a result of a 
#'     call to \code{\link{seas}}.
#' @param ...  spec-argument options sent to X-13 (with the same 
#'   syntax as in \code{\link{seas}}) 
#' @param evaluate logical. If \code{TRUE}, the call is evaluated.
#' @return Object of class \code{"seas"}. Or an object of class \code{"call"} 
#'   if \code{evaluate = FALSE}.
#' @seealso \code{\link{seas}} for the main function.
#' @seealso \code{\link{static}}, to return the (optionally evaluated) static 
#'   call of a \code{"seas"} object.
#' @export
#' @importFrom stats update
#' @method update seas
#' @examples
#' \dontrun{
#' # updating the call
#' m <- seas(AirPassengers)
#' update(m, x11 = "")
#' update(m, x = sqrt(AirPassengers), x11 = "")
#' 
#' # 'update' can be also used with lapply (or mapply)
#' 
#' # a list of time series
#' dta <- list(fdeaths = fdeaths, mdeaths = mdeaths)
#' 
#' # use 'seas' via lapply
#' ll <- lapply(dta, seas, x11 = "")
#' 
#' # use 'update' via lapply
#' lapply(ll, update, arima.model = c(0, 1, 1, 0, 1, 1))
#' }
update.seas <- function(object, ..., evaluate = TRUE){
  ml <- object$list
  ldots <- list(...)
  # overwrite args in existing list
  ml <- ml[!names(ml) %in% names(ldots)]

  # browser()
  if (!evaluate) return(update.default(object, evaluate = FALSE))

  seas(list = c(ml, ldots))
}

#' Seasonal Adjusted Series  
#' 
#' Returns the seasonally adjusted series of an
#' (optionally re-evaluated) model of class \code{"seas"}. Without further
#' arguments, this is equivalent to a call to the \code{\link{final}} function.
#' 
#' With the \code{newdata} argument supplied, the \code{"seas"} object is re-
#' evaluated, using the original model call. This is equivalent of calling 
#' \code{final(update(m, x = newdata))}.
#' 
#' @param object   an object of class \code{"seas"}.
#' @param newdata   an object of class \code{"ts"}. new data values for the 
#'   \code{x} argument in the \code{\link{seas}} function.
#' @param ...   further arguments, passed to \code{\link{update.seas}}, to 
#'   re-evaluate the model.
#' @return Object of class \code{"ts"}.
#' @export
#' @importFrom stats predict
#' @method predict seas
#' @examples
#' \dontrun{
#' # Using data from Dec. 59 to estimate a model
#' ap.short <- window(AirPassengers, end = c(1959, 12))
#' m <- seas(ap.short)
#' predict(m)
#' final(m)     # equivalent
#' 
#' # Use Dec. 59 model specification to estimate data up to Dec. 60
#' predict(m, AirPassengers)
#' }

predict.seas <- function(object, newdata, ...){
  if (missing("newdata")) {
    if (length(list(...)) == 0) return(final(object))
    return(final(update(object, ...)))
  }
  final(update(object, x = newdata, ...))
}

