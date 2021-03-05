#' Update and Re-evaluate a Seasonal Adjustment Model
#'
#' Method to update and re-evaluate an object of class `"seas"`.
#'
#' Contrary to the default method of [update()], the  `"seas"`
#' method uses the evaluated call, rather than the actual call for re-
#' evaluation. This means you can savely use it in other functions, which
#' is useful with [lapply()] and friends (see examples.)
#'
#' @param object   an object of class `"seas"`, usually, a result of a
#'     call to [seas()].
#' @param ...  spec-argument options sent to X-13 (with the same
#'   syntax as in [seas()])
#' @param evaluate logical. If `TRUE`, the call is evaluated.
#' @return Object of class `"seas"`. Or an object of class `"call"`
#'   if `evaluate = FALSE`.
#' @seealso [seas()] for the main function.
#' @seealso [static()], to return the (optionally evaluated) static
#'   call of a `"seas"` object.
#' @export
#' @importFrom stats update
#' @method update seas
#' @examples
#' \donttest{
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

  if (!evaluate) return(update.default(object, evaluate = FALSE))

  seas(list = c(ml, ldots))
}

#' Seasonal Adjusted Series
#'
#' Returns the seasonally adjusted series of an
#' (optionally re-evaluated) model of class `"seas"`. Without further
#' arguments, this is equivalent to a call to the [final()] function.
#'
#' With the `newdata` argument supplied, the `"seas"` object is re-
#' evaluated, using the original model call. This is equivalent of calling
#' `final(update(m, x = newdata))`.
#'
#' @param object   an object of class `"seas"`.
#' @param newdata   an object of class `"ts"`. new data values for the
#'   `x` argument in the [seas()] function.
#' @param ...   further arguments, passed to [update.seas()], to
#'   re-evaluate the model.
#' @return Object of class `"ts"`.
#' @export
#' @importFrom stats predict
#' @method predict seas
#' @examples
#' \donttest{
#' # Using data from Dec. 59 to estimate a model
#' ap.short <- window(AirPassengers, end = c(1959, 12))
#' m <- seas(ap.short)
#' predict(m)
#' final(m)     # equivalent
#'
#' # Use Dec. 59 model specification to estimate data up to Dec. 60
#' predict(m, AirPassengers)
#' }

predict.seas <- function(object, newdata, ...){
  if (missing("newdata")) {
    if (length(list(...)) == 0) return(final(object))
    return(final(update(object, ...)))
  }
  final(update(object, x = newdata, ...))
}

