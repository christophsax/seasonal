#' Defunct Functions
#' 
#' The \code{arimamodel} functions is defunct now. Use 
#' the more univeral \code{\link{udg}} function.
#' @param x object of class \code{"seas"}
#' @rdname seasonal-defunct
#' @seealso \code{\link{udg}}, for universal access to X-13 statistics
#' @examples
#' \dontrun{
#' m <- seas(AirPassengers)
#' udg(x, "x13mdl")
#' }
#' @export
arimamodel <- function(x){
  stopifnot(inherits(x, "seas"))

  msg <- 'udg(x, "x13mdl")'
  .Defunct(msg)

  str <- x$model$arima$model
  str <- gsub("[ \\(\\)]", "", str)
  z <- c(substr(str, 1, 1),
         substr(str, 2, 2),
         substr(str, 3, 3),
         substr(str, 4, 4),
         substr(str, 5, 5),
         substr(str, 6, 6)
  )
  as.numeric(z)
}
