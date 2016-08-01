#' X-13ARIMA-SEATS Stats
#' 
#' The udg file contains most statistics of an X-13 run. The 
#' \code{udg} function returns its content in a properly parsed form.
#' 
#' @param x  an object of class \code{"seas"}.
#' @param stats  character vector. If specified, only a subset of the available 
#'  stats are returned. This speeds up the call, as only a subset needs to be 
#'  parsed. Should be used for programming.
#' @param simplify  logical; should the result be simplified to a vector, if possible?
#' @param fail   logical; if TRUE, an error is droped if an element of 
#'   \code{stats} is missing in \code{names(udg(x))}.
#'   
#' @return a named vector or list.
#' 
#' @export
#' @examples
#' \dontrun{
#' m <- seas(AirPassengers, x11 = "")
#'
#' # a list with all entries from udg
#' udg(m)
#'
#' # extracting a few selected stats from udg
#' udg(m, c("f3.m02", "f3.m05", "qsori"))  # returns a list
#' udg(m, c("f3.m02", "f3.m05"))           # returns a vector
#'
#' # faster than:
#' udg(m)[c("f3.m01", "f3.m02", "qsori")]
#' }
#' @export
udg <- function(x, stats = NULL, simplify = TRUE, fail = TRUE){
  stopifnot(inherits(x, "seas"))

  xx <- x$udg

  if (!is.null(stats)){
    stopifnot(inherits(stats, "character"))
    if (any(!stats %in% names(xx))){
      if (fail) {
        stop(paste(stats[!stats %in% names(xx)], collapse = ", "), " not contained in 'names(udg(x))'")
      } else {
        stats <- stats[stats %in% names(xx)]
        if (length(stats) == 0) return(NULL)
      }
    }
    xx <- xx[stats]
  }

  z <- lapply(xx, function(e) type.convert(e))

  # factors to character
  is.char <- sapply(z, inherits, "factor")
  if (any(is.char)){
    # browser()
    z[is.char] <- lapply(z[is.char], as.character)

    # detect muli-element numeric vecotors
    zc <- z[is.char]
    is.long <- grepl("\\d+ +.", zc)

    # exclude false positives (ARIMA models)
    is.long[is.long] <- !grepl("\\(", zc[is.long])

    # split at spaces and convert to numeric
    if (any(is.long)){
      lsplit <- strsplit(as.character(zc[is.long]), split = " +")
      ll <- lapply(lsplit, type.convert)
      ll[sapply(ll, inherits, "factor")] <- lapply(ll[sapply(ll, inherits, "factor")], as.character)

      names(ll) <- names(zc[is.long])
      # plug in back
      z[is.char][is.long] <- ll
    }
  }
  if (simplify) simplify2array(z) else z
}
