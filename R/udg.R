#' X-13ARIMA-SEATS Stats
#' 
#' Returns an object of class \code{"list"} that contains properly parsed
#' content of the \code{udg} stat file.
#' 
#' @param x      an object of class \code{"seas"}.
#' @param stats  character vector. If specified, only a subset of the available 
#'  stats are returned. This speeds up the call, as only a subset needs to be 
#'  parsed. Should be used for programming.
#' @param fail   if TRUE, an error is droped if a stat is not in the \code{udg}
#'  file.
#'   
#' @return a named \code{"list"}.
#' 
#' @export
#' @examples
#' 
#' m <- seas(AirPassengers, x11 = "")
#'
#' # a list with all entries from udg
#' udg(m)
#' names(udg(m))  # show all available stats
#'
#' # extracting a few selected stats from udg
#' udg(m, c("f3.m01", "f3.m02", "f3.m03"))
#'
#' # this is faster than:
#' udg(m)[c("f3.m01", "f3.m02", "f3.m03")]
#'
#' # a table with some m quality mesures
#' do.call(rbind, udg(m, c("f3.m01", "f3.m02", "f3.m03")))
#' @export
udg <- function(x, stats = NULL, fail = TRUE){
  stopifnot(inherits(x, "seas"))

  xx <- x$udg

  if (!is.null(stats)){
    stopifnot(inherits(stats, "character"))
    if (any(!stats %in% names(xx))){
      if (fail) {
        stop(paste(stats[!stats %in% names(xx)], collapse = ", "), " not in 'names(x$udg)'")
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
    z[is.char] <- lapply(z[is.char], as.character)

    # detect muli-element numeric vecotors
    zc <- z[is.char]
    is.long <- grepl("\\d+ +.", zc)

    # exclude false positives (ARIMA models)
    is.long[is.long] <- !grepl("\\(", zc[is.long])

    # split at spaces and convert to numeric
    lsplit <- strsplit(as.character(zc[is.long]), split = " +")
    ll <- lapply(lsplit, type.convert)
    ll[sapply(ll, inherits, "factor")] <- lapply(ll[sapply(ll, inherits, "factor")], as.character)

    names(ll) <- names(zc[is.long])

    # pug in back
    z[is.char][is.long] <- ll
  }
  

  z
}
