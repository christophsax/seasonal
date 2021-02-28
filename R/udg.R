#' Diagnostical Statistics
#'
#' The `udg` function provides access to a large number of diagnostical
#' statistics. The `qs` function and the `AIC`, `BIC` and
#' `logLik` methods are  wrappers that use `udg` to access some
#' specific diagnostical statistics.
#'
#' @param x,object  an object of class `"seas"`.
#' @param stats  character vector; if specified, only a subset of the available
#'   stats are returned. This speeds up the call, as only a subset needs to be
#'   type converted. Should be used for programming.
#' @param simplify  logical; should the result be simplified to a vector or
#'   matrix, if possible?
#' @param fail   logical; if `TRUE`, an error is droped if an element of
#'   `stats` is missing in `names(udg(x))`.
#' @param ...   further arguments (not used)
#' @return  `qs` returns the QS statistics for seasonality of input and
#'   output series and the corresponding p-values.
#' @return  `AIC`, `BIC`, `nobs` and `logLik` return the
#'   corresponding statistics.
#'
#' @seealso [seas()] for the main function.
#' @seealso [series()], for universal X-13 output extraction.
#' @seealso [plot.seas()], for diagnostical plots.
#' @seealso [out()], for accessing the full output of X-13ARIMA-SEATS.
#'
#' @references Vignette with a more detailed description:
#'   <http://www.seasonal.website/seasonal.html>
#'
#'   Comprehensive list of R examples from the X-13ARIMA-SEATS manual:
#'   <http://www.seasonal.website/examples.html>
#'
#'   Official X-13ARIMA-SEATS manual:
#'   <https://www.census.gov/ts/x13as/docX13ASHTML.pdf>
#'
#' @export
#' @examples
#' \dontrun{
#' m <- seas(AirPassengers, x11 = "")
#'
#' qs(m)
#' AIC(m)
#' BIC(m)
#' nobs(m)
#' logLik(m)
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

  z <- lapply(xx, function(e) type.convert(e, as.is = FALSE))

  # factors to character
  is.char <- sapply(z, inherits, "factor")
  if (any(is.char)){

    z[is.char] <- lapply(z[is.char], as.character)

    # detect muli-element numeric vecotors
    zc <- z[is.char]
    is.long <- grepl("\\d+ +.", zc)

    ## exclude false positives
    # ARIMA models
    is.long[is.long] <- !grepl("\\(", zc[is.long])
    # time spans
    is.long[grepl("span$", names(zc))] <- FALSE

    # split at spaces and convert to numeric
    if (any(is.long)){
      lsplit <- strsplit(as.character(zc[is.long]), split = " +")
      ll <- lapply(lsplit, type.convert, as.is = FALSE)
      ll[sapply(ll, inherits, "factor")] <- lapply(ll[sapply(ll, inherits, "factor")], as.character)

      names(ll) <- names(zc[is.long])
      # plug in back
      z[is.char][is.long] <- ll
    }
  }
  z <- if (simplify) simplify2array(z) else z

  # class(z) <- c("udg", class(z))

  z
}


#' @rdname udg
#' @export
qs <- function(x){
  qs.var <- c("qsori", "qsorievadj", "qsrsd", "qssadj", "qssadjevadj", "qsirr",  "qsirrevadj", "qssori", "qssorievadj", "qssrsd", "qsssadj", "qsssadjevadj",  "qssirr", "qssirrevadj")
  z <- t(udg(x, qs.var, fail = FALSE))
  colnames(z) <- c("qs", "p-val")
  z
}


#' @rdname udg
#' @method AIC seas
#' @export
AIC.seas <- function(object, ...){
  unname(udg(object, "aic"))
}

#' @rdname udg
#' @method BIC seas
#' @export
BIC.seas <- function(object, ...){
  unname(udg(object, "bic"))
}


#' @rdname udg
#' @export
#' @method nobs seas
nobs.seas <- function(object, ...){
  length(final(object))
}


#' @rdname udg
#' @method logLik seas
#' @export
logLik.seas <- function(object, ...){
  unname(udg(object, "loglikelihood"))
}


