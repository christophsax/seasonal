#' Static Call of a seas Object
#'
#' In a 'static' call, the default automatic procedures in the model call
#' are substituted by the choices they made.
#'
#' If `evaluate = TRUE`, the call is evaluated. The call can be copy/pasted
#' to a script and used for further manipulations or future evaluation of the
#' same model.
#'
#' By default, the static call is tested. It is executed and compared to the
#' input call. If the final series is not identical, a message is returned.
#'
#' If `coef = TRUE`, the coefficients are fixed as well. If
#' `x11.filter = TRUE`, the X-11 moving averages are fixed as well.
#'
#' @param x an object of class `seas`.
#' @param coef  logical. If `TRUE`, the coefficients are treated as fixed,
#'   instead of being estimated.
#' @param x11.filter logical. X-11 only. if `TRUE`, the X-11 moving
#'   averages will be fixed as well. This leads to different filters at
#'   different stages, and the resulting series can be are slightly different.
#'   If `test = TRUE`, this may cause a warning  message.
#' @param test logical. By default the static call is executed and compared to
#'   the input call. If the final series is not identical, a message is
#'   returned. If `FALSE`, no test is performed (faster).
#' @param fail logical. If `TRUE`, differences will cause an error. Ignored
#'   if `test = FALSE`.
#' @param evaluate logical. If `TRUE`, the call is evaluated.
#' @return Object of class `"call"`. Or an object of class `"seas"`
#'   if `evaluate = TRUE`.
#' @seealso [stats::getCall()] to extract the actual call.
#' @seealso [seas()] for the main function of seasonal.
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
#' \donttest{
#'
#' m <- seas(AirPassengers)
#' getCall(m)                   # default call
#' static(m)                    # static call
#' static(m, test = FALSE)      # much faster
#' static(m, evaluate = TRUE)   # returns an object of class "seas"
#'
#' m <- seas(AirPassengers, x11 = "")
#'
#' static(m, x11.filter = TRUE) # also fixes the X-11 filter (with a warning)
#' static(m, coef = TRUE)       # also fixes the coefficients
#' }
static <- function(x, coef = FALSE, x11.filter = FALSE, test = TRUE,
                   fail = FALSE, evaluate = FALSE){

  if (!inherits(x, "seas")){
    stop("first argument must be of class 'seas'")
  }

  lc <- x$list

  if ("list" %in% names(lc)){
    stop("static does not work with the 'list' argument in seas")
  }

  lc$regression.variables <- x$model$regression$variables
  lc$arima.model <- x$model$arima$model

  # Turn off outomatic procedures:

  # remove all arguments to the auto specs
  lc <- lc[!grepl("^automdl", names(lc))]
  lc <- lc[!grepl("^outlier", names(lc))]
  lc <- lc[!grepl("^pickmdl", names(lc))]

  # To assign NULL instead of removing the element, do this trick
  lc['regression.aictest'] <- NULL
  names(lc['regression.aictest']) <- "regression.aictest"

  lc['outlier'] <- NULL
  names(lc['outlier']) <- "outlier"

  lc$transform.function = transformfunction(x)

  # substitute X-11 filters
  if (x11.filter){
    if (!is.null(x$spc$x11)) {
      if (is.null(lc$x11.trendma) || lc$x11.trendma == ""){
        tma <- udg(x, "finaltrendma", fail = FALSE)
        lc$x11.trendma <- as.numeric(unname(tma))
      }
      if (is.null(lc$x11.seasonalma) || lc$x11.seasonalma == ""){
        sma <- udg(x, "sfmsr", fail = FALSE)
        if (!is.null(sma)) sma <- paste0("s", sma)
        lc$x11.seasonalma <- unname(sma)
      }
    }
  }

  if (coef){
    if (!is.null(x$model$regression$b)) {
      lc$regression.b = c(add_f(x$model$regression$b))
    }
    if (!is.null(x$model$arima$ma)) {
      lc$arima.ma = add_f(x$model$arima$ma)
    }
    if (!is.null(x$model$arima$ar)) {
      lc$arima.ar = add_f(x$model$arima$ar)
    }
  }

  if (test){
    # testing the static call
    x.static <- seas(list = lc)
    test <- (all.equal(final(x.static), final(x), tolerance = 1e-05))
    if (!isTRUE(test)){
      (if (fail) stop else message)(paste("Static series is different.", test))
    }
  }

  if (evaluate){
    return(seas(list = lc))
  }

  # prepare call, using same series names as orginal call
  z <- as.call(c(quote(seas), lc))
  z0 <- x$call
  is.ser <- intersect(c("x", "xreg", "xtrans"), names(z0))
  z[is.ser] <- z0[is.ser]

  message(pretty_call(z))

  invisible(z)
}

pretty_call <- function(z) {
  l <- as.list(z)
  deparse2 <- function(e) paste(deparse(e, width.cutoff = 500), collapse = " ")
  args <- unlist(unname(Map(paste, names(l[-1]), lapply(l[-1], deparse2), sep = " = ")))
  args_indented <- paste0("  ", args)
  paste(
    paste0(as.character(l[[1]]), "("),  # open
    paste(args_indented, collapse = ",\n"),
    ")",                                # close
    sep = "\n"
  )
}




add_f <- function(x){
  # Make coefficents 'fixed'
  #
  # Put a "f" at the end of number, if not already there
  #
  # x <- c("2342f", "324234")
  # SubFixed(x)

  z <- paste0(x, "f")
  gsub("f+", "f", z)
}
