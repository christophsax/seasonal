#' Summary of a X13-ARIMA-SEATS seasonal adjustment
#' 
#' \code{summary} method for class "seas".
#' 
#' @param object      an object of class \code{"seas"}, usually, a result of a 
#'                    call to \code{\link{seas}}.
#' @param x           an object of class \code{"summary.seas"}, usually, a result 
#'                    of a call to \code{summary.seas}.
#' @param digits      the number of significant digits to use when printing.
#' @param signif.stars logical. If \code{TRUE}, 'significance stars' are printed 
#'                    for each coefficient.
#' @param \dots       further arguments passed to or from other methods.

#' @return \code{summary.seas} returns a list containing the summary statistics 
#'   included in \code{object}, and computes the following additional
#'   statistics:
#'  
#'   \item{coefficients}{a named matrix containing coefficients, standard
#'   deviations, t-values and p-values}
#'   
#'   The \code{print} method prints the summary output in a similar way as the method for \code{"lm"}.
#'   
#' @examples
#' x <- seas(AirPassengers)
#' summary(x)  
#'   
#' @method summary seas
#' @export
summary.seas <- function(object, ...){
  # build output on top of the input
  z <- object
  
  # coefficents matrix
  if (!is.null(coef(object))){
    est  <- coef(object)
    se   <- object$se
    tval <- est/se
    pval <- 2 * pnorm(-abs(tval))
    
    z$coefficients <- cbind(est, se, tval, 
                            2 * pnorm(abs(tval), lower.tail = FALSE))
    dimnames(z$coefficients) <- list(names(est),
                                     c("Estimate", "Std. Error", "z value", 
                                       "Pr(>|z|)"))
  }
  class(z) <- "summary.seas"
  z
}



#' @method print summary.seas
#' @export
#' @rdname summary.seas
print.summary.seas <- function (x, digits = max(3, getOption("digits") - 3), 
                              signif.stars = getOption("show.signif.stars"), ...) {
  
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n",
      sep = "")
#   resid <- x$residuals
#   cat("Residuals:\n")
#   if (length(resid) > 5) {
#     nam <- c("Min", "1Q", "Median", "3Q", "Max")
#     quantile.resid <- zapsmall(quantile(resid), digits + 1)
#     print(structure(quantile.resid, names = nam), digits = digits)
#   } else {
#     print(resid, digits = digits)
#   }
  
  if (is.null(coef(x))) {
    cat("\nNo Coefficients\n")
  } else {
    cat("\nCoefficients:\n")
    coefs <- coef(x)
    printCoefmat(coefs, digits = digits, signif.stars = signif.stars,
                 na.print = "NA")
  }
  
#   cat("\n'", x$method, "' disaggregation with '", x$conversion, 
#       "' conversion", sep = "")
#   cat("\n", x$n_l, " low-freq. obs. converted to ", x$n, " high-freq. obs.", sep="")
#   if (!is.null(x$adj.r.squared)) {
#     cat("\nAdjusted R-squared:", formatC(x$adj.r.squared, digits = digits))
#   }
#   if (!is.null(x$rho)) {
#     cat("\tAR1-Parameter:", formatC(x$rho, digits = digits))
#     if (x$truncated){
#       cat(" (truncated)")
#     }
#   }
#   if (!is.null(x$criterion)) {
#     cat("\ncriterion:", x$criterion, "\torder of differencing 'h':", x$h)
#   }
  cat("\n")
  invisible(x)
}
