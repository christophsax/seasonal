#' Summary of a X13-ARIMA-SEATS seasonal adjustment
#' 
#' Like the corresponding method for \code{"lm"} objects, the method for
#' \code{"seas"} objects returns the estimated coefficients, its standard errors,
#' z-statistics and corresponding (two-sided) p-values. Coefficients are returned
#' both for the exogenous regressors and the coefficients of the ARIMA model.
#'
#' The lower part of the output shows additional information on the estimation:
#' \describe{
#' \item{Adjustment}{use of SEATS or X11}
#' \item{ARIMA}{structure of the seasonal ARIMA model}
#' \item{Obs.}{number of observations}
#' \item{Transform}{prior transformation}
#' \item{AICc, BIC}{value of the information criterion (lower is better)}
#' \item{QS}{test for seasonality in the final series; null hypothesis: no
#' seasonality in final; signif. codes are shown if the null hypothesis is
#' rejected. QS statistics for more series (e.g., the original series) can be
#' extracted with \code{\link{qs}}.}
#' \item{Box-Ljung}{test for residual autocorrelation; null hypothesis: no
#' autocorrelation in residuals; signif. codes are shown if the null hypothesis
#' is rejected. The test statistic is the result of 
#' \code{Box.test(resid(m), lag = 24, type = "Ljung")}}
#' \item{Shapiro}{test for normality of the residuals; null hypothesis: normal
#' distribution of the residuals; signif. codes are shown if the null
#' hypothesis is rejected. The test statistic is the result of 
#' \code{shapiro.test(resid(m))}}
#' }
#' @param object      an object of class \code{"seas"}, usually, a result of a 
#'                    call to \code{\link{seas}}.
#' @param x           an object of class \code{"summary.seas"}, usually, a result 
#'                    of a call to \code{summary.seas}.
#' @param digits      the number of significant digits to use when printing.
#' @param signif.stars logical. If \code{TRUE}, 'significance stars' are printed 
#'                    for each coefficient.
#' @param \dots       further arguments passed to or from other methods.
#' 
#' @return \code{summary.seas} returns a list containing the summary statistics 
#'   included in \code{object}, and computes the following additional 
#'   statistics:
#'   
#'   \item{coefficients}{a named matrix containing coefficients, standard 
#'   deviations, t-values and p-values}
#'   
#'   \item{transform}{character string with the type of intial transformation}
#'   
#'   The \code{print} method prints the summary output in a similar way as the
#'   method for \code{"lm"}.
#'   
#' @examples
#' \dontrun{
#' m <- seas(AirPassengers)
#' summary(m)  
#' }
#' @method summary seas
#' @export
summary.seas <- function(object, ...){
  # build output on top of the input
  z <- object
  
  # coefficents matrix
  if (!is.null(coef(object))){
    est  <- coef(object)
    se   <- object$est$se
    tval <- est/se
    pval <- 2 * pnorm(-abs(tval))
    
    z$coefficients <- cbind(est, se, tval, 
                            2 * pnorm(abs(tval), lower.tail = FALSE))
    dimnames(z$coefficients) <- list(names(est),
                                     c("Estimate", "Std. Error", "z value", 
                                       "Pr(>|z|)"))
  }
  
  z$resid <- residuals(object)
  z$qs <- qs(object)
  z$transform.function <- transformfunction(object)
  
  class(z) <- "summary.seas"
  z
}



#' @method print summary.seas
#' @export
#' @rdname summary.seas
print.summary.seas <- function (x, digits = max(3, getOption("digits") - 3), 
                              signif.stars = getOption("show.signif.stars"), ...) {
  
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n",
      sep = "")
  
  if (is.null(coef(x))) {
    cat("\nNo Coefficients\n")
  } else {
    cat("\nCoefficients:\n")
    coefs <- coef(x)
    printCoefmat(coefs, digits = digits, signif.stars = signif.stars,
                 na.print = "NA")
  }
  
  cat("\n")
  if (!is.null(x$spc$seats)){
    cat("SEATS adj.")
  } else if (!is.null(x$spc$x11)){
    cat("X11 adj.")
  } else {
    cat("No adj.")
  }
  
  cat("  ARIMA:", x$model$arima$model)

  cat("  Obs.:", formatC(x$lks['nobs'], format = "d"))
  cat("  Transform:", x$transform.function)
  cat("\nAICc:", formatC(x$lks['Aicc'], digits = digits))
  cat(", BIC:", formatC(x$lks['bic'], digits = digits))

  # QS Test
  qsv <- qs(x)[c('qssadj'), ]
  qsstars <- symnum(as.numeric(qsv['p-val']), 
                    corr = FALSE, na = FALSE, legend = FALSE,
                    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                    symbols = c("***", "**", "*", ".", " "))
  cat("  QS (no seasonality in final):", formatC(as.numeric(qsv['qs']), digits = digits)," ", qsstars, sep = "")
  
  if (!is.null(x$resid)){
    # Box Ljung Test
    bltest <- Box.test(x$resid, lag = 24, type = "Ljung")
    blstars <- symnum(bltest$p.value, 
                      corr = FALSE, na = FALSE, legend = FALSE,
                      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                      symbols = c("***", "**", "*", ".", " "))
    cat("\nBox-Ljung (no autocorr.):", 
        formatC(bltest$statistic, digits = digits), blstars)
    
    # Normality
    swtest <- shapiro.test(x$resid)
    swstars <- symnum(swtest$p.value, 
                      corr = FALSE, na = FALSE, legend = FALSE,
                      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                      symbols = c("***", "**", "*", ".", " "))
    cat(" Shapiro (normality):", formatC(swtest$statistic, digits = digits), swstars)
  }
  if (!is.null(x$err)){
    cat("\n")
    print(x$err)
  }
  cat("\n")
  invisible(x)
}
