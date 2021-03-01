#' Summary of a X13-ARIMA-SEATS seasonal adjustment
#'
#' Like the corresponding method for `"lm"` objects, the method for
#' `"seas"` objects returns the estimated coefficients, its standard errors,
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
#' extracted with [qs()].}
#' \item{Box-Ljung}{test for residual autocorrelation; null hypothesis: no
#' autocorrelation in residuals; signif. codes are shown if the null hypothesis
#' is rejected. The test statistic is the result of
#' `Box.test(resid(m), lag = 24, type = "Ljung")`}
#' \item{Shapiro}{test for normality of the residuals; null hypothesis: normal
#' distribution of the residuals; signif. codes are shown if the null
#' hypothesis is rejected. The test statistic is the result of
#' `shapiro.test(resid(m))`}
#' }
#' @param object      an object of class `"seas"`, usually, a result of a
#'                    call to [seas()].
#' @param stats  (experimental) character vector, additional stat to be shown in the `summary` output.
#'   function. For a list of all possible values, see the [udg()]
#'   function. If a value is not present, it will be ignored. Values can be
#'   specified via `options`. See examples.
#' @param x           an object of class `"summary.seas"`, usually, a result
#'                    of a call to `summary.seas`.
#' @param digits      the number of significant digits to use when printing.
#' @param signif.stars logical. If `TRUE`, 'significance stars' are printed
#'                    for each coefficient.
#' @param \dots       further arguments passed to or from other methods.
#' @return `summary.seas` returns a list containing the summary statistics
#'   included in `object`, and computes the following additional
#'   statistics:
#'
#'   \item{coefficients}{a named matrix containing coefficients, standard
#'   deviations, t-values and p-values}
#'
#'   \item{transform}{character string with the type of initial transformation}
#'
#'   The `print` method prints the summary output in a similar way as the
#'   method for `"lm"`.
#'
#' @examples
#' \dontrun{
#' m <- seas(AirPassengers)
#' summary(m)
#'
#' ### user defined stats from the udg function
#' # (experimental, see ?udg)
#'
#' # also show some M quality statistics for X11 in summary
#' options(seas.stats = c("f3.m01", "f3.m02", "f3.m03", "f3.m04"))
#' summary(seas(AirPassengers, x11 = ""))
#'
#' # this does not affect the SEATS output
#' summary(seas(AirPassengers))
#'
#' # reset to default
#' options(seas.stats = NULL)
#' }
#' @method summary seas
#' @export
summary.seas <- function(object, stats = getOption("seas.stats"), ...){
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
  z$transform.function <- transformfunction(object)
  z$qsv <- if ('qssadj' %in% rownames(qs(object))) {
    qs(object)[c('qssadj'), ]
  } else {
    qs(object)[c('qssori'), ]
  }
  z$stats <- stats

  z$nobs <- nobs(object)
  z$aicc <- unname(udg(object, "aicc", fail = FALSE))
  z$bic <- BIC(object)

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
  cat("  Obs.:", formatC(x$nobs, format = "d"))
  cat("  Transform:", x$transform.function)
  cat("\nAICc:", formatC(x$aicc, digits = digits))
  cat(", BIC:", formatC(x$bic, digits = digits))

  # QS Test
  qsv <- x$qsv
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

  if (!is.null(x$stats)){
    x1 <- x; class(x1) <- "seas"  # reclassify, so udg() can be used on it
    uu0 <- udg(x1, x1$stats, fail = FALSE, simplify = FALSE)
      if (length(uu0) > 0){
        # format numeric
        cformat <- function(x){
          if (inherits(x, "numeric")) {
            prettyNum(x, digits = digits)
          } else {
            x
          }
        }

        # make multi element vetors single element, to char vector
        uu0 <- sapply(uu0, function(e) paste(cformat(e), collapse = ", "))

        stats <- paste0(names(uu0), ": ", uu0, " ")
        cat("\n")
        cat(stats, fill = 0.8 * getOption("width"))
      }
  }

  if (!is.null(x$err)){
    # cat("\n")
    print(x$err)
  }
  cat("\n")
  invisible(x)
}
