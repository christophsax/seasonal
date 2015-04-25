# modified print method for website
PrintSummarySeas <- function (x, digits = max(3, getOption("digits") - 3), 
                              signif.stars = getOption("show.signif.stars"), ...) {
  
  # cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n",
  #     sep = "")
  
  if (is.null(coef(x))) {
    cat("No Coefficients\n")
  } else {
    cat("Coefficients:\n")
    coefs <- coef(x)
    printCoefmat(coefs, digits = digits, signif.stars = signif.stars,
                 na.print = "NA")
  }
  
  cat("\n")
  if (!is.null(x$spc$seats)){
    cat("SEATS adj.")
  }
  if (!is.null(x$spc$x11)){
    cat("X11 adj.")
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
  if (is.null(x$err)){
    cat("\n")
    print(x$err)
  }
  invisible(x)
}