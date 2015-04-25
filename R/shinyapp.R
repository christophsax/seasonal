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

format_seascall <- function(x){
  stopifnot(inherits(x, "call"))
  if (deparse(x[[1]]) != "seas"){
    stop('format_seascall() can only be applied to calls of the seas()')
  }
  # call in which all arguments are specified by their full names
  x <- match.call(definition = seas, x)
  xl <- as.list(x)
  z <- list()
  for (i in 2:length(xl)){
    z[[i - 1]] <- paste(names(xl)[i], "=", deparse(xl[[i]]))
  }
  argstr <- do.call(paste, c(z, sep = ",\n"))
  z <- paste("seas(", argstr, ")", sep = "\n")
  z
}



IsCallSave <- function(cl){
  fun.allowed <- c("window", "genhol", "ts", "seas", "c", "list", "-", "+", "*", "/")


  # does the call have subcalls, if not, it is save
  is.scl <- sapply(cl, class) == "call"
  if (all(!(is.scl))){
    return(TRUE)
  }
  # if it has subcalls, they must be further analyzed
  scl <- cl[is.scl]
  scl.names <- unlist(lapply(scl, function(e) as.character(e[[1]])))
  if (!all(scl.names %in% fun.allowed)){
    not.allowed <- scl.names[!scl.names %in% fun.allowed]
    write(paste("### Attempt to call forbidden function:", not.allowed), file = "www/log555/unsave.txt", append = TRUE)
    return(FALSE)
  } else {
    rr <- unlist(lapply(scl, IsCallSave))
    if (all(rr)){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}


EvalOrFail <- function(cstr){
  pcl <- try(parse(text = cstr))
  if (inherits(pcl, "try-error")){
    z <- "Invalid call."
    class(z) <- "try-error"
  } else if (inherits(try(as.call(pcl)[[1]][[1]], silent = TRUE), "try-error")){
    z <- "Invalid call."
    class(z) <- "try-error"
  } else if (as.call(pcl)[[1]][[1]] != "seas"){
    z <- "Only calls to seas() are allowed."
    class(z) <- "try-error"
  } else if (!IsCallSave(as.call(pcl)[[1]])){
    write(cstr, file = "www/log555/unsave.txt", append = TRUE)
    z <- "Call is not save and thus not allowed."
    class(z) <- "try-error"
  } else {
    z <- try(eval(pcl), silent = TRUE)
  }
  z
}

