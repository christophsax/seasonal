
#' @export
robust.seas <- function(x, xreg = NULL, xtrans = NULL, seats.noadmiss = "yes", 
                        transform.function = "auto", 
                        regression.aictest = c("td", "easter"), 
                        outlier = "", automdl = "", na.action = na.omit, 
                        out = FALSE, dir = NULL, ..., list = NULL){

  if (is.null(list)) {
      list <- list(...)
  }

  robust.seas.call <- match.call()

  try_with <- function(...){
    list <- c(list, list(...))
    z <- try(seas(x, xreg = xreg, xtrans = xtrans, seats.noadmiss = seats.noadmiss, 
    transform.function = transform.function, regression.aictest = regression.aictest, outlier = outlier, automdl = automdl, na.action = na.action, 
    out = FALSE, dir = NULL, list = list), silent = TRUE)

    if (!inherits(z, "try-error")) {
      z$call <- robust.seas.call
    }
    z
  }

  z <- try_with(); if (!inherits(z, "try-error")) return(z)

  if (tsp(x)[1] < 1000){
    message("adjusting invalid start year (+ 2000 years)")
    tsp(x)[1:2] <- tsp(x)[1:2] + 2000
    z <- try_with(regression.aictest = NULL); if (!inherits(z, "try-error")) return(z)
  }


  if (frequency(x) == 1){
    # a bunch of tweaks for annual series
    message("annual series: ensure SEATS and AIC testing are off")
    z <- try_with(seats = NULL, regression.aictest = NULL) 
    if (!inherits(z, "try-error")) return(z)

    message("annual series: turn off outlier detection")
    z <- try_with(seats = NULL, regression.aictest = NULL, outlier = NULL)
    if (!inherits(z, "try-error")) return(z)

    message("annual series: fix model to (110)")
    z <- try_with(seats = NULL, regression.aictest = NULL, outlier = NULL, arima.model = c(1, 1, 0))
    if (!inherits(z, "try-error")) return(z)
  }

  message("increasing maxiter")
  z <- try_with(estimate.maxiter = 10000)
  if (!inherits(z, "try-error")) return(z)

  message("fix model to (011)(011)")
  z <- try_with(estimate.maxiter = 10000, arima.model = c(0, 1, 1, 0, 1, 1))
  if (!inherits(z, "try-error")) return(z)

  message("turn off aic testing")
  z <- try_with(estimate.maxiter = 10000, arima.model = c(0, 1, 1, 0, 1, 1), 
                regression.aictest = NULL)
  if (!inherits(z, "try-error")) return(z)

  message("turn off outlier detection")
  z <- try_with(estimate.maxiter = 10000, arima.model = c(0, 1, 1, 0, 1, 1), 
                regression.aictest = NULL, outlier = NULL)

  if (!inherits(z, "try-error")) return(z)


  message("GRRRR, STILL AN ERROR... :-(")

  return(z)

}

