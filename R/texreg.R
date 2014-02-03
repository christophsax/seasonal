extract.seas <- function (model, include.diff = TRUE, include.sdiff = TRUE, 
                          include.aicc = TRUE, include.trans = TRUE,
                          include.nobs = TRUE, ...) 
{
  s <- summary(model, ...)
  names <- rownames(s$coef)
  co <- s$coef[, 1]
  se <- s$coef[, 2]
  pval <- s$coef[, 4]
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.diff == TRUE) {
    gof <- c(gof, arimamodel(model)[2])
    gof.names <- c(gof.names, "D-Nonseasonal-01")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.sdiff == TRUE) {
    gof <- c(gof, arimamodel(model)[5])
    gof.names <- c(gof.names, "D-Seasonal-12")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.trans == TRUE) {
    if (s$transform.function == "log"){
      tf <- 1
    } else {
      tf <- 0
    }
    gof <- c(gof, tf)
    gof.names <- c(gof.names, "log-transform")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.aicc == TRUE) {
    gof <- c(gof, s$lkstats['Aicc'])
    gof.names <- c(gof.names, "AICc")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    gof <- c(gof, nobs(model))
    gof.names <- c(gof.names, "Num. obs.")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  tr <- createTexreg(coef.names = names, coef = co, se = se, pvalues = pval, 
                     gof.names = gof.names, gof = gof, 
                     gof.decimal = gof.decimal)
  return(tr)
}


require(texreg, quietly = TRUE)

setClass("seas")

#' @export
#' @import texreg
setMethod("extract", signature = className("seas", "seasonal"), 
          definition = extract.seas)

