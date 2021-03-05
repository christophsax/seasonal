#' Seasonal Adjustment Plots
#'
#' Functions to graphically analyze a `"seas"` object.
#'
#' `plot` calls the plot method for class `"seas"`. It plots the
#' adjusted and unadjusted series, as well as the outliers. Optionally draws the
#' trend series.
#'
#' `residplot` plots the residuals and the outliers.
#'
#' `monthplot` calls the monthplot method for class `"seas"`. It plot
#' the seasonal and SI component periodwise. Despite its name, `monthplot`
#' can be used for series of all frequencies.
#'
#' @param x  an object of class `"seas"`, usually, a result of a call to
#'   [seas()].
#' @param outliers   logical, should the outliers be drawn.
#' @param trend      logical, should the trend be drawn.
#' @param choice     character string, `"seasonal"` (default) or
#'   `"irregular"`.
#' @param main    character string, title of the graph.
#' @param xlab    character string, title for the x axis.
#' @param ylab    character string, title for the y axis.
#' @param transform   character string, optionally transform the data to period to period  `"PC"` or year to year`"PCY"` percentage change rates.
#' @param \dots   further arguments passed to the plotting functions.
#'
#' @return All plot functions return a plot as their side effect.
#'
#' @seealso [seas()], for the main function.
#' @seealso [udg()], for diagnostical statistics.
#'
#' @references Vignette with a more detailed description:
#'   <http://www.seasonal.website/seasonal.html>
#'
#'   Comprehensive list of R examples from the X-13ARIMA-SEATS manual:
#'   <http://www.seasonal.website/examples.html>
#'
#'
#'
#'   Official X-13ARIMA-SEATS manual:
#'   <https://www.census.gov/ts/x13as/docX13ASHTML.pdf>
#'
#' @export
#' @method plot seas
#'
#' @examples
#' \donttest{
#'
#' m <- seas(AirPassengers)
#'
#' plot(m)
#' plot(m, outliers = FALSE)
#' plot(m, trend = TRUE)
#'
#' residplot(m)
#' residplot(m, outliers = FALSE)
#'
#' monthplot(m)
#'
#' # use standard R functions to analyze "seas" models
#' pacf(resid(m))
#' spectrum(diff(resid(m)))
#' plot(density(resid(m)))
#' qqnorm(resid(m))
#' }
plot.seas <- function(x, outliers = TRUE, trend = FALSE,
                      main = "Original and Adjusted Series",
                      xlab = "Time", ylab = "",
                      transform = c("none", "PC", "PCY"),
                      ...){

  transform <- match.arg(transform)

  orignalx <- original(x)
  finalx <- final(x)

  if (transform == "PC"){
    orignalx <- (orignalx - lag(orignalx, -1)) / lag(orignalx, -1)
    if (!is.null(final(x))){
      finalx <- (finalx - lag(finalx, -1)) / lag(finalx, -1)
    }
    if (main != ""){
      main <- paste(main, "(PC)")
    }
  }
  if (transform == "PCY"){
    fr <- frequency(orignalx)
    orignalx <- (orignalx - lag(orignalx, -fr)) / lag(orignalx, -fr)
    if (!is.null(final(x))){
      finalx <- (finalx - lag(finalx, -fr)) / lag(finalx, -fr)
    }
    if (main != ""){
      main <- paste(main, "(PCY)")
    }
  }

  plot(cbind(orignalx, finalx),
          col = c("black", "red"),
          lwd = c(1, 2),
          main = main, plot.type = "single",
          xlab = xlab,
          ylab = ylab, ...
  )

  if (identical(trend, TRUE)){
    trendx <- x$data[, 'trend']
    if (transform == "PC") trendx <- (lag(trendx, -1) - trendx) / lag(trendx, -1)
    if (transform == "PCY") trendx <- (lag(trendx, -fr) - trendx) / lag(trendx, -fr)
    lines(trendx, col = "blue", lty = "dashed")
  }

  if (identical(outliers, TRUE) && (!is.null(final(x)))){
    ol.ts <- outlier(x)
    sym.ts <- ol.ts
    sym.ts[!is.na(sym.ts)] <- 3
    points(final(x), pch=as.numeric(sym.ts))
    text(final(x), labels=ol.ts, pos=3, cex=0.75, offset=0.4)
  }
}

#' @rdname plot.seas
#' @export
residplot <- function(x, outliers = TRUE, main = "residuals of regARIMA",
                      xlab = "Time", ylab = "", ...){
  if(is.null(resid(x))){
    stop("no residuals to plot")
  }

  plot(resid(x), main = main, ylab = ylab, xlab = xlab, plot.type = "single", ...)

  if (identical(outliers, TRUE)){
    ol.ts <- outlier(x)
    sym.ts <- ol.ts
    sym.ts[!is.na(sym.ts)] <- 3
    points(resid(x), pch=as.numeric(sym.ts))
    text(resid(x), labels=ol.ts, pos=3, cex=0.75, offset=0.4)
  }
}


#' @rdname plot.seas
#' @export
#' @import stats
#' @method monthplot seas
monthplot.seas <- function(x, choice = c("seasonal", "irregular"), main, ...){

  choice <- match.arg(choice)
  if (choice == "seasonal"){
    if(!'seasonal' %in% colnames(x$data)){
      stop("model has no seasonal component")
    }
    monthplot(x$data[,'seasonal'], ylab = "", lwd = 2, col = "red", ...)
    if (missing("main")) main <- "Seasonal Component, SI Ratio"
    title(main = main)
    monthplot(siratio(x), col = "blue", type = "h", add = TRUE)
  }
  if (choice == "irregular"){
    if(!'irregular' %in% colnames(x$data)){
      stop("model has no irregular component")
    }
    monthplot(x$data[,'irregular'], ylab = "", main = "Irregular Component")
    if (missing("main")) main <- "Irregular Component"
    title(main = main)
  }
}


siratio <- function(x){
  stopifnot(inherits(x, "seas"))

  tf <- transformfunction(x)

  # if irregular or seasonal are not returned, set them to 0 or 1
  # (depending on transformfunction())
  if ('irregular' %in% colnames(x$data)){
    irregular <- x$data[, 'irregular']
  } else {
    irregular <- x$data[, 'final']
    irregular[] <- if (tf == "log") {1} else {0}

  }
  if ('seasonal' %in% colnames(x$data)){
    seasonal <- x$data[, 'seasonal']
  } else {
    seasonal <- x$data[, 'final']
    seasonal[] <- if (tf == "log") {1} else {0}
  }

  if (tf == "log"){
    z <- irregular * seasonal
  } else {
    z <- irregular + seasonal
  }
  na.omit(z)
}

