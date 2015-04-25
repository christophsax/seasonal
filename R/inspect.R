#' Interactively Inspect a Seasonal Adjustment Model
#' 
#' Interactively inspect a \code{"seas"} object. The goal of \code{inspect} is 
#' to summarize all relevant options, plots and statistics that should be 
#' usually considered.
#' 
#' The \code{inspect} function opens an interactive window that allows for the 
#' manipulation of a number of arguments. It offers several views to analyze the
#' series graphically. With each change, the adjustment process and the 
#' visualizations are recalculated.
#' 
#' Summary statistics are shown in the first tab. The last tab offers access to
#' all series that can be produced with X-13.
#' 
#' The views in \code{inspect} may be customized via the \code{fun} argument.
#' One or several plot functions may be supplied. The plot functions should have
#' a \code{"seas"} object as their only argument. Several functions must be 
#' wrapped in a list (see examples).
#' 
#' @param x an object of class \code{"seas"}. 
#' @param fun a function or a list of functions (see details)
#' @param ... further arguments, passed on to 
#'   \code{\link[shiny]{runApp}}. (The \code{launch.browser} argument from 
#'   version 0.8 can be still used that way)
#'   
#' @seealso \code{\link{seas}} for the main function of seasonal.
#'   
#' @examples
#' \dontrun{
#' 
#' m <- seas(AirPassengers)
#' 
#' inspect(m)
#' 
#' 
#' ### customizing inspect
#' 
#' # a single function
#' fc <- function(m){
#'   ts.plot(series(m, "fct", verbose = FALSE))
#' }
#' inspect(m, fc)
#' 
#' # more than one function collected in a list
#' myfun <- list()
#' myfun[['Spectum X-13']] <- function(m){
#'   plot(series(m, "spectrum.specorig", verbose = FALSE)[,-1], t = "l")
#' }
#' myfun[['Spectum R']] <- function(m){
#'   spectrum(diff(log(AirPassengers)), method = "ar")
#' }
#' inspect(m, myfun)
#' 
#' # and a bonus example
#' spaghetti <- function(m, back = 10){
#' ser <- original(m)
#' tx <- time(ser)[(length(ser) - back):length(ser)]
#' z <- NULL
#' for (txi in tx){
#'   assign(as.character(m$call$x), window(ser, end = txi))
#'   z <- cbind(z, final(eval(m$call)))
#' }
#' ts.plot(window(z, start = time(ser)[(length(ser) - back- 15)]), 
#'         col = rainbow(back + 1))
#' }
#' inspect(m, spaghetti)
#' 
#' }
#' @export
inspect <- function(x, fun = NULL, check.version = TRUE, quiet=TRUE, ...){ 

  if(getRversion() < "3.2.0" && check.version) { 
    stop("You need to have at least R version 3.2.0 installed to run inspect smoothly. To ignore this test, use the 'check.version = FALSE' argument.")
  }

  if (!requireNamespace("shiny", quietly = TRUE)){
    stop("the inspect function depends on the 'shiny' package. To install it from CRAN, type: \n\n  install.packages('shiny')\n ")
  }

  if (packageVersion("shiny") < "0.11.1" && check.version){
    stop("You need to have at least shiny version 0.11.1 installed to run inspect smoothly. To ignore this test, use the 'check.version = FALSE' argument. To update shiny from CRAN, type:  \n\n  install.packages('shiny')\n")
  }

  if (!inherits(x, "seas")){
    stop("first argument must be of class 'seas'")
  }
  
  inter.session.file <- paste0(gsub("[a-zA-Z0-9]+$", "", tempdir()), "intersession.RData")
  init.model <- x
  save(init.model, fun, file = inter.session.file)
  z <- shiny::runApp(system.file("inspect", package="seasonal"), quiet = quiet, ...)
  file.remove(inter.session.file)
  return(z)
}
