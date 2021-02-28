#' Display X-13ARIMA-SEATS Output
#' 
#' The `out` function shows the full content of the X-13ARIMA-SEATS output 
#' in the browser. If you want to use a specific statistic in R, the 
#' [udg()] function is preferable.
#' 
#' To keep the size of `"seas"` objects small, `seas` does not save 
#' the output by default. Instead, `out` re-evaluates the model.
#' 
#' @param x an object of class `"seas"`.
#' @param browser browser to be used, passed on to 
#'   [browseURL()].
#' @param ... additional spec-arguments options sent to X-13ARIMA-SEATS during 
#'   re-evaluation, passed to [update()].
#'   
#' @return displays the output as a side effect.
#'   
#' @seealso [seas()] for the main function of seasonal.
#'   
#' @references Vignette with a more detailed description: 
#'   <http://www.seasonal.website/seasonal.html>
#'   
#'   Comprehensive list of R examples from the X-13ARIMA-SEATS manual: 
#'   <http://www.seasonal.website/examples.html>
#'   
#'   
#'   Official X-13ARIMA-SEATS manual: 
#'   <https://www.census.gov/ts/x13as/docX13ASHTML.pdf>
#'   
#' @export
#' 
#' @examples
#' 
#' \dontrun{
#' m <- seas(AirPassengers) 
#' out(m) 
#' # customizing the output with additional elements
#' out(m, automdl.print = "autochoicemdl")
#' 
#' }
out <- function(x, browser = getOption("browser"), ...){
  if (getOption("htmlmode") == 0){
    stop("Non HTML mode is not supported anymore. \nPlease install x13binary to get HTML binaries:\n\n  install.packages('x13binary')")
  }
  
  # clean remainings from previous out runs
  unlink(list.files(tempdir(), pattern = "^x13out", full.names = TRUE), recursive = TRUE)
  
  m <- update(x, out = TRUE, ...)
  browseURL(url = file.path(m$wdir, "iofile.html"), browser = browser)
}





