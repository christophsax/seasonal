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
#'   <https://www2.census.gov/software/x-13arima-seats/x13as/windows/documentation/docx13as.pdf>
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
  unlink(list.files(tempdir(), pattern = "^x13", full.names = TRUE), recursive = TRUE)

  if (inherits(x, "seas")) {
    # use exported update.seas
    m <- update(x, out = TRUE, ...)
    iofile <- m$iofile
  } else if (inherits(x, "seas_multi")) {
    if (!("composite" %in% names(x))) {
      stop("Not a composite object. Use out() on individual elments of `x` instead.")
    }
    # use non-exported pseudeo method
    if (length(list(...)) > 0) message("... are ignored when out() is applied on 'composite' output.")
    m <- update_seas_multi(x, out = TRUE)
    iofile <- m$composite$iofile
  } else {
    stop("class of x is not supported")
  }

  out_file <- normalizePath(paste0(iofile, ".html"), mustWork = TRUE)

  if (!is.null(browser)) browseURL(url = out_file, browser = browser)
  invisible(out_file)
}



