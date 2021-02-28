#' Interactively Modify a Seasonal Adjustment Model
#' 
#' Interactively modify a `"seas"` object. The goal of `view` is 
#' to summarize all relevant options, plots and statistics of a 
#' seasonal adjustment model. The `view` function in the \pkg{seasonal} 
#' package imports the identical [seasonalview::view()] function from 
#' the \pkg{seasonalview} package, so there is no need to explicitly load the 
#' \pkg{seasonalview} package.
#' 
#' Frequently used options can be modified using the drop down selectors in the
#' upper left box. Each change will result in a re-estimation of the seasonal
#' adjustment model. The R-call, the X-13 call, the graphical output and the 
#' summary are updated accordingly.
#'
#' Alternatively, the R call can be modified manually in the lower left box.
#' Click 'Run Call' to re-estimate the model and to adjust the option selectors,
#' the graphical output, and the summary. With the 'To console' button, 
#' the GUI is closed and the call is imported to R. The 'Static' button
#' substitutes automatic procedures by the automatically chosen 
#' spec-argument options, in the same way as the [seasonal::static()] 
#' function.
#'
#' If you are familiar with the X-13 spec syntax, you can modify the X-13 call,
#' with the same consequences as when modifying the R call.
#'
#' The lower right panel shows the summary, as described in the help page of
#' [seasonal::summary.seas()]. The 'X-13 output' button opens the complete 
#' output of X-13 in a separate tab or window.
#' 
#' If you have the x13story package installed (not yet on CRAN, see references), 
#' you can call the function with the `story` argument. This will render 
#' an R Markdown document and produce a *story* on seasonal adjustment that 
#' can be manipulated interactively.
#' 
#' @param x an object of class `"seas"`. 
#' @param story character, local file path or URL to an `".Rmd"` file. 
#' @param quiet logical, if `TRUE` (default), error messages from calls in 
#'   `view` are not shown in the console.
#' @param ... arguments passed to [shiny::runApp()]. E.g., for selecting 
#'   if the GUI should open in the browser or in the RStudio viewer pane.
#' 
#' @references Seasonal vignette with a more detailed description: 
#'   <http://www.seasonal.website/seasonal.html>
#' 
#'   Development version of the x13story package: 
#'   <https://github.com/christophsax/x13story>
#' 
#' @return `view` returns an object of class `"seas"`, the modified 
#' model; or `NULL`, if the `story` argument is supplied.
#'
#' @examples
#' \dontrun{
#' 
#' m <- seas(AirPassengers)
#' view(m)
#' 
#' # store the model after closing the GUI, for further processing in R
#' m.upd <- view(m)  
#' }
#' @export
view <- function(x = NULL, story = NULL, quiet = TRUE, ...){ 
  z <- try(seasonalview::view, silent = TRUE)  # is seasonalview installed?
  if (inherits(z, "try-error")) {
    stop("'seasonalview' required to run the graphical user interface.\n\nTo install from CRAN, use:\n\n    install.packages(\"seasonalview\")", call. = FALSE)
  }

  # temp workaround until next version of seasonalview is on CRAN 
  # (current version allways picks frame 1, rather than the one of 
  # seasonalview::view())
  if (!is.null(x)) .model.passed.to.shiny <- x
  if (!is.null(story)) .story.filename.passed.to.shiny <- story

  seasonalview::view(x = x, story = story, quiet = quiet, ...)
}



