#' Interactively Modify a Seasonal Adjustment Model
#' 
#' Interactively modify a \code{"seas"} object. The goal of \code{view} is 
#' to summarize all relevant options, plots and statistics of a 
#' seasonal adjustment model. The \code{view} function in the \pkg{seasonal} package 
#' re-directs to the \code{\link[seasonalview]{view}} function in the \pkg{seasonalview} package.
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
#' spec-argument options, in the same way as the \code{\link[seasonal]{static}} 
#' function.
#'
#' If you are familiar with the X-13 spec syntax, you can modify the X-13 call,
#' with the same consequences as when modifying the R call.
#'
#' The lower right panel shows the summary, as described in the help page of
#' \code{\link[seasonal]{summary.seas}}. The 'X-13 output' button opens the complete 
#' output of X-13 in a separate tab or window.
#' 
#' If you have the x13story package installed (not yet on CRAN, see references), 
#' you can call the function with the \code{story} argument. This will render 
#' an R Markdown document and produce a \emph{story} on seasonal adjustment that 
#' can be manipulated interactively.
#' 
#' @param x an object of class \code{"seas"}. 
#' @param story character, local file path or URL to an \code{".Rmd"} file. 
#' @param quiet logical, if \code{TRUE} (default), error messages from calls in 
#'   \code{view} are not shown in the console.
#' @param ... arguments passed to \code{\link[shiny]{runApp}}. E.g., for selecting 
#'   if the GUI should open in the browser or in the RStudio viewer pane.
#' 
#' @references Seasonal vignette with a more detailed description: 
#'   \url{http://www.seasonal.website/seasonal.html}
#' 
#'   Development version of the x13story package: 
#'   \url{https://github.com/christophsax/x13story}
#' 
#' @return \code{view} returns an object of class \code{"seas"}, the modified 
#' model; or \code{NULL}, if the \code{story} argument is supplied.
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

  z <- try(seasonalview::view, silent = TRUE)  # can it be found?
  if (inherits(z, "try-error")) {
    stop("The 'seasonalview' package is needed to display \nthe graphical user interface. To install from CRAN, use:\n\n    install.packages(\"seasonalview\")", call. = FALSE)
  }

  if (!is.null(story)){

    if (!grepl("\\.Rmd", story, ignore.case = TRUE)){
      stop("File must have rmarkdown extension (.Rmd)")
    }

    # auto download from the internet
    if (grepl("^http", story)){
      tfile <- tempfile(fileext = ".Rmd")
      download.file(story, tfile)
      story <- tfile
    }

    .story.filename.passed.to.shiny <- normalizePath(story)

    wd <- system.file("app", package = "seasonalview")
    shiny::runApp(wd, quiet = quiet)
    return(NULL)
  } 

  if (!inherits(x, "seas")){
    stop("first argument must be of class 'seas'")
  }

  .model.passed.to.shiny <- x

  cat("Press ESC (or Ctrl-C) to get back to the R session\n")

  wd <- system.file("app", package = "seasonalview")
  shiny::runApp(wd, quiet = quiet, ...)
}



