#' Check the installation of X-13ARIMA-SEATS
#' 
#' The function checks the installation of the binary executables of
#' X-13ARIMA-SEATS. For installation details, consider Section 2 of the package
#' vignette: \code{vignette("seas")}
#' 
#' @param fail  logical, wether an error should interrupt the process. If 
#' \code{FALSE}, only a message will be returned.
#' @param confirmation  logical, a message is returned if everything works.
#'   
#' @examples
#' \dontrun{
#' old.path <- Sys.getenv("X13_PATH")
#' Sys.setenv(X13_PATH = "")  # its broken now
#' checkX13()
#' 
#' Sys.setenv(X13_PATH = old.path)  # fix it (provided it worked in the first place)
#' checkX13()
#' }
#' 
#' @export
checkX13 <- function(fail = FALSE, confirmation = TRUE){
  no.path.message <- "No path to the binary executable of X-13ARIMA-SEATS specified.
  \nFor installation details, consider Section 2 of the package vignette:\n  vignette(\"seas\")\n"
  env.path <- Sys.getenv("X13_PATH")
  if (env.path == ""){
    if (fail){
      message(no.path.message)
      stop("Process terminated")
    } else {
      packageStartupMessage(no.path.message)
    }
  } else {
    # platform dependent binaries
    if (.Platform$OS.type == "windows"){
      x13.bin <- file.path(env.path, "x13as.exe")
    } else {
      x13.bin <- file.path(env.path, "x13as")
    }
    
    no.file.message <- paste("Binary executable file", x13.bin, "not found.
    \nFor installation details, consider Section 2 of the package vignette:\n  vignette(\"seas\")\n")
    
    if (!file.exists(x13.bin)){
      if (fail){
        message(no.file.message)
        stop("Process terminated")
      } else {
        packageStartupMessage(no.file.message)
      }
    } else {
      if (confirmation){
        message("Congratulations! The X13_PATH is correctly specified and the binary\nexecutable file has been found.")
      }
    }
  }
  
}
