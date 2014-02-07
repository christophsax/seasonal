#' Check the installation of X-13ARIMA-SEATS
#' 
#' The function checks the installation of the binary executables of
#' X-13ARIMA-SEATS. For installation details, consider Section 2 of the package
#' vignette: \code{vignette("seas")}
#' 
#' @param fail  logical, wether an error should interrupt the process. If 
#' \code{FALSE}, a message is returned.
#' @param full  logical, wether a full test should be performed. Runs \code{Testairline.spc} (which is shiped with X-13ARIMA-SEATS) to test the working of your binaries. Returns a message.   
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
checkX13 <- function(fail = FALSE, full = TRUE){
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
      if (full){
        # perform a full working test
        
        # temporary working dir and filenames
        wdir <- file.path(tempdir(), "x13")
        if (!file.exists(wdir)){
          dir.create(wdir)
        }
        file.remove(list.files(wdir, full.names = TRUE))
        
        testfile <- file.path(path.package("seasonal"), "tests", "Testairline.spc")
        file.copy(testfile, wdir)
        run_x13(file.path(wdir, "Testairline"))
        if (file.exists(file.path(wdir, "Testairline.out"))){
          message("Congratulations! 'seasonal' should work fine!
  - the X13_PATH is correctly specified
  - the binary executable file has been found
  - a test run has been successful")
        } else {
          message("There is something wrong with your X-13ARIMA-SEATS binaries:
  - the X13_PATH is correctly specified
  - the binary executable file has been found
  - however, a test run has failed")
        }
      }
    }
  }
  
}
