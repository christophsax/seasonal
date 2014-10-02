#' Check the installation of X-13ARIMA-SEATS
#' 
#' Check the installation of the binary executables of X-13ARIMA-SEATS. For
#' installation details, consider Section 2 of the package vignette:
#' \code{vignette("seas")}
#' 
#' @param fail  logical, whether an error should interrupt the process. If 
#'   \code{FALSE}, a message is returned.
#' @param fullcheck  logical, whether a full test should be performed. Runs
#'   \code{Testairline.spc} (which is shiped with X-13ARIMA-SEATS) to test the
#'   working of the binaries. Returns a message.
#' @param htmlcheck  logical, whether the presence of the the HTML version of 
#'   X-13 should be checked.
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
checkX13 <- function(fail = FALSE, fullcheck = TRUE, htmlcheck = TRUE){
  ### check path
  no.path.message <- "No path to the binary executable of X-13 specified.
  \nFor installation details, consider Section 2 of the package vignette:\n  http://cran.r-project.org/web/packages/seasonal/vignettes/seas.pdf\n"
  env.path <- Sys.getenv("X13_PATH")
  if (env.path == ""){
    if (fail){
      message(no.path.message)
      stop("Process terminated")
    } else {
      packageStartupMessage(no.path.message)
      return(invisible(NULL))
    }
  }
  
  ### check existence of binaries 
  # platform dependent binaries
  if (.Platform$OS.type == "windows"){    
    x13.bin <- file.path(env.path, "x13as.exe")
    x13.bin.html <- file.path(env.path, "x13ashtml.exe")
  } else {
    x13.bin <- file.path(env.path, "x13as")
    # ignore case on unix to avoid problems with different binary names
    fl <- list.files(env.path)
    fn <- fl[grepl("^x13ashtml$", fl, ignore.case = TRUE)]
    if (length(fn) > 0){
      x13.bin.html <- file.path(env.path, fn)
    } else {
      x13.bin.html <- file.path(env.path, "x13ashtml")
    }
  }
  no.file.message <- paste("Binary executable file", x13.bin, "or", x13.bin.html, "not found.\nFor installation details, consider Section 2 of the package vignette:\n  vignette(\"seas\")\n")
  
  if (!(file.exists(x13.bin) | file.exists(x13.bin.html))){
    if (fail){
      message(no.file.message)
      stop("Process terminated")
    } else {
      packageStartupMessage(no.file.message)
      return(invisible(NULL))
    }
  }
  
  ### set html mode
  if (file.exists(file.path(x13.bin.html))){
    options(htmlmode = 1)
  } else {
    options(htmlmode = 0)
  }
  
  ### full working test
  if (fullcheck){
    # temporary working dir and filenames
    wdir <- file.path(tempdir(), "x13")
    if (!file.exists(wdir)){
      dir.create(wdir)
    }
    file.remove(list.files(wdir, full.names = TRUE))
    
    testfile <- file.path(path.package("seasonal"), "tests", "Testairline.spc")
    file.copy(testfile, wdir)
    run_x13(file.path(wdir, "Testairline"), out = TRUE)
    if (file.exists(file.path(wdir, "Testairline.out")) | file.exists(file.path(wdir, "Testairline.html"))){
      message("Congratulations! 'seasonal' should work fine!
              - the X13_PATH is correctly specified
              - the binary executable file has been found
              - a test run has been successful")
      if (file.exists(file.path(wdir, "Testairline.html"))){
        message("              - HTML output is produced.")
      }
    } else {
      message("There is something wrong with your X-13ARIMA-SEATS binaries:
              - the X13_PATH is correctly specified
              - the binary executable file has been found
              - however, a test run has failed")
    }
  }
  
  
  ### check HTML mode
  if (htmlcheck){
    if ((getOption("htmlmode") == 0)){
      if (.Platform$OS.type == "windows"){
        packageStartupMessage("\nseasonal now supports the HTML version of X13, which offers a more\naccessible output via the out() function. For best user experience, \ndownload the HTML version from:",
                              "\n\n  http://www.census.gov/srd/www/x13as/x13down_pc.html\n\n",
                              "and copy x13ashtml.exe to:\n\n",
                              "  ", env.path)
      } else {
        packageStartupMessage("\nseasonal now supports the HTML version of X13, which offers a more \naccessible output via the out() function. For best user experience, \ncopy the binary executables of x13ashtml to:\n\n",
                              "  ", env.path)
      }
    }
  }
  
}

