.onLoad <- function(...){

  if (Sys.getenv("X13_PATH") == ""){
    default.path <- file.path(path.package("x13binary"), "bin")
    if (.Platform$OS.type == "windows"){    
      x13.bin.html <- file.path(default.path, "x13ashtml.exe")
    } else {
      x13.bin.html <- file.path(default.path, "x13ashtml")
    }

    if (file.exists(x13.bin.html)){
      Sys.setenv(X13_PATH = default.path)
      env.path = default.path
    }
  } else {
    packageStartupMessage(
      "The system variable 'X13_PATH' has been manually set to: ", 
      Sys.getenv("X13_PATH"),
      "\n\nSince version 1.2, 'seasonal' uses the binaries from the 'x13binary' 
package, and does not require 'X13_PATH' to be set. Only set 'X13_PATH' 
manually if you want to use your own compilation."
    )
  }

  checkX13(fullcheck = FALSE, htmlcheck = TRUE)
}


