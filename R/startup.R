.onLoad <- function(...){
  if (Sys.getenv("X13_PATH") != ""){
    if (x13binary::supportedPlatform()){
      # skip this message if X13_PATH is set to x13binary.path
      if (Sys.getenv("X13_PATH") != x13binary::x13path()){
      # if (Sys.getenv("X13_PATH") != x13binary::x13path()){
        mymsg(
          "The system variable 'X13_PATH' has been manually set to: \n  ", 
          Sys.getenv("X13_PATH"),
          "\nSince version 1.2, 'seasonal' relies on the 'x13binary' ",
          "\npackage and does not require 'X13_PATH' to be set anymore. ",
          "\nOnly set 'X13_PATH' manually if you intend to use your own",
          "\nbinaries. See ?seasonal for details."
        )
      }
    }
  }

  if (Sys.getenv("X13_PATH") == ""){
    if (!x13binary::supportedPlatform()){
      return(mymsg("Unsupported platform: ", 
        Sys.info()["sysname"], " ", Sys.info()["release"], 
        "\nFor this platform, the path to the binary executable of X-13",
        "\nhas to be manually specified. See ?seasonal for details."
      ))
    }
    cc <- try(x13binary::checkX13binary(verbose = FALSE), silent = TRUE)
    if (inherits(cc, "try-error")) {
      return(mymsg(
        "The binaries provided by 'x13binary' do not work on this", 
        "\nmachine. To get more information, run:", 
        "\n  x13binary::checkX13binary()", 
        "\n\nYou can set 'X13_PATH' manually if you intend to use your own",
        "\nbinaries. See ?seasonal for details.\n"
      ))
    }
    Sys.setenv(X13_PATH = x13binary::x13path())
  }
  checkX13(fullcheck = FALSE, htmlcheck = TRUE)
}

# to avoid a NOTE in R CMD CHECK
mymsg <- packageStartupMessage
