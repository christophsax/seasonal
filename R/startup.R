.onLoad <- function(...){

  if (Sys.getenv("X13_PATH") == ""){
    default.path <- file.path(path.package("x13binary"), "bin")
    if (.Platform$OS.type == "windows"){    
      x13.bin.html <- file.path(default.path, "x13ashtml.exe")
    } else {
      # Solaris users get this message
      if (!Sys.info()["sysname"] %in% c("Darwin", "Linux")){
      return(packageStartupMessage("Unusual platform: ", Sys.info()["sysname"], 
        "\nFor this platform, the path to the binary executable of X-13",
        "\nhas to be manually specified. For more information, consider", 
        "\nAppendix A of the package vignette:",
        "\n  vingnette(seas)\n")
      )
      }
      x13.bin.html <- file.path(default.path, "x13ashtml")
    }

    if (file.exists(x13.bin.html)){
      Sys.setenv(X13_PATH = default.path)
      env.path = default.path
    }
  } else {
    # Solaris users don't get this message
    if ((.Platform$OS.type == "windows") || (Sys.info()["sysname"] %in% c("Darwin", "Linux"))){
      packageStartupMessage(
        "The system variable 'X13_PATH' has been manually set to: \n  ", 
        Sys.getenv("X13_PATH"),
        "\n\nFrom version 1.2 on, 'seasonal' uses the X-13 binaries", 
        "\nprovided by the 'x13binary' package, and does not require ",
        "\n'X13_PATH' to be set anymore. Only set 'X13_PATH' manually ",
        "\nif you intend to use your own binaries."
      )
    }
  }

  checkX13(fullcheck = FALSE, htmlcheck = TRUE)
}


