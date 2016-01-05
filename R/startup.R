.onLoad <- function(...){
  x13binary.path <- system.file("bin", package="x13binary")
  if (Sys.getenv("X13_PATH") != ""){
    if ((.Platform$OS.type == "windows") || (Sys.info()["sysname"] %in% c("Darwin", "Linux"))){
      # Also skip this message if X13_PATH is set to x13binary.path
      if (Sys.getenv("X13_PATH") != x13binary.path){
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
  }
  if (Sys.getenv("X13_PATH") == ""){
    # Solaris users get this message (and return())
    if ((.Platform$OS.type != "windows") && !(Sys.info()["sysname"] %in% c("Darwin", "Linux"))){
      return(message("Unusual platform: ", Sys.info()["sysname"], 
        "\nFor this platform, the path to the binary executable of X-13",
        "\nhas to be manually specified. For more information, consider", 
        "\nAppendix A of the package vignette:",
        "\n  vingnette(seas)\n")
      )
    }
    Sys.setenv(X13_PATH = system.file("bin", package="x13binary"))
  }
  # setX13Path()
  checkX13(fullcheck = FALSE, htmlcheck = TRUE)
}



# setX13Path <- function(){
#   if (Sys.getenv("X13_PATH") == ""){
#     # Solaris users get this message (and return())
#     if ((.Platform$OS.type != "windows") && !(Sys.info()["sysname"] %in% c("Darwin", "Linux"))){
#       return(message("Unusual platform: ", Sys.info()["sysname"], 
#         "\nFor this platform, the path to the binary executable of X-13",
#         "\nhas to be manually specified. For more information, consider", 
#         "\nAppendix A of the package vignette:",
#         "\n  vingnette(seas)\n")
#       )
#     }
#     Sys.setenv(X13_PATH = system.file("bin", package="x13binary"))
#   }
# }
