.onLoad <- function(...){
#   if (file.exists(file.path(Sys.getenv("X13_PATH"), "x13asHTML"))){
#     options(htmlmode = 1)
#   } else {
#     options(htmlmode = 0)
#   }
  
  checkX13(fullcheck = FALSE, htmlcheck = TRUE)
  

#   if (is.null(getOption("htmlmode"))){
#     message("seasonal (>= 0.8) uses shiny to display the html output of x13html. It is recommended to install the HTML version of X13 as well as the shiny R package. For installation, see section 2 of the vingette ('vignette(\"seas\")'). TODO: make this message dependent on whether these things are installed.")
#   }
  
}


