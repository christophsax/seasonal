# subfunctions that search the .out file for some information
# used by: seas

detect_log <- function(outtxt){
  # parse automatic log detection from .out txt
  #
  # outtxt  character vector, content of .out output file
  #
  # returns logical, is log preferred
  
  first <- which(outtxt == 
            " Likelihood statistics for model fit to log transformed series.")
  area <- outtxt[(first + 10):(first + 30)]
  line <- area[grepl("prefers", area)]
  
  if (length(line) == 1){
    z <- grepl("log", line)
  } else {
    z <- FALSE
  }
  z
}


detect_fivebestmdl <- function(outtxt){
  # parse fivebestmdl from .out txt
  #
  # outtxt  character vector, content of .out output file
  #
  # returns character vector
  
  first <- which(outtxt == "  Best Five ARIMA Models")
  
  if (length(first) == 1){
    z <- outtxt[first:(first + 10)]
  } else {
    z <- NULL
  }
  z
}


detect_seatsmdl <- function(outtxt){
  # parse seatsmdl from .out txt
  #
  # outtxt  character vector, content of .out output file
  #
  # returns character vector
  
  first <- which(outtxt == "  MODEL CHANGED TO :")
  
  if (length(first) == 1){
    z <- outtxt[seatsmdl+1]
    z <- gsub("\\s+", " ", z)      # subst several spaces by one space
    z <- gsub(",", "", z)          # remove , 
    z <- gsub("^\\s|\\s$", "", z)  # trim lead. and trail spaces
  } else {
    z <- NULL
  }
  z
}





