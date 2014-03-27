# subfunctions that search the .out file for some information
# used by: seas

detect_auto <- function(outtxt){
  # parse automatic log detection from .out txt
  #
  # outtxt  character vector, content of .out output file
  #
  # returns character string, "log" or "none"
  first <- which(outtxt == 
            " Likelihood statistics for model fit to log transformed series.")
  if (length(first) == 0){
    return(NULL)
  }
  area <- outtxt[(first + 10):(first + 30)]
  line <- area[grepl("prefers", area)]
  
  if (length(line) == 1){
    if (grepl("log", line)){
      z <- "log"
    } else {
      z <- "none"
    }
  } else {
    z <- NULL
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
  
  # if there are several runs, take the last
  if (length(first) >= 1) {
    z <- outtxt[first[length(first)]:(first[length(first)] + 10)]
  } else {
    z <- NULL
  }
  z
}


detect_qs <- function(outtxt){
  # parse QS test for seasonality from .out txt
  #
  # outtxt  character vector, content of .out output file
  #
  # returns character vector
  
  
  first <- which(outtxt == "  QS statistic for seasonality:")
  if (length(first) == 1){
    
    # lines to show (-1)
    nl <- grep("Irregular Series \\(EV adj\\)", outtxt[first:(first + 10)])
    
    txt <- outtxt[(first + 1):(first + nl - 1)]
    # parse fixed width table
    descr <- substr(txt, start = 3, stop = 51)
    descr <- gsub("^\\s+|\\s+$", "", descr)   # trim lead. and trail spaces
    stat <- as.numeric(substr(txt, start = 52, stop = 60))
    pval <- as.numeric(substr(txt, start = 71, stop = 81))
    
    z <- cbind(stat, pval)
    rownames(z) <- descr
    z <- z[!is.na(z[, "stat"]), ]
    
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
    z <- outtxt[first + 1]
    z <- gsub("\\s+", " ", z)      # subst several spaces by one space
    z <- gsub(",", "", z)          # remove , 
    z <- gsub("^\\s|\\s$", "", z)  # trim lead. and trail spaces
  } else {
    z <- NULL
  }
  z
}


