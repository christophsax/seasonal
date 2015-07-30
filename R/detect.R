
detect_auto <- function(outtxt){
  # parse automatic log detection from .out txt
  #
  # outtxt  character vector, content of .out output file
  #
  # returns character string, "log" or "none"
  
  if (getOption("htmlmode") == 1){
    first <- which(outtxt == "<h3> Likelihood statistics for model fit to log transformed series.</h3>")
  } else {
    first <- which(outtxt == " Likelihood statistics for model fit to log transformed series.")
  }
  
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
  
  if (getOption("htmlmode") == 1){
    first <- which(outtxt == "<p>Best Five ARIMA Models</p>")
  } else {
    first <- which(outtxt == "  Best Five ARIMA Models")
  }
  
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
  
  if (getOption("htmlmode") == 1){
    first <- which(outtxt == "<caption><strong>QS statistic for seasonality (Full series)</strong></caption>")
    # lines to show (-1)
    nl <- grep("Irregular Series \\(extreme value adjusted\\)", outtxt[first:(first + 20)])
    txt <- outtxt[(first + 8):(first + nl - 1)]
    # parse fixed width table
    descr <- substr(txt, start = 21, stop = 51)
    descr <- gsub("^\\s+|\\s+$", "", descr)   # trim lead. and trail spaces
    stat <- as.numeric(substr(txt, start = 52, stop = 60))
    pval <- as.numeric(substr(txt, start = 71, stop = 81))
    
    
  } else {
    first <- which(outtxt == "  QS statistic for seasonality:")
  }
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


