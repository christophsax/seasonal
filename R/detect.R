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
    
    txt <- outtxt[first:(first + 8)]
    # parse fixed width table
    descr <- substr(txt, start = 3, stop = 51)
    descr <- gsub("^\\s+|\\s+$", "", descr)   # trim lead. and trail spaces
    stat <- as.numeric(substr(txt, start = 52, stop = 60))
    pval <- as.numeric(substr(txt, start = 71, stop = 81))
    
    df <- cbind(stat, pval)
    rownames(df) <- descr
    
    z <- list()
    z <- df[2:8,]
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

# 
# detect_satest.original <- function(outtxt){
#   # parse f-test for seasonality from .out txt
#   #
#   # outtxt  character vector, content of .out output file
#   #
#   # returns character vector
#   
#   first <- which(outtxt == " D 8.A  F-tests for seasonality")
#   
#   if (length(first) == 1){
#     z <- outtxt[first:(first + 35)]
#   } else {
#     z <- NULL
#   }
#   z
# }
# 
# 
# detect_satest <- function(outtxt){
#   # parse f-test for seasonality from .out txt
#   #
#   # outtxt  character vector, content of .out output file
#   #
#   # returns character vector
#   
#   lookup <- "   F 2.I:                                                    Statistic   Prob."
#   first <- which(outtxt == lookup)
#   
#   if (length(first) == 1){
#     z <- outtxt[first:(first + 6)]
#   } else {
#     z <- NULL
#   }
#   z
# }
# 
# 
# detect_quality <- function(outtxt){
#   # parse f-test for seasonality from .out txt
#   #
#   # outtxt  character vector, content of .out output file
#   #
#   # returns character vector
#   
#   lookup <- " F 3. Monitoring and Quality Assessment Statistics"
#   first <- which(outtxt == lookup)
#   
#   if (length(first) == 1){
#     z <- outtxt[first:(first + 30)]
#   } else {
#     z <- NULL
#   }
#   z
# }
# 
# 
# 
# 
# detect_satest.final <- function(outtxt){
#   # parse f-test for seasonality from .out txt
#   #
#   # outtxt  character vector, content of .out output file
#   #
#   # returns character vector
#   
#   first <- which(outtxt == 
#         "                    Test for the presence of residual seasonality.")
#   
#   if (length(first) == 1){
#     z <- outtxt[first:(first + 12)]
#   } else {
#     z <- NULL
#   }
#   z
# }
# 
# 
# 
# 
# 
# 
# 
# 
