# subfunctions that search the .out file for some information
# used by: seas

detect_log <- function(outfile){
  # a dirty way to detect wether the series was transformed or not
  #
  # outfile  character vector, content of .out output file
  #
  # returns TRUE / FALSE 
  
  logn <- grep("prefers", outfile[50:150])
  if (length(logn) == 1){
    logstr <- outfile[50:150][logn]
    z <- grepl("log", logstr)
  } else if (length(logn) == 0){
    z <- FALSE
  } else {
    stop("more than one match of 'prefers' in the .out file. Dont know what to do.")
  }
  z
}


detect_fivebestmdl <- function(outfile){
  # a dirty way to detect fivebestmdl
  #
  # out  character vector, content of .out output file
  #
  # returns character vector
  
  relevant.area <- outfile[100:400]
  bestfive <- which(relevant.area == "  Best Five ARIMA Models")
  if (length(bestfive) == 0){
    return("fivebestmdl section in .out file not found.")
  } else {
    return(relevant.area[bestfive:(bestfive+10)])
  }
}


detect_seatsmdl <- function(outfile){
  # a dirty way to detect seatsmdl
  #
  # out  character vector, content of .out output file
  #
  # returns character vector
  seatsmdl <- which(outfile == "  MODEL CHANGED TO :")
  if (length(seatsmdl) == 0){
    z <- "MODEL CHANGED TO section in .out file not found."
  } else {
    z <- outfile[seatsmdl+1]
    z <- gsub("\\s+", " ", z)      # subst several spaces by one space
    z <- gsub(",", "", z)          # remove , 
    z <- gsub("^\\s|\\s$", "", z)  # trim lead. and trail spaces
  }
  z
}

# 
# 
# detect_history <- function(outfile){
#   outfile[grep("History", outfile)[1]:length(outfile)]
# }
# 
# 
# detect_slidingspans <- function(outfile){
#   slid.line <- which(outfile == " S  2.  Percentage of months flagged as unstable.")
#   z <- list()
#   txt.sf <- outfile[slid.line + 3]
#   n.sf <- as.numeric(gsub(".*(\\d) out of.*", "\\1", txt.sf))
#   N.sf <- as.numeric(gsub(".* out of \\s?(\\d*) .*", "\\1", txt.sf))
#   z$factors <- c(n.sf, N.sf, n.sf/N.sf)
#   
#   z$factors.text <- paste0(n.sf, " out of ", N.sf, " (", formatC(100*n.sf/N.sf, digits = 0), "%",
#                            symnum(n.sf/N.sf, cutpoints = c(0, 0.15, 0.25, 1), 
#                                   symbols = c(")", ": too high)", ": much too high)"), legend = FALSE
#                            )
#   )
#   
#   txt.ch <- outfile[slid.line + 5]
#   n.ch <- as.numeric(gsub(".*(\\d) out of.*", "\\1", txt.ch))
#   N.ch <- as.numeric(gsub(".* out of \\s?(\\d*) .*", "\\1", txt.ch))
#   z$changes <- c(n.ch, N.ch, n.ch/N.ch)
#   
#   z$changes.text <- paste0(n.ch, " out of ", N.ch, " (", formatC(100*n.ch/N.ch, digits = 0), "%",
#                            symnum(n.ch/N.ch, cutpoints = c(0, 0.35, 0.40, 1), 
#                                   symbols = c(")", ": too high)", ": much too high)"), legend = FALSE
#                            )
#   )
#   z$out <- outfile[grep("[Ss]liding spans", outfile)[1]:length(outfile)]
#   z
# }
