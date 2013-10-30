# Read a .lks file from X13-ARIMA-SEATS
# 
# file  full path without file ending
#
# return a vector with some statistics
#
read_lks <- function(file){
  lks.raw <-  readLines(paste0(file, ".lks"))[-1]
  lks.split <- str_split(lks.raw, "\\s+")
  
  lks <- do.call(rbind, lks.split)
  z <- as.numeric(lks[,2])
  names(z) <- lks[,1]
  z
}