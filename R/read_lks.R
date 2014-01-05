read_lks <- function(file){
  # read and parse a .lks file
  # 
  # file  full path without file ending
  #
  # return a vector with some statistics
  
  lks.raw <-  readLines(paste0(file, ".lks"))[-1]
  lks.split <- strsplit(lks.raw, "\\s+")
  lks <- do.call(rbind, lks.split)
  z <- as.numeric(lks[,2])
  names(z) <- lks[,1]
  z
}