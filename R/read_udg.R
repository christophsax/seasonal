read_udg <- function(file){
  # read and parse a .udg file
  # 
  # file  full path without file ending
  #
  # return a named character vector with some statistics
  udg.raw <-  read.table(paste0(file, ".udg"), sep = ":", stringsAsFactors = FALSE)
  udg <- udg.raw[,-1]
  names(udg) <- udg.raw[,1]
  udg <- gsub("^ +| +$", "", udg)
  # TODO: list of elements to keep (memory reasons)
  udg
}