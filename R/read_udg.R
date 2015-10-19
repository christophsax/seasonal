read_udg <- function(file){
  # read and parse a .udg file
  # 
  # file  full path without file ending
  #
  # return a named character vector with some statistics
  udg0 <- readLines(paste0(file, ".udg"))
  udg1 <- read.table(text = paste(udg0[grepl(":", udg0)], collapse = "\n"), sep = ":", stringsAsFactors = FALSE)
  udg <- udg1[,-1]
  names(udg) <- udg1[,1]
  udg <- gsub("^ +| +$", "", udg)
  # TODO: list of elements to keep (memory reasons)
  udg
}