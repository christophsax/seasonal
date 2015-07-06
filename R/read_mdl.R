# DEPRECATED, substituted by more universal parse_spc remove if tests in seas are done


read_mdl <- function(file){
  # read and parse a .mdl file
  # 
  # file  full path without file ending
  #
  # return "spclist" object, similar to the input specification
  #
  # requires parse_spec
  
  # collapse char vector to single char element
  txt = paste(readLines(paste0(file, ".mdl")), collapse = " ")
    
  # positions of curly braces
  op <- gregexpr("\\{", txt)[[1]]
  cl <- gregexpr("\\}", txt)[[1]]

  stopifnot(length(op) == length(cl))

  # separate individual specs
  z0 <- list()
  for (i in 1:length(op)){
    # content in the curly braces (spec)
    z0[[i]] <- substr(txt, start = (op[i] + 1), stop = (cl[i] - 1))  
    
    # name of the spec
    start.name <- ifelse(i == 1, 1, cl[i - 1] + 1)
    name.i <- substr(txt, start = start.name, stop = (op[i] - 1))
    names(z0)[i] <- gsub(" ","", name.i)   # remove whitespace
  }
  
  # parse each element
  z <- lapply(z0, parse_spec)  
  class(z) <- c("spclist", "list")
  
  z
}



parse_spec <- function(txt){
  # parse a single spec into arguments
  #
  # txt  character string, content of a spec
  #
  # returns a named list the arguments
  #
  # requires tidyup_arg
  
  # positions of curly braces (ignore subsequent bracktets form arima model)
  
  op <- gregexpr("[^\\)]\\(", txt)[[1]] + 1
  cl <- gregexpr("\\)[^\\(]", txt)[[1]]
  
  # separate individual arguments
  z <- list()
  for (i in 1:length(op)){
    # content in the brackets (argument)
    z[[i]] <- substr(txt, start = op[i], stop = (cl[i]))  
    
    # name of the argument
    start.name <- ifelse(i == 1, 1, cl[i - 1] + 1)
    name.i <- substr(txt, start = start.name, stop = (op[i] - 1))
    names(z)[i] <- gsub("[ =]","", name.i)   # remove whitespace and "="
    
    # tidy up unless its a 'model' entry (containing the arima argument)
    if (names(z)[i] != "model"){
      z[[i]] <- tidyup_arg(z[[i]])
    } else {
      z[[i]] <- gsub("^\\s+|\\s+$", "", z[[i]])  # trim lead. and trail spaces
    }
  }
  z
}



