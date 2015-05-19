#' @export
parse_spc <- function(txt){
  # parse text of an X-13 spc file
  # 
  # text  character vector
  #
  # return "spclist" object
  #
  # requires parse_spec
  
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
    names(z0)[i] <- gsub(" ","", name.i) 
  }
  
  names(z0) <- gsub("\\n", "", names(z0))

  # parse each element
  z <- lapply(z0, parse_singlespc)  
  class(z) <- c("spclist", "list")
  
  z
}


#' @export
parse_singlespc <- function(txt){
  # parse a single spec into arguments
  #
  # txt  character string, content of a spec
  #
  # returns a named list the arguments
  #
  # requires tidyup_arg
  
  # positions of curly braces (ignore subsequent bracktets form arima model)
   
  spltxt <- strsplit(txt, split = "=")[[1]]

  arg <- spltxt[-1]
  arg <- gsub("( +$)|(^ +)", "", arg)
  arg <- gsub("[a-zA-Z0-9]+$", "", arg)
  arg <- gsub("\\n", "", arg)
  arg <- gsub("( +$)|(^ +)", "", arg)

  nam <- spltxt[-length(spltxt)]
  nam <- gsub("( +$)|(^ +)", "", nam)
  nam <- gsub(".* ([A-Za-z0-9]+$)", "\\1", nam)


  z <- as.list(arg)
  names(z) <- nam

  z <- lapply(z, tidyup_arg)

  z
}

