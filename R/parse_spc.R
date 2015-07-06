#' @export
parse_spc <- function(txt){  
  # parse text of an X-13 spc file
  # 
  # text  character vector
  #
  # return "spclist" object
  #
  # requires parse_spec

  stopifnot(inherits(txt, "character"))
  if (length(txt) > 1) {
    txt <- paste(txt, collapse = " ")
  }

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
  
  # invoke tidyup_arg, but not for the 'model' argument
  z[names(z) != "model"] <- lapply(z[names(z) != "model"], tidyup_arg)

  z
}




tidyup_arg <- function(x){
  # tidy up an argument from a spec
  # removes brackets, converts to (numeric) vector
  #
  # x   character vector of length 1
  #
  # returns a character string
  
  stopifnot(length(x) == 1)
  
  # remove curved brackets
  x.nb <- gsub("[\\(\\)]", " ", x)
  
  # split along spaces (if not double quoted)
  if (!grepl('[\\"].*[\\"]', x.nb)){
    z <- strsplit(x.nb, '\\s+')[[1]]
    z <- z[z != ""]    # remove emtpy elements
  } else {
    z <- x.nb
  }
  
  # convert to numeric if possible
  try.numeric <- suppressWarnings(as.numeric(z))
  if (!any(is.na(try.numeric))){
    z <- as.numeric(z)
    if (identical(z, numeric(0))){ # don't return 'numeric(0)'
      z <- NULL
    }
  }
  
  z
}



