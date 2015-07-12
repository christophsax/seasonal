#' @export
parse_spc <- function(txt){  
  # parse text of an X-13 spc file
  # 
  # text  character vector
  #
  # return "spclist" object
  #
  # requires parse_spec

# browser()



# txt <- "transform{\n  function = auto\n  print = aictransform\n}\n\nregression{\n  aictest = (td easter)\n}\n\noutlier{\n\n}\n\nautomdl{\n  print = bestfivemdl\n}\n\nx11{\n  save = (d10 d11 d12 d13 d16 e18)\n}\n\nestimate{\n  save = (model estimates lkstats residuals)\n}\n\nspectrum{\n  print = qs\n}"



  stopifnot(inherits(txt, "character"))
  if (length(txt) > 1) {
    txt <- paste(txt, collapse = "\n")
  }

  # positions of curly braces
  op <- gregexpr("\\{", txt)[[1]]
  cl <- gregexpr("\\}", txt)[[1]]

  z0 <- Map(substr, x = txt, start = op + 1, stop = cl - 1)
  
  # trim spaces
  z0 <- lapply(z0, function(e) gsub("^ +| +$", "", e))

  nam <- Map(substr, x = txt, start = c(1, cl[-length(cl)] + 1),
   stop = op - 1)
  
  nam <- gsub("\\n", "", nam)


  stopifnot(length(z0) == length(nam))


names(z0) <- nam
  
  # # separate individual specs
  # z0 <- list()
  # for (i in 1:length(op)){
  #   # content in the curly braces (spec)
  #   z0[[i]] <- substr(txt, start = (op[i] + 1), stop = (cl[i] - 1))  
    
  #   # name of the spec
  #   # start.name <- ifelse(i == 1, 1, cl[i - 1] + 1)
  #   # name.i <- substr(txt, start = start.name, stop = (op[i] - 1))
  #   # names(z0)[i] <- gsub(" ","", name.i) 
  # }
  

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
  
  # e.g.
  # txt <- ("\n  function=auto\n  savelog=autotransform  \n")
  # txt <- ("\n  savelog=peaks\t\n")

  # positions of curly braces (ignore subsequent bracktets form arima model)
  
  # parse_singlespc("\n  noadmiss = yes\n  save = (s10 s11 s12 s13 s16 s18)\n")
  # parse_singlespc("\n  aictest = (td easter)\n")
  # parse_singlespc("\n\n")




  st <- strsplit(txt, split = "\n ")[[1]]
  if (length(st) == 1) return(gsub("\\n|^ *| *$", "", st))

  st <- st[!grepl("^ *$", st)]

  snamarg <- strsplit(st, split = "=")

  # arg <- spltxt[-1]

  arg <- sapply(snamarg, function(e) e[[2]])

  # arg <- gsub("( +$)|(^ +)", "", arg)
  # arg <- gsub("[a-zA-Z0-9]+$", "", arg)
  arg <- gsub("\\n", "", arg)
  arg <- gsub("( +$)|(^ +)", "", arg)

  nam <- sapply(snamarg, function(e) e[[1]])

  # nam <- spltxt[-length(spltxt)]
  nam <- gsub("( +$)|(^ +)", "", nam)
  # nam <- gsub(".* ([A-Za-z0-9]+$)", "\\1", nam)


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



