read_mdl <- function(file){
  # Read an .mdl file from X13-ARIMA-SEATS
  # 
  # file  full path without file ending. this is a bit complicated, and could be
  # probably simplified.
  #
  # return "spclist" object, similar to the input specification
  #
  # depends: ReadFile, ReadSpec, ReadText, CleanElement
  
  z <- ReadFile(paste0(file, ".mdl"))
  class(z) <- c("spclist", "list")
  
  # keep arima models as a single string, 
  # transform other SPC vectors to R vectors
  for (i in seq_along(z)){
    z[[i]] <- as.list(z[[i]])
    for (j in seq_along(z[[i]])){
      if (names(z[[i]])[j] != "model"){
        z[[i]][[j]] <- CleanElement(z[[i]][[j]])
      } else {
        z[[i]][[j]] <- str_trim(z[[i]][[j]])
      }
    }    
  }  
  z
}



ReadSpec <- function(txt){
  # Read a single Spec from a file formated like .spc or .mdl
  
  st <- str_split(txt, '=')[[1]]  # split at '='
  
  if (length(st) > 2){
    st.red <- st[2:(length(st)-1)]  # exclude last and first element
    sl.red <- str_match_all(st.red, '(.*)\\s([a-z]+)')  # match to a list
    sm.red <- do.call(rbind, sl.red)  # convert to a matrix
    
    z <- c(sm.red[,2], st[length(st)])  # combine with the excluded last content
    names(z) <- c(st[1], sm.red[,3])  # combine with the excluded first name
  } else if (length(st) == 2){
    z <- st[2]
    names(z) <- st[1]
  } else {
    z <- NULL
  }
  z
}

# removes brackets and converts to (numeric if possible) vector
#
# x   character vector of length 1
#
CleanElement <- function(x){
  stopifnot(length(x) == 1)
  
  # remove curved brackets
  x.nb <- str_replace_all(x, '[\\(\\)]', ' ')
  
  # slit along spaces (if not double quoted)
  if (!str_detect(x.nb, '[\\"].*[\\"]')){
    z <- str_split(str_trim(x.nb), '\\s+')[[1]]
  } else {
    z <- x.nb
  }
  
  # convert to numeric if possible
  try.numeric <- suppressWarnings(as.numeric(z))
  if (!any(is.na(try.numeric))){
    z <- as.numeric(z)
  }
  
  z
}



# Read a file formated like .spc or .mdl
ReadFile <- function(file){
  txt = paste(readLines(file), collapse = " ")
  curly.txt <- ReadText(txt)
  z <- lapply(curly.txt, ReadSpec)
  #   z <- lapply(z, function(el) lapply(el, ReadElement))
  z
}


ReadText <- function(txt){
  mat <- str_match_all(txt, '(?:([a-zA-Z1-9]+))\\{(.*?)\\}')[[1]]
  z <- mat[,3]
  names(z) <- mat[,2]
  z <- str_replace_all(z, pattern = '\\s+', " ")
  str_trim(z)
}

