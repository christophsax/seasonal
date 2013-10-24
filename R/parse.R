#' @export
#' @import stringr
ReadText <- function(txt){
  require(stringr)
  mat <- str_match_all(txt, '(?:([a-zA-Z1-9]+))\\{(.*?)\\}')[[1]]
  z <- mat[,3]
  names(z) <- mat[,2]
  z <- str_replace_all(z, pattern = '\\s+', " ")
  str_trim(z)
}


#' @export
ReadSpec <- function(txt){
  require(stringr)
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

#' @export
CleanElement <- function(x){
  # removes brackets and converts to (numeric if possible) vector

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

#' @export
ReadSPC <- function(file){
  txt = paste(readLines(file), collapse = " ")
  curly.txt <- ReadText(txt)
  z <- lapply(curly.txt, ReadSpec)
#   z <- lapply(z, function(el) lapply(el, ReadElement))

  z
}

#' @export
WriteSpec <- function(x){
  nx <- names(x)
  z <- character(length = length(x))
  for (i in seq_along(x)){
    if (length(x[[i]]) > 1){
      # put brackets around several elements
      x.i <- paste0(nx[i], " = (", paste(x[[i]], collapse = " "), ")")
      z[i] <- str_wrap(x.i, indent = 2, exdent = 4)
    } else if (length(x[[i]] == 1)){
      # put brackets around elements containing a comma
      if (str_detect(x[[i]], ',')){
        x.i <- paste0("(", x[[i]], ")")
      } else {
        x.i <- x[[i]]
      }
      z[i] <- paste0("  ", nx[i], " = ", x.i)
    } else {
      z[i] <- ""
    }
  }
  paste(z, collapse = "\n")
}

#' @export
WriteText <- function(x){
  xl <- lapply(x, WriteSpec)
  paste(paste0(names(x), "{\n", xl, "\n}"), collapse = "\n\n")
}


#' @export
WriteSPC <- function(x, file){
  stopifnot(inherits(x, "list"))
  txt <- WriteText(x)
  writeLines(txt, con = file)
}

# WriteDatavalue(austres, file = "data.dat")
WriteDatavalue <- function(x, file = "data.dat"){
  stopifnot(inherits(x, "ts"))
  data <- cbind(floor(time(x)), cycle(x), x)
  write.table(data, file = file, sep = " ", 
              row.names = FALSE,
              col.names = FALSE)
}


