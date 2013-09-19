#' @export
ReadText <- function(txt){
  require(stringr)
  mat <- str_match_all(txt, '(?:([a-z]+))\\{(.*?)\\}')[[1]]
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
ReadElement <- function(x){
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
ReadX13 <- function(file){
  txt = paste(readLines(file), collapse = " ")
  curly.txt <- ReadText(txt)
  z <- lapply(curly.txt, ReadSpec)
  lapply(z, function(el) lapply(el, ReadElement))
}

#' @export
WriteSpec <- function(x){
  nx <- names(x)
  z <- character(length = length(x))
  for (i in seq_along(x)){
    if (length(x[[i]]) > 1){
      # only put brackets around several elements
      z[i] <- paste0("  ", nx[i], " = (", paste(x[[i]], collapse = " "), ")")
    } else if (length(x[[i]] == 1)){
      z[i] <- paste0("  ", nx[i], " = ", x[[i]])
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
WriteX13 <- function(x, file){
  stopifnot(inherits(x, "list"))
  txt <- WriteText(x)
  writeLines(txt, con = file)
}
