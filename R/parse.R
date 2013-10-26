

# Parse a single spec from a spclist 
#
# x   list element in a spclist
#
SpcToTxt <- function(x){
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
SpclistToTxt <- function(x){
  xl <- lapply(x, SpcToTxt)
  paste(paste0(names(x), "{\n", xl, "\n}"), collapse = "\n\n")
}


# WriteDatavalue(austres, file = "data.dat")
WriteDatavalue <- function(x, file = "data.dat"){
  stopifnot(inherits(x, "ts"))
  data <- cbind(floor(time(x)), cycle(x), x)
  write.table(data, file = file, sep = " ", 
              row.names = FALSE,
              col.names = FALSE)
}


