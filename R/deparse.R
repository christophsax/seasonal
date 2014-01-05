deparse_spc <- function(x){
  # deparse a single spec from a spclist 
  #
  # x   list element in a spclist
  #
  # returns the parsed text for writing to a .spc file
  
  nx <- names(x)
  z <- character(length = length(x))
  for (i in seq_along(x)){
    if (length(x[[i]]) > 1){
      # put brackets around several elements
      x.i <- paste0(nx[i], " = (", paste(x[[i]], collapse = " "), ")")
      
      z[i] <- strwrap(x.i, indent = 2, exdent = 4)
    } else if (length(x[[i]] == 1)){
      # put brackets around elements containing a comma
      if (grepl(',', x[[i]])){
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

deparse_spclist <- function(x){
  # deparse a spclist
  #
  # x   a "spclist" object
  #
  # returns the parsed text for writing to a .spc file

  xl <- lapply(x, deparse_spc)
  paste(paste0(names(x), "{\n", xl, "\n}"), collapse = "\n\n")
}



write_ts_dat <- function(x, file = "data.dat"){
  # Write a "ts" object to a .dat file
  #
  # Uses the X13-ARIMA-SEATS "datevalue" format.
  # 
  # x  a "ts" object
  # 
  # WriteDatavalue(austres, file = "data.dat")
  
  stopifnot(inherits(x, "ts"))
  data <- cbind(floor(time(x)), cycle(x), x)
  write.table(data, file = file, sep = " ", 
              row.names = FALSE,
              col.names = FALSE)

}


