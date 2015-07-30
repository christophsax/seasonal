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
      str <- paste(x[[i]], collapse = " ")
      if (nchar(str) > 115){  # strings should be no longer than 130 
        str <- paste(strwrap(str, width = 115), collapse = "\n  ")
      }
      # put brackets around several elements
      z[i] <- paste0("  ", nx[i], " = (", str, ")")
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
  stopifnot(inherits(x, "spclist"))

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
  # 10e-4 avoids rounding error
  year <- floor(time(x) + 10e-4)
  per <- cycle(x)
  data <- cbind(year, per, x)
  
  
  write.table(data, file = file, sep = " ", 
              row.names = FALSE,
              col.names = FALSE)

}


as.character.arima <- function(x){
  # converts a numeric arima model to a character string
  # 
  # x  numeric or character,  "ts" object
  # 
  # WriteDatavalue(austres, file = "data.dat")
  
  if (!class(x) %in% c("numeric", "character")){
    stop("arima.model: must be numeric or character", call. = FALSE)
  }
  
  if (is.numeric(x)){
    if (length(x) == 6){
      z <- paste0("(", 
                  paste(x[1:3], collapse = " "), 
                  ")(",
                  paste(x[4:6], collapse = " "),
                  ")"
      )
    } else if (length(x) == 3){
      z <- paste0("(", 
                  paste(x[1:3], collapse = " "), 
                  ")"
      )
    } else {
      stop("arima.model: only numeric vector of length 3 or 6 allowed, or a character string.", call. = FALSE)
    }
  } else {
    z <- x
  }
  z
}
