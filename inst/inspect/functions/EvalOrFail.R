
# cstr <- 'seas(AirPassengers, xreg = file.remove("ttt2.xlsx"))'
# EvalOrFail(cstr)

# cstr <- 'seas(AirPassengers)'
# EvalOrFail(cstr)

# cstr <- 'rm(AirPassengers)'
# EvalOrFail(cstr)

# cstr <- 'seas(window(AirPassengers, end = 1960))'
# EvalOrFail(cstr)

# cstr <- 'seas(window(AirPassengers, end = file.remove("ttt2.xlsx")))'
# EvalOrFail(cstr)

# cstr <- 'seas(
# x = AirPassengers,
# regression.variables = c("easter[8]", "td1coef")
# )'
# EvalOrFail(cstr)


# seas(
# x = AirPassengers,
# x11.seasonalma = c("S3x9", "S3x9", "S3x5", "S3x5", "S3x5", "S3x5", "S3x5", "S3x5", "S3x5", "S3x5", "S3x5", "S3x5")
# )




IsCallSave <- function(cl){
  fun.allowed <- c("window", "genhol", "ts", "seas", "c", "list", "-", "+", "*", "/")


  # does the call have subcalls, if not, it is save
  is.scl <- sapply(cl, class) == "call"
  if (all(!(is.scl))){
    return(TRUE)
  }
  # if it has subcalls, they must be further analyzed
  scl <- cl[is.scl]
  scl.names <- unlist(lapply(scl, function(e) as.character(e[[1]])))
  if (!all(scl.names %in% fun.allowed)){
    not.allowed <- scl.names[!scl.names %in% fun.allowed]
    write(paste("### Attempt to call forbidden function:", not.allowed), file = "www/log555/unsave.txt", append = TRUE)
    return(FALSE)
  } else {
    rr <- unlist(lapply(scl, IsCallSave))
    if (all(rr)){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}


EvalOrFail <- function(cstr){
  pcl <- try(parse(text = cstr))
  if (inherits(pcl, "try-error")){
    z <- "Invalid call."
    class(z) <- "try-error"
  } else if (inherits(try(as.call(pcl)[[1]][[1]], silent = TRUE), "try-error")){
    z <- "Invalid call."
    class(z) <- "try-error"
  } else if (as.call(pcl)[[1]][[1]] != "seas"){
    z <- "Only calls to seas() are allowed."
    class(z) <- "try-error"
  } else if (!IsCallSave(as.call(pcl)[[1]])){
    write(cstr, file = "www/log555/unsave.txt", append = TRUE)
    z <- "Call is not save and thus not allowed."
    class(z) <- "try-error"
  } else {
    z <- try(eval(pcl), silent = TRUE)
  }
  z
}

