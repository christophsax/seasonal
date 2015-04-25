

format_seascall <- function(x){
  stopifnot(inherits(x, "call"))
  if (deparse(x[[1]]) != "seas"){
    stop('format_seascall() can only be applied to calls of the seas()')
  }
  # call in which all arguments are specified by their full names
  x <- match.call(definition = seas, x)
  xl <- as.list(x)
  z <- list()
  for (i in 2:length(xl)){
    z[[i - 1]] <- paste(names(xl)[i], "=", deparse(xl[[i]]))
  }
  argstr <- do.call(paste, c(z, sep = ",\n"))
  z <- paste("seas(", argstr, ")", sep = "\n")
  z
}

