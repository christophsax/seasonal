read_mdl <- function(iofile) {

  mdlfile <- paste0(iofile, ".mdl")
  if(!file.exists(mdlfile)) {
    return(NULL)
  }
  mdl <- readLines(mdlfile)

  # Workaround: in the .mdl output, full regime changes are returned weiredly.
  # E.g.
  # variables=(
  #  td/ for before 1955.Jan/
  # )
  is.r.change <- grepl("//?[ A-Za-z]", mdl)
  rch0 <- mdl[is.r.change]
  rch <- gsub("//[ A-Za-z].+ ", "//", rch0)
  rch <- gsub("/[ A-Za-z].+ ", "/", rch0)
  mdl[is.r.change] <- rch
  out <- try(parse_spc(mdl), silent = TRUE)

  # fails for very complicated models, but is needed only for static()
  if (inherits(out, "try-error")){
    return(NULL)
  }
  out
}



parse_spec <- function(txt){
  # parse a single spec into arguments
  #
  # txt  character string, content of a spec
  #
  # returns a named list the arguments
  #
  # requires tidyup_arg

  # positions of curly braces (ignore subsequent bracktets form arima model)

  op <- gregexpr("[^\\)]\\(", txt)[[1]] + 1
  cl <- gregexpr("\\)[^\\(]", txt)[[1]]

  # separate individual arguments
  z <- list()
  for (i in 1:length(op)){
    # content in the brackets (argument)
    z[[i]] <- substr(txt, start = op[i], stop = (cl[i]))

    # name of the argument
    start.name <- ifelse(i == 1, 1, cl[i - 1] + 1)
    name.i <- substr(txt, start = start.name, stop = (op[i] - 1))
    names(z)[i] <- gsub("[ =]","", name.i)   # remove whitespace and "="

    # tidy up unless its a 'model' entry (containing the arima argument)
    if (names(z)[i] != "model"){
      z[[i]] <- tidyup_arg(z[[i]])
    } else {
      z[[i]] <- gsub("^\\s+|\\s+$", "", z[[i]])  # trim lead. and trail spaces
    }
  }
  z
}



