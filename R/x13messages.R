# subfunctions that search output files for some information
# used by: seas

detect_error <- function(err, htmlmode = getOption("htmlmode")){
  # error parsing from .err or .err.html character vector
  #
  # err  character vector, content of output file
  #
  # returns an object of class x13messages which can be printed

  ParseInfoHtml <- function(openl){
    # find next closing tag
    clt <- grep("</p>", err)

    # in rare cases, X-13 html returns non html errors
    if (all(clt < openl)){
      return(ParseInfoNonHtml(openl))
    }

    closel <- clt[clt >= openl][1]

    # in rare cases, there is a second paragraph after a ":". If so, go for the
    # next ":".
    if (grepl(":$", err[closel - 1]) && length(clt[clt >= openl] > 1)){
      closel <- clt[clt >= openl][2]
    }

    # extract info between tags
    z <- paste(err[openl:closel], collapse = "")

    # clean info
    z <- gsub("<p>.*</strong>", "", z) # remove trailing tag
    z <- gsub("</p>", "", z)           # remove closing tag 
    z <- gsub("&nbsp;", "", z)  
    z <- gsub("\\s+", " ", z)          # remove multiple space
    z <- gsub("^\\s", "", z)           # remove trailing space
    z <- gsub("<.+?>", "", z)          # remove inside HTML tags
    z
  }

  ParseInfoNonHtml <- function(openl){
    clt <- which(err == "  " | err == "" | grepl("^ [A-Z]{4}", err) | grepl("  \\*\\*", err)) 
    closel <- clt[clt > openl][1] - 1

    if (is.na(closel)){
      closel <- length(err)
    }

    z <- paste(err[openl:closel], collapse = "")
    z <- gsub("\\s+", " ", z)    # remove multiple space
    z <- gsub("^.*: ", "", z)    # remove trailing tag
    z <- gsub("^\\s", "", z)     # remove trailing space
    z
  }

  if (htmlmode == 1){
    ParseInfo <- ParseInfoHtml
  } else {
    ParseInfo <- ParseInfoNonHtml
  }

  z <- list()
  class(z) <- "x13messages"
  z$error <- sapply(grep("ERROR:", err), ParseInfo)
  z$warning <- sapply(grep("WARNING:", err), ParseInfo)
  # do not show this meaningless warning 
  # (its caused by default activation of spectrum)
  z$warning <- z$warning[!grepl("Spectrums are only generated for monthly series.", z$warning)]
  z$note <- sapply(grep("note:", err, ignore.case = TRUE), ParseInfo)
  z
}

#' @method print x13messages
print.x13messages <- function(x, ...) {
  print_x13messages(x)
}

print_x13messages <- function(x, main = "\nMessages generated by X-13:\n"){
  if (length(x$error) == 0 & length(x$warning) == 0 & length(x$note) == 0 ){
    return(NULL)
  }
  cat(main)
  if (length(x$error) > 0){
    cat("Errors:\n")
    cat((paste(strwrap(paste("-", x$error), width = getOption("width") - 15, exdent = 2), 
               collapse = "\n")))
    cat("\n\n")
  }
  if (length(x$warning) > 0){
    cat("Warnings:\n")
    cat((paste(strwrap(paste("-", x$warning), width = getOption("width") - 15, exdent = 2), 
               collapse = "\n")))
    cat("\n\n")
  }
  if (length(x$note) > 0){
    cat("Notes:\n")
    cat((paste(strwrap(paste("-", x$note), width = getOption("width") - 15, exdent = 2), 
               collapse = "\n")))
    cat("\n")
  }
}



read_err <- function(basename){
  if (getOption("htmlmode") == 1){
    errtxt <- readLines(paste0(basename, "_err.html"))
  } else {
    errtxt <- readLines(paste0(basename, ".err"))
  }
  detect_error(errtxt)
}



drop_x13messages <- function(x, msg = "X-13 run failed\n\n", msgfun = stop, ontype = "error"){
  if (ontype != "all"){
    if (length(x[[ontype]]) == 0){
      return("")
    }
  }

  em <- paste0(capture.output(print_x13messages(x, main = "")), collapse = "\n")
  if (em == "NULL") em <- ""
  msgfun(paste0(msg, em), call. = FALSE)
}


