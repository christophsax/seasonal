#' Extractor Functions for seas Elements and Series
#' 
#' These functions extract elements or series from a \code{"seas"} object.
#' 
#' @param x  an object of class \code{"seas"}.
#'   
#' @return return an element or a series, depending on the function
#'   
#' @seealso \code{\link{seas}} for the main function.
#'   
#' @export
#' @examples
#' \dontrun{
#' x <- seas(AirPassengers)
#' 
#' final(x)
#' original(x)
#' irregular(x)
#' trend(x)
#' 
#' spc(x)  # X13-ARIMA-SEATS .spc file
#' mdl(x)  # X13-ARIMA-SEATS .mdl file
#' 
#' # out(x)  # returns an error
#' x2 <- seas(AirPassengers, out = TRUE)
#' out(x2)    # this works: X13-ARIMA-SEATS .out file
#' }
#' 
spc <- function(x){
  x$spc
}

#' @rdname spc
#' @export
mdl <- function(x){
  x$mdl
}

#' @rdname spc
#' @export
out <- function(x, line = 1, n = 500, search = NULL){
  
  lc <- as.list(x$call)
  lc$out <- TRUE
  z <- eval(as.call(lc), envir = globalenv())
  txt <- z$out
  txt[grepl("^\f", txt)] <- ""
  
  if (!is.null(search)){
    search.res <- which(grepl(search, txt))
    if (length(search.res > 0)){
      line <- search.res[1]
    } else {
      message("string not found")
      return(NULL)
    }
  }
  
  l <- line
  status <- 1
  while (status == 1){
    
    if (l < 0){
      l <- 1
    }
    if (l > (length(txt) - n + 1)){
      l <- length(txt) - n + 1
    }
    
    cat("\014")
    
    cat(txt[l:(l + n - 1)], sep = "\n")
    cat ("\nline ", l, " to ", l + n - 1, " (of ", length(txt), ")\nnext [enter]  previous [p]  to line [number]  quit [q]",sep = "")
    
    inp <- readline()
    if (inp == ""){
      l <- l + n
    } else if (inp == "p"){
      l <- l - n
    } else if (grepl("\\d+", inp)){
      l <- as.numeric(inp)
    } else if (inp == "q"){
      status <- 0
    }
  }
  return(invisible(NULL))
}


#' @rdname spc
#' @export
sarevisions <- function(x, verbose = TRUE, ...){
  ldot <- list(...)
  lc <- as.list(x$call)
  lc <- c(lc, ldot)
  if (length(ldot) == 0) {
    lc$history <- list()
  }
  z <- eval(as.call(lc), envir = globalenv())
  if (verbose){
    txt <- z$history
    txt[grepl("^\f", txt)] <- ""
    cat(txt, sep = "\n")
  }
  invisible(z$saresvions)
}



#' @rdname spc
#' @export
slidingspans <- function(x, verbose = TRUE, ...){
  ldot <- list(...)
  lc <- as.list(x$call)
  lc <- c(lc, ldot)
  if (length(ldot) == 0) {
    lc$slidingspans <- list()
  }
  z <- eval(as.call(lc), envir = globalenv())
  if (verbose){
    txt <- z$slidingspans$out
    txt[grepl("^\f", txt)] <- ""
    cat(txt, sep = "\n")
  }
  invisible(z$slidingspans)
}







#' @rdname spc
#' @export
final <- function(x){
  na_action(x, 'final')
}

#' @rdname spc
#' @export
original <- function(x){
  x$x
}

#' @rdname spc
#' @export
trend <- function(x){
  na_action(x, 'trend')
}

#' @rdname spc
#' @export
irregular <- function(x){
  na_action(x, 'irregular')
}

#' Show regressioneffects
#' 
#' matrix of regression variables multiplied by the vector of estimated 
#' regression coefficients
#' 
#' @param x  object of class "seas"
#' 
#' @export
#' @examples
#' 
#' # trading day and easter adjustment w/o seasonal adjustment:
#' x <- seas(AirPassengers)
#' 
#' summary(x)
#' re <- regressioneffects(x)
#' ce <- re[, 'Trading.Day'] + re[, 'Holiday'] 
#'  # be aware of log transformation
#' AirPassengersWoTd <- exp(log(AirPassengers) - ce)
#' 
regressioneffects <- function(x){
  x$regressioneffects
}




na_action <- function(x, name){
  z <- na.omit(x$data[, name])
  if (!is.null(x$na.action)){
    if (attr(x$na.action, "class") == "exclude") {
      z <- ts(napredict(x$na.action, z))
      tsp(z) <- tsp(original(x))
    }
  }
  z
}



#' @rdname seas
#' @export
fivebestmdl <- function(x){
  if (is.null(x$spc$automdl)){
    stop("only works with 'automdl' spc")
  }
  cat(paste(x$fivebestmdl, collapse = "\n"))
}


