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
out <- function(x){
  z <- eval(recall(x$call, output = "out"), envir = globalenv())

  cat(paste(z$out, collapse = "\n"))
  invisible(z$out)
}


recall <- function(call, output = "out"){
  stopifnot(inherits(call, "call"))
  lc <- as.list(call)
  lc$output <- output
  as.call(lc)
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
  cat(paste(x$fivebestmdl, collapse = "\n"))
}


