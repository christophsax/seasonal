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
  if (is.null(x$out)){
    stop("Contains no 'out' data. Use 'seas' with the 'out = TRUE' option.")
  }
  # remove page breaks
  #   x$output <- str_replace_all(x$output, '\\f', '')
  page(x$out)
  invisible(x$out)
}



#' @rdname spc
#' @export
final <- function(x){
  x$data[,'final']
}

#' @rdname spc
#' @export
original <- function(x){
  x$data[,'original']
}

#' @rdname spc
#' @export
trend <- function(x){
  x$data[,'trend']
}

#' @rdname spc
#' @export
irregular <- function(x){
  x$data[,'irregular']
}

