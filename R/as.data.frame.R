#' Coerce Output to \code{data.frame}
#' 
#' These methods coerce the output to a \code{data.frame}. This is useful for
#' further processing. (This is a second attempt of doing that; the first
#' experimenal attempt in version 1.4 used an argument \code{data.frame} in the
#' call to the functions, and is now obsolete. The present approach seems
#' cleaner, but still consider it as \strong{experimental}.)
#' 
#' The data.frames produced by these functions follow the nameing conventions
#' proposed by the broom package, but do not rely on the package otherwise.
#' 
#' @param x      an object of class \code{"seas"}, \code{"udg"} or \code{"summary.seas"}, usually, a result of a 
#'                    call to the functions with the same name.
#' @param ...    unused.
#' 
#' @return a \code{data.frame} without row names. 
#' @method as.data.frame seas
#' @export
#' @examples
#' \dontrun{
#' m <- seas(AirPassengers, x11 = "")
#' 
#' # a data.frame containing the M-statistics
#' as.data.frame(udg(m, paste0("f3.m", formatC(1:10, width=2, flag="0"))))
#' 
#' # a data.frame containing data
#' as.data.frame(m)
#' 
#' # a data.frame containing the summary information on the coefficients
#' as.data.frame(summary(m))
#' }
as.data.frame.seas <- function(x, ...){
  data.frame(date = Date_ts(m$data), m$data) 
}

# get 'Date' from a 'ts' object
Date_ts <- function(x){
  f <- frequency(x)
  if (f == 12){
    z <- as.Date(paste(floor(time(x)), cycle(x), "1", sep = "-"))
  } else if (f == 2){
    z <- as.Date(paste(floor(time(x)), (cycle(x) - 1) * 6, "1", sep = "-"))
  } else if (f == 4){
    z <- as.Date(paste(floor(time(x)), (cycle(x) - 1) * 3, "1", sep = "-"))
  } else {
    stop("unsupported frequency")
  }
  z
}

#' @method as.data.frame udg
#' @rdname as.data.frame.seas
#' @export
as.data.frame.udg <- function(x, ...){
  if (is.list(x)){
    x.len <- vapply(x, length, 0)

    if (length(unique(x.len)) > 1){
      most.common <- as.integer(names(table(x.len)[1]))
      message("Elements are of different length, only using elements of length ", most.common, ", which is the most common.")
      x <- simplify2array(x[x.len == most.common])
    } else {
      x <- simplify2array(x)
    }
  }
  data.frame(key = names(x), value = unclass(unname(x)), stringsAsFactors = FALSE)
}

#' @method as.data.frame summary.seas
#' @rdname as.data.frame.seas
#' @export
as.data.frame.summary.seas <- function(x, ...){
  if (is.null(x$coefficients)) return(NULL)
  df <- as.data.frame(x$coefficients)
  colnames(df) <- c("estimate", "std.error", "statistic", "p.value")
  rownames(df) <- NULL
  data.frame(term = rownames(x$coefficients), df, stringsAsFactors = FALSE)
}


