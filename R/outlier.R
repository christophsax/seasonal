#' Outlier Time series
#' 
#' Returns an object of class \code{"ts"} that contains the names of the outliers.
#' 
#' @param x   an object of class \code{"seas"}.
#' 
#' @return character string time series with outliers.
#' 
#' @export
#' @examples
#' \dontrun{
#' x <- seas(AirPassengers)
#' outlier(x)
#' }
outlier <- function(x){
  stopifnot(inherits(x, "seas"))
  
  ol <- x$mdl$regression$variables[str_detect(x$mdl$regression$variables, "\\.")]
  
  z <- final(x)
  z[1:length(z)] <- NA
  
  # if there are no outlier, return a time series with NAs
  if (length(ol) == 0){
    return(z)
  }
  
  stopifnot(inherits(ol, "character"))
  
  ol.type <- str_sub(ol, start = 1, end = 2)
  ol.time <- str_sub(ol, start = 3, end = -1)
  
  if (frequency(z) == 12){
    MonthToNum <- function(x){
      switch(x, "Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4, "May" = 5, "Jun" = 6,
             "Jul" = 7, "Aug" = 8, "Sep" = 9, "Oct" = 10, "Nov" = 11, "Dec" = 12)
    }
    
    ol.time.R <- lapply(strsplit(ol.time, "\\."), 
                        function(el) {c(el[1], MonthToNum(el[2]))}
    )
  } else if (frequency(z) == 4){
    
    ol.time.R <- lapply(strsplit(ol.time, "\\."), 
                        function(el) {c(el[1], (el[2]))}
    )
  }
  
  stopifnot(length(ol.time.R) == length(ol))
  
  for (i in 1:length(ol.time.R)){
    window(z, start = as.numeric(ol.time.R[[i]]), 
           end = as.numeric(ol.time.R[[i]])) <- toupper(ol.type[i])
  }
  
  z
}
