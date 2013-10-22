
#' A string time series with outliers
#' 
#' @param x time series template
#' @param outlier character string with outliers
#' 
#' @export
#' @examples
#' OutlierTimeSeries(AirPassengers, c("ao1951.May", "ao1954.Feb", "ls1956.Feb"))
#' OutlierTimeSeries(austres, c("ao1981.1", "ao1984.2", "ls1986.4"))
#' OutlierTimeSeries(austres, character())
#' 
OutlierTimeSeries <- function(x, outlier){
  stopifnot(inherits(x, "ts"))
  
  z <- x
  z[1:length(z)] <- NA
  
  # if there are no outlier, return a time series with NAs
  if (length(outlier) == 0){
    return(z)
  }
  
  stopifnot(inherits(outlier, "character"))
  
  ol.type <- str_sub(outlier, start = 1, end = 2)
  ol.time <- str_sub(outlier, start = 3, end = -1)
  
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
  
  stopifnot(length(ol.time.R) == length(outlier))
  
  for (i in 1:length(ol.time.R)){
    window(z, start = as.numeric(ol.time.R[[i]]), 
           end = as.numeric(ol.time.R[[i]])) <- toupper(ol.type[i])
  }
  
  z
}
