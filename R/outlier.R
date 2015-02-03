#' Outlier Time series
#' 
#' Returns an object of class \code{"ts"} that contains the names of the
#' outliers.
#' 
#' @param x      an object of class \code{"seas"}.
#' @param full   logical, should the full label of the outlier be shown? If
#'   \code{FALSE}, only the type of the outlier is shown.
#'   
#' @return character string time series with outliers.
#'   
#' @export
#' @examples
#' \dontrun{
#' x <- seas(AirPassengers)
#' outlier(x)
#' }
outlier <- function(x, full = FALSE){
  stopifnot(inherits(x, "seas"))

  # outlier detection: regressors with a . but no /
  ol <- x$model$regression$variables[grepl("\\.", x$model$regression$variables)]
  ol <- ol[!grepl("\\/", ol)]

  z <- final(x)
  z[1:length(z)] <- NA
  
  # if there are no outlier, return a time series with NAs
  if (length(ol) == 0){
    return(z)
  }
  
  stopifnot(inherits(ol, "character"))
  
  ol.type <- substr(ol, start = 1, stop = 2)
  ol.time <- substr(ol, start = 3, stop = nchar(ol))
  
  if (frequency(z) == 12){
    ol.time.R <- lapply(strsplit(ol.time, "\\."), 
                        function(el) {c(el[1], which(month.abb == el[2]))}
    )
  } else if (frequency(z) == 4){
    ol.time.R <- lapply(strsplit(ol.time, "\\."), 
                        function(el) {c(el[1], (el[2]))}
    )
  }
  
  stopifnot(length(ol.time.R) == length(ol))

  for (i in 1:length(ol.time.R)){
    if (!full){
      window(z, start = as.numeric(ol.time.R[[i]]), 
             end = as.numeric(ol.time.R[[i]])) <- toupper(ol.type[i])
    } else {
      window(z, start = as.numeric(ol.time.R[[i]]), 
             end = as.numeric(ol.time.R[[i]])) <- ol[i]
    }
  }
  
  z
}
