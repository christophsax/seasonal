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

  z0 <- final(x)
  z0[] <- NA
  
  # if there are no outlier, return a time series with NAs
  if (length(ol) == 0){
    return(z0)
  }
  
  stopifnot(inherits(ol, "character"))
  
  ol.type <- substr(ol, start = 1, stop = 2)
  ol.time <- substr(ol, start = 3, stop = nchar(ol))

  # if a range ("2001.3-2004.3"), only use the start date ("2001.3")
  is.range <- grepl("-", ol.time, fixed = TRUE)
  if (any(is.range)){
    ol.time[is.range] <- gsub("-.+$", "", ol.time[is.range])
  }
  
  if (frequency(z0) == 12){
    ol.time.R <- lapply(strsplit(ol.time, "\\."), 
                        function(el) {c(el[1], which(month.abb == el[2]))}
    )
  } else if (frequency(z0) %in% c(4, 2)){
    ol.time.R <- lapply(strsplit(ol.time, "\\."), 
                        function(el) {c(el[1], (el[2]))}
    )
  } else {
    stop("Frequency not supported: ", frequency(z0))
  }

  stopifnot(length(ol.time.R) == length(ol))

  # shoiuld only the type of the outlier be shown?
  if (!full) ol <- toupper(ol.type)

  ol.time.R <- lapply(ol.time.R, as.numeric)
  ol.ts <- Map(function(value, time.stamp) {
                      ts(value, start = time.stamp, frequency = frequency(z0))
                    }, 
                    value = as.list(ol),
                    time.stamp = ol.time.R)

  z <- window(do.call(ts.union, ol.ts), 
              start = start(z0), end = end(z0), extend = TRUE)
  z
}
