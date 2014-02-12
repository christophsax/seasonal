#' seasonal: R interface to X-13ARIMA-SEATS
#' 
#' @description seasonal is an easy-to-use R-interface to X-13ARIMA-SEATS, a
#'   seasonal adjustment software developed by the United States Census
#'   Bureau. X-13ARIMA-SEATS combines and extends the capabilities of
#'   the older X-12ARIMA (developed by the Census Bureau) and the TRAMO-SEATS
#'   (developed by the Bank of Spain) software packages.
#'   
#'   The best way to start is to have a look at the vignette:
#'   
#'   \code{vignette("seas")}
#'   
#' @name seasonal-package
#' @aliases seasonal
#' @docType package
#' @author Christoph Sax \email{christoph.sax@@gmail.com}
#' @keywords package
#' @seealso \code{\link{seas}} for more information on usage.
NULL



#' Consumer Price Index of Switzerland
#' 
#' Monthly consumer price index of Switzerland. Base year is 1993.
#' 
#' @docype data
#' 
#' @format Each time series is an object of class \code{"ts"}.
#' 
#' @source Swiss Federal Statistical Office 
#' \url{http://www.bfs.admin.ch/bfs/portal/de/index/themen/05/02/blank/key/basis_aktuell.html} 
#' (cc-d-05.02.12)
#' 
#' @name cpi
#' @keywords datasets
NULL



#' Dates of Easter and Chinese New Year
#' 
#' @docype data
#' 
#' @format Objects of class \code{"Date"}.
#' 
#' @source 
#' \url{http://www.census.gov/srd/www/genhol/genhol_downloads.html} 
#' 
#' \url{http://www.chinesenewyears.info/chinese-new-year-calendar.php} 
#' 
#' @name easter
#' @aliases cny
#' @keywords datasets
NULL


#' Exports and Imports of China
#' 
#' Monthly exports and imports of China (July 1983 to December 2013). 
#' 
#' In 100 mio. U.S. Dollar.
#' 
#' @docype data
#' 
#' @format Each time series is an object of class \code{"ts"}.
#' 
#' @source China Customs
#' 
#' @name exp
#' @aliases imp
#' @keywords datasets
NULL

