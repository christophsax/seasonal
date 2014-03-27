#' seasonal: R interface to X-13ARIMA-SEATS
#' 
#' @description seasonal is an easy-to-use and (almost) full-featured 
#'   R-interface to X-13ARIMA-SEATS, the newest seasonal adjustment software 
#'   developed by the United States Census Bureau. X-13ARIMA-SEATS combines and 
#'   extends the capabilities of the older X-12ARIMA (developed by the Census 
#'   Bureau) and TRAMO-SEATS (developed by the Bank of Spain).
#'   
#'   The best way to start is to have a look at the vignette, where installation
#'   and usage is described.
#'   
#'   \code{vignette("seas")}
#'   
#' @name seasonal-package
#' @aliases seasonal
#' @docType package
#' @author Christoph Sax \email{christoph.sax@@gmail.com}
#' @keywords package
#' @seealso \code{\link{seas}} for the core function and more information on
#'   package usage.
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



#' List of Available X-13ARIMA-SEATS Outputs
#' 
#' The data is used by several functions as a look-up table. Users
#' should consider the table in \code{\link{series}} or in the official manual.
#' 
#' @docype data
#'   
#' @format An object of class \code{"data.frame"}.
#'   
#' @source United States Census Bureau
#'   
#' @references Official X-13ARIMA-SEATS manual: 
#'   \url{http://www.census.gov/ts/x13as/docX13AS.pdf}
#'   
#' @name SPECS
#' @aliases SPECS
#' @keywords datasets
NULL
