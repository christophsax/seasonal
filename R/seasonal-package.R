#' seasonal: R interface to X-13ARIMA-SEATS
#' 
#' @description seasonal is an easy-to-use and (almost) full-featured 
#'   R-interface to X-13ARIMA-SEATS, the newest seasonal adjustment software 
#'   developed by the United States Census Bureau. X-13ARIMA-SEATS combines and 
#'   extends the capabilities of the older X-12ARIMA (developed by the Census 
#'   Bureau) and TRAMO-SEATS (developed by the Bank of Spain).
#'   
#'   The best way to start is to have a look at the vignette:
#'   
#'   \code{vignette("seas")}
#'   
#' @section Installation:
#' Seasonal depends on the \pkg{x13binary} package, which downloads and installs
#' the X-13 binaries. To install both packages, simply type to the R
#' console:
#' 
#'   \code{install.packages("seasonal")}
#'
#' A startup message is given if the path to X-13 is specified manually. To
#' surpress the message, use \code{\link{suppressPackageStartupMessages}}.
#'
#' @section Setting the X-13 path manually:
#' 
#' Sometimes, you either cannot or don't want to rely on the binaries provided
#' by \pkg{x13binary}:
#' \itemize{
#' \item because you are on an unsupported system, like Solaris.
#' \item because you cannot run executable files in your R library folders, due 
#' to corporate IT policy.
#' \item because you are using your own Fortran compilation of X-13ARIMA-SEATS.
#' }
#' 
#' Setting the path manually can be done as in previous versions of seasonal. In
#' order to tell seasonal where to find the binary executables of X-13ARIMA-
#' SEATS, the specific environmental variable X13_PATH needs to be set. This may
#' be done during your active session in R:
#' 
#' \code{Sys.setenv(X13_PATH = "YOUR_X13_DIRECTORY")}
#' 
#' Exchange \code{YOUR_X13_DIRECTORY} with the path to your installation of
#' X-13ARIMA- SEATS. You can always check your installation with:
#'
#' \code{checkX13()}
#'
#' If it works, you may want to set the environmental variable permanently, by
#' adding the Sys.setenv line to one of your \code{.Rprofile} files. The easiest
#' is to use the one located in your home directory, which can be written
#' directly from R:
#'
#' \code{write('Sys.setenv(X13_PATH = "YOUR_X13_DIRECTORY")', 
#'       file = "~/.Rprofile", append = TRUE)}
#'
#' If the file does not exist (by default), it will be created. Make sure that
#' you get the quotes right: double quotes around your directory, single quotes
#' around the whole \code{Sys.setenv} line, such that R understands your string.
#' Check first that the the \code{Sys.setenv} line works correctly; once it is
#' written you may have to edit \code{.Rprofile} manually. (Or add a second,
#' overwriting line to it.) For other ways to set an environmental variable
#' permanently in R, see \code{\link{Startup}}.
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
#' @docType data
#' 
#' @format Time series of class \code{"ts"}.
#' 
#' @source Federal Statistical Office, Switzerland 
#' \url{http://www.bfs.admin.ch/bfs/portal/de/index/themen/05/02/blank/key/basis_aktuell.html} 
#' (cc-d-05.02.12)
#' 
#' @name cpi
#' @keywords datasets
#' @examples
#' 
#' data(seasonal)
#' cpi
NULL



#' Dates of Chinese New Year, Indian Diwali and Easter
#' 
#' Dates of Chinese New Year, Indian Diwali and Easter, suitable for the use in 
#' \code{\link{genhol}}.
#'
#' @docType data
#' 
#' @format Objects of class \code{"Date"}.
#' 
#' @source 
#' \url{http://www.census.gov/srd/www/genhol/genhol_downloads.html} 
#' 
#' \url{http://www.chinesenewyears.info/chinese-new-year-calendar.php} 
#'
#' \url{http://india.gov.in/calendar}, with help from Pinaki Mukherjee
#' 
#' @name easter
#' @aliases cny diwali
#' @keywords datasets
#' @examples
#' 
#' data(holiday)
#' cny
#' diwali
#' easter
NULL


#' Industrial Production of India
#' 
#' Industrial Production of India (IIP). 
#' 
#' Index value. IIP is used for measuring the performance overall industrial 
#' sector of the Indian economy. IIP is compiled by using data from 16 source 
#' agencies.
#' 
#' @docType data
#' 
#' @format Time series of class \code{"ts"}.
#' 
#' @source Central Statistics Office of the Ministry of Statistics and Programme 
#' Implementation 
#' \url{http://mospi.nic.in/Mospi_New/upload/iip/table3_04_05.xls}, with help 
#' from Pinaki Mukherjee
#' 
#' @name iip
#' @keywords datasets
#' @examples
#' 
#' data(seasonal)
#' iip
NULL


#' Exports and Imports of China
#' 
#' Monthly exports and imports of China (July 1983 to December 2013). 
#' 
#' In 100 mio. U.S. Dollar.
#' 
#' @docType data
#' 
#' @format Each time series is an object of class \code{"ts"}.
#' 
#' @source China Customs
#' 
#' @name exp
#' @aliases imp
#' @keywords datasets
#' @examples
#' 
#' data(seasonal)
#' imp
#' exp
NULL


#' List of Available X-13ARIMA-SEATS Outputs
#' 
#' The data is used by several functions as a look-up table. Users
#' should consider the table in \code{\link{series}} or in the official manual.
#' 
#' @docType data
#'   
#' @format An object of class \code{"data.frame"}
#'   
#' @source United States Census Bureau
#'   
#' @references Official X-13ARIMA-SEATS manual: 
#'   \url{http://www.census.gov/ts/x13as/docX13AS.pdf}
#'   
#' @name SPECS
#' @aliases INSPDATA
#' @keywords datasets
NULL
