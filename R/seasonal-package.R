#' seasonal: R interface to X-13ARIMA-SEATS
#'
#' @description
#'   \pkg{seasonal} is an asy-to-use interface to X-13-ARIMA-SEATS, the seasonal
#'   adjustment software by the US Census Bureau. It offers full access to
#'   almost all options and outputs of X-13, including X-11 and SEATS, automatic
#'   ARIMA model search, outlier detection and support for user defined holiday
#'   variables, such as Chinese New Year or Indian Diwali. A graphical user
#'   interface can be used through the \pkg{seasonalview} package. Uses the
#'   X-13-binaries from the \pkg{x13binary} package.
#'
#'   The best way to start is to have a look at the vignette:
#'
#'   `vignette("seas")`
#'
#' @section Installation:
#' Seasonal depends on the \pkg{x13binary} package, which downloads and installs
#' the X-13 binaries. To install both packages, simply type to the R
#' console:
#'
#'   `install.packages("seasonal")`
#'
#' A startup message is given if the path to X-13 is specified manually. To
#' surpress the message, use [suppressPackageStartupMessages()].
#'
#' @section Setting the X-13 path manually:
#'
#' Sometimes, you either cannot or don't want to rely on the binaries provided
#' by \pkg{x13binary}:
#' \itemize{
#' \item because you are on an unsupported system, like Solaris. If you manage
#'   to build X-13 on such a system, please let the developers of
#'   \pkg{x13binary} know.
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
#' `Sys.setenv(X13_PATH = "YOUR_X13_DIRECTORY")`
#'
#' Exchange `YOUR_X13_DIRECTORY` with the path to your installation of
#' X-13ARIMA- SEATS. You can always check your installation with:
#'
#' `checkX13()`
#'
#' If it works, you may want to set the environmental variable permanently, by
#' adding the Sys.setenv line to one of your `.Rprofile` files. The easiest
#' is to use the one located in your home directory, which can be written
#' directly from R:
#'
#' `write('Sys.setenv(X13_PATH = "YOUR_X13_DIRECTORY")',
#'       file = "~/.Rprofile", append = TRUE)`
#'
#' If the file does not exist (by default), it will be created. Make sure that
#' you get the quotes right: double quotes around your directory, single quotes
#' around the whole `Sys.setenv` line, such that R understands your string.
#' Check first that the the `Sys.setenv` line works correctly; once it is
#' written you may have to edit `.Rprofile` manually. (Or add a second,
#' overwriting line to it.) For other ways to set an environmental variable
#' permanently in R, see [Startup()].
#'
#' @references
#'   Sax C, Eddelbuettel D (2018). "Seasonal Adjustment by X-13ARIMA-SEATS
#'   in R." *Journal of Statistical Software*, *87*(11), 1-17. doi:
#'   10.18637/jss.v087.i11 (<http://doi.org/10.18637/jss.v087.i11>).
#'
#' @name seasonal-package
#' @aliases seasonal
#' @docType package
#' @author Christoph Sax \email{christoph.sax@@gmail.com}
#' @keywords package
#' @seealso [seas()] for the core function and more information on
#'   package usage.
NULL



#' Consumer Price Index of Switzerland
#'
#' Monthly consumer price index of Switzerland. Base year is 1993.
#'
#' @docType data
#'
#' @format Time series of class `"ts"`.
#'
#' @source Federal Statistical Office, Switzerland
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
#' [genhol()].
#'
#' @docType data
#'
#' @format Objects of class `"Date"`.
#'
#' @source
#' <https://www.census.gov/srd/www/genhol/genhol_downloads.html>
#'
#' <http://www.chinesenewyears.info/chinese-new-year-calendar.php>
#'
#' Ministry of Statistics and Programme Implementation, with help from Pinaki
#'   Mukherjee
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
#' @format Time series of class `"ts"`.
#'
#' @source Central Statistics Office of the Ministry of Statistics and Programme
#' Implementation, with help
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
#' @format Each time series is an object of class `"ts"`.
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


#' United States Unemployment Level
#'
#' Thousands of Persons
#'
#' @docType data
#'
#' @format Each time series is an object of class `"ts"`.
#'
#' @source U.S. Bureau of Labor Statistics, retrieved from FRED, Federal Reserve
#'   Bank of St. Louis; <https://fred.stlouisfed.org/series/LNU03000000>,
#'   December 14, 2016.
#'
#' @name unemp
#' @keywords datasets
#' @examples
#'
#' data(seasonal)
#' unemp
NULL


#' List of Available X-13ARIMA-SEATS Outputs
#'
#' The data is used by several functions as a look-up table. Users
#' should consider the table in [series()] or in the official manual.
#'
#' @docType data
#'
#' @format An object of class `"data.frame"`
#'
#' @source United States Census Bureau
#'
#' @references Official X-13ARIMA-SEATS manual:
#'   <https://www.census.gov/ts/x13as/docX13ASHTML.pdf>
#'
#' @name SPECS
#' @aliases INSPDATA
#' @keywords datasets
NULL
