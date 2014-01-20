#' Generate Holiday Regression Variables
#' 
#' This is a replacement for genhol, a utility that uses the same procedure as 
#' X-12-ARIMA to create regressors for the U. S. holidays of Easter, Labor Day, 
#' and Thanksgiving.
#' 
#' The resulting time series can be used as a user defined variable in
#' \code{\link{seas}}. Usually, you want the holiday effect to be removed from
#' the final series, so you need to specify \code{regression.usertype =
#' "holiday"}. (The default is to include user defined variables in the final
#' series.)
#' 
#' @param start   integer, shift start of the holiday. Use negative values if
#'   start is before the specified date.
#' @param end   integer, shift end of the holiday. Use negative values if start
#'   is before the specified date.
#' @param frequency  integer, frequency of the resulting series
#' @param center   character string. Either \code{"calendar"}, \code{"mean"} or
#'   \code{"none"} (default). Centering avoids a bias in the resultign series.
#'   Use \code{"calendar"} for Easter or Chinese New Year, \code{"mean"} for
#'   Ramadan. See references.
#' @return an object of class \code{"ts"} that can be used as a user defined
#'   variable in \code{\link{seas}}.
#'   
#' @references United States Census Bureau, Notes on centering holiday
#'   regressors: \url{http://www.census.gov/srd/www/genhol/genhol_center.html}
#'   
#'   X-13ARIMA-SEATS manual: \url{http://www.census.gov/ts/x13as/docX13AS.pdf}
#'   
#' @export
#' @examples
#' 
#' data(holiday)
#' 
#' 
#' ### using genhol
#' 
#' # 10 day before Easter day to one day after, quarterly data:
#' genhol(easter, start = -10, end = 1, frequency = 4) 
#' genhol(easter, frequency = 2)  # easter is allways in the first half-year
#' 
#' # centering for overall mean or monthly calendar means
#' genhol(easter, center = "mean") 
#' genhol(easter, center = "calendar") 
#' 
#' 
#' ### replicating X13's integrated easter adjustment
#' 
#' # integrated adjustment
#' m1 <- seas(x = AirPassengers,
#' regression.variables = c("td1coef", "easter[1]", "ao1951.May"),
#' arima.model = "(0 1 1)(0 1 1)", regression.aictest = NULL,
#' outlier = NULL, transform.function = "log")
#' summary(m1)
#' 
#' # user defined easter variable
#' ea1 <- genhol(easter, start = -1, end = -1, center = "calendar")
#' 
#' # regression.usertype = "holiday" ensures that the effect is removed from 
#' # the final series.
#' m2 <- seas(x = AirPassengers,
#'            regression.variables = c("td1coef", "ao1951.May"),
#'            xreg = ea1, regression.usertype = "holiday",
#'            arima.model = "(0 1 1)(0 1 1)", regression.aictest = NULL,
#'            outlier = NULL, transform.function = "log")
#' summary(m2)
#' 
#' all.equal(final(m2), final(m1), tolerance = 1e-06)
#' 
#' 
#' # with genhol, its possible to do sligtly better, by adjusting to the length
#' # of easter
#' 
#' ea2 <- genhol(easter, start = -2, end = +1, center = "calendar")
#' m3 <- seas(x = AirPassengers, 
#'            regression.variables = c("td1coef", "ao1951.May"), 
#'            xreg = ea2,
#'            arima.model = "(0 1 1)(0 1 1)", regression.aictest = NULL, 
#'            outlier = NULL, transform.function = "log")
#' summary(m3)
#' 
#' 
#' ### Chinese New Year
#' 
#' data(exports)  # Swiss exports to China
#' data(holiday)
#' 
#' # exact specification 
#' # (de facto holiday length: http://en.wikipedia.org/wiki/Chinese_New_Year)
#' cny1 <- genhol(cny, start = 0, end = 6, center = "calendar")
#' m1 <- seas(exports, xreg = cny1, regression.usertype = "holiday",
#'            regression.aictest = NULL,
#'            regression.variables = c("td1coef", "easter[1]")
#' )
#' summary(m1)
#' 
#' # AICC minimzation (same idea as auto Easter adjustment in X-13ARIMA-SEATS)
#' cny2 <- genhol(cny, start = 0, end = 15, center = "calendar")
#' m2 <- seas(exports, xreg = cny2, regression.usertype = "holiday",
#'            regression.aictest = NULL,
#'            regression.variables = c("td1coef", "easter[1]")
#' )
#' summary(m2)
#' 
#' # compare to no-CNY model:
#' m3 <- seas(exports,
#'            regression.aictest = NULL,
#'            regression.variables = c("td1coef", "easter[1]")
#' )
#' 
#' # effects are visible, but it's not a whole new story
#' ts.plot(diff(log((cbind(final(m2), final(m3))))), col = c("red", "black"))
#' 
genhol <- function(x, start = 0, end = 0, frequency = 12, center = "none"){
  if (!inherits(x, "Date")){
    stop("x must be of class 'Date'. Use 'as.Date' to convert.")
  }
  
  if (!center %in% c("none", "calendar", "mean")){
    stop("wrong center argument. Use 'mean', 'calendar' or 'none'.")
  }

  event.st <- x + start
  event.en <- x + end
  frequency <- frequency
  
  if ((end - start) > 150) {
    stop("holiday length must =< 150")
  }
  
  # empty series, starting at Jan 1 of the first year
  z.ts <- ts(NA, 
          start = as.numeric(format(event.st[1], "%Y")),
          end = c(as.numeric(format(event.en[length(event.en)], "%Y")), frequency),
          frequency = frequency)
  
  # Dates for beginning and end of each period
  by <- switch(as.character(frequency), 
               "12" = "month", 
               "4" = "3 month", 
               "2" = "6 month",
               "1" = "year")

  period.st <- seq(from = as.Date(paste0(format(event.st[1], "%Y"), "/1/1")), 
                    by = by, length.out = length(z.ts))
  
  suffix <- switch(as.character(frequency), 
                   "12" = "/2/1", 
                   "4" = "/4/1", 
                   "2" = "/7/1")
  
  period.en <- seq(from = as.Date(paste0(format(event.st[1], "%Y"), suffix)), 
                  by = by, length.out = length(z.ts)) - 1
  
  # add first of january as NA if not already there (needed for cut)
  first.day <- as.Date(paste0(start(z.ts)[1], "/1/1"))  
  if (!first.day %in% event.st){
    event.st.added <- c(first.day, event.st)
    event.st <- c(as.Date(NA), event.st)
  }
  if (!first.day %in% event.en){
    event.en.added <- c(first.day, event.en)
    event.en <- c(as.Date(NA), event.en)
  }
  
  # "ts" object with each date in the right period
  event.st.ts <- z.ts
  event.st.ts[cut(event.st.added, by, labels = F)] <- as.character(event.st) 
  
  event.en.ts <- z.ts
  event.en.ts[cut(event.en.added, by, labels = F)] <- as.character(event.en)
  
  # number of days
  days <- pmin((period.en), as.Date(as.character(event.en.ts)), na.rm = T) - 
    pmax(period.st, as.Date(as.character(event.st.ts)), na.rm = T) + 1
  
  # filling NAs with start and end dates
  fillNA <- function(x){
    x.span <- x
    x.i <- NA
    for (i in 1:length(x.span)){
      if (!is.na(x.span[i])){
        x.i <- x.span[i]
      } else {
        x.span[i] <- x.i
      }
    }
    x.span
  }
  
  # drop 
  drop <- is.na(event.st.ts) & is.na(event.en.ts)
  
  # dont drop these (start value larger than end value)
  drop[fillNA(event.st.ts) > fillNA(event.en.ts)] <- FALSE

  days[drop] <- 0
  z.raw <- ts(c(days), start = start(z.ts), frequency = frequency(z.ts))
  z.raw <- z.raw / (end - start + 1)
  
  if (center == "mean"){
    z <- z.raw - mean(z.raw)
  } else if (center == "calendar"){
    z.mat <- t(matrix(z.raw, ncol = frequency(z.raw), byrow = TRUE))
    z.mat <- z.mat - rowMeans(z.mat)
    z <- ts(as.numeric((z.mat)), start = start(z.raw), frequency = frequency(z.raw))
  } else {
    z <- z.raw
  }
  
  z
}



