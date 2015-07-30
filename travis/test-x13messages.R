
## X-13 terminates with errors
seas(ts(0, start = 2000, end = 2010, frequency = 4))

seas(ts(9999999999999999, start = 2000, end = 2010, frequency = 4))


## X-13 does not terminate but returns no data
seas(ts(-9999999999999999, start = 2000, end = 2010, frequency = 4))


## Multiple Errors
seas(x = AirPassengers, series.comptype = "add", series.span = "1984.1,", 
     transform.function = "log", arima.model = "(0 1 1 )(0 1 1)", 
     regression.variables = c("ao1985.1", "ao1985.2"), estimate = "", 
     check.print = "all", check.savelog = "lbq", x11.calendarsigma = "select", 
     x11.sigmavec = c("jan", "feb"), x11.seasonalma = c("s3x5", 
     "s3x5", "s3x9", "s3x9", "s3x9", "s3x9", "s3x9", "s3x9", "s3x9", 
     "s3x9", "s3x9", "s3x5"), x11.sigmalim = c(1.8, 2.8), x11.savelog = c("m7", 
     "m10", "m11", "q2"), slidingspans.fixmdl = "yes", slidingspans.savelog = "percents", 
     slidingspans.cutseas = 4.5, slidingspans.cutchng = 4.5, history.estimates = c("sadj", 
     "sadjchng", "aic", "fcst"), history.start = 1996.1, history.savelog = c("asa", 
     "ach", "asf"), automdl = NULL, outlier = NULL, regression.aictest = NULL)


seas(
x = AirPassengers,
regression.aictest = NULL,
regression.variables = c("easter[1]", "td"),
arima.model = "(5 1 0)(0 1 1)"
)

seas(
x = AirPassengers,
outlier.critical = 5,
arima.model = c(0, 1, 1, 4),
x11 = ""
)



m <- seas(
x = AirPassengers,
transform.function = "none",
regression.aictest = "easter",
outlier.critical = 3,
arima.model = "(0 1 0)",
regression.variables = "td1coef"
)
monthplot(m)






