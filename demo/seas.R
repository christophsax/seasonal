require(seasonal)


# 'seas' ist the core function of the seasonal package. By default, 'seas'
# calls the automatic procedures of X-13ARIMA-SEATS to perform a seasonal
# adjustment that works well in most circumstances. It returns an object of
# class "seas" that contains all necessary information on the adjustment
# process, as well as the series.

m <- seas(AirPassengers)

# The first argument of 'seas' has to be a time series of class "ts". It
# retruns an object of class 'seas'. There are several functions and methods for
# 'seas' objects: The 'final' function returns the adjusted series, the 'plot'
# method shows a plot with the unadjusted and the adjusted series. The 'summary'
# method allows you to display an overview of the model:

plot(m)
final(m)
plot(resid(m))
summary(m)

# By default, 'seas' calls the SEATS adjustment procedure. If you prefer the X11
# adjustment procedure, use the following option:
  
seas(AirPassengers, x11 = list())
  
# Alternatively, all inputs may be entered manually, as in the following
# example:
    
seas(x = AirPassengers, 
     regression.variables = c("td1coef", "easter[1]", "ao1951.May"), 
     arima.model = "(0 1 1)(0 1 1)", regression.aictest = NULL,
     outlier = NULL, transform.function = "log")
  
# The 'static' command reveals the static call from above that is needed to
# replicate the automatic seasonal adjustment procedure:
    
static(m)
  
# invoke X-13ARIMA-SEATS options as 'spec.argument' through the ... argument
seas(AirPassengers, regression.aictest = c("td"))  # no easter testing
seas(AirPassengers, force.type = "denton")  # force equality of annual values

# all series from X-13 can be extracted with series()
ts.plot(series(m, "forecast.forecasts"))

# use genhol to generate user defined holiday variables:
data(cntrade)  # exports and imports of China
data(holiday)  # dates of Chinese New Year and Easter
cny.ts <- genhol(cny, start = 0, end = 6, center = "calendar")

# the series can be used as a user defined regressor
plot(cny.ts)

seas(imp, xreg = cny.ts, regression.usertype = "holiday", x11 = list(),
     regression.variables = c("td1coef", "ls1985.Jan", "ls2008.Nov"),
     arima.model = "(0 1 2)(0 1 1)", regression.aictest = NULL,
     outlier = NULL, transform.function = "log")

