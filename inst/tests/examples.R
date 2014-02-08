# --- static --------------------------------------------------------------


m <- seas(AirPassengers)
static(m)
static(m, test = FALSE)


# --- out --------------------------------------------------------------

x <- seas(AirPassengers)
outlier(x)

slidingspans(m)
revisions(m)

# plot method for slingspans and revisions (see ?plot.seas)
plot(slidingspans(m))
plot(revisions(m))

# --- final --------------------------------------------------------------


m <- seas(AirPassengers)

final(m)
original(m)
irregular(m)
trend(m)
regressioneffects(m)

# trading day and easter adjustment w/o seasonal adjustment
summary(m)
re <- regressioneffects(m)
ce <- re[, 'Trading.Day'] + re[, 'Holiday']
# be aware of the log transformation
AirPassengersWoTd <- exp(log(AirPassengers) - ce)

# NA handling
AirPassengersNA <- window(AirPassengers, end = 1962, extend = TRUE)
final(seas(AirPassengersNA, na.action = na.omit))    # no NA in final series
final(seas(AirPassengersNA, na.action = na.exclude)) # NA in final series
# final(seas(AirPassengersNA, na.action = na.fail))    # fails



# --- checkX13 --------------------------------------------------------------
checkX13()



# --- qs --------------------------------------------------------------

m <- seas(AirPassengers)

qs(m)
spc(m)
fivebestmdl(m)
arimamodel(m)

# if no automdl spec is present, the model is re-evaluated
m2 <- seas(AirPassengers, arima.model = "(0 1 1)(0 1 1)")
spc(m2)           # arima overwrites the automdl spec
fivebestmdl(m2)   # re-evaluation with automdl

# more diagnostical statistics with R functions
shapiro.test(resid(m))  # no rejection of normality
Box.test(resid(m), lag = 24, type = "Ljung-Box")  # no auto-correlation

# accessing the full output (see ?out)
out(m)
out(m, search = "Ljung-Box")


# --- genhol --------------------------------------------------------------

data(holiday)  # Dates of Chinese New Year and Easter

### use of genhol

# 10 day before Easter day to one day after, quarterly data:
genhol(easter, start = -10, end = 1, frequency = 4)
genhol(easter, frequency = 2)  # easter is allways in the first half-year

# centering for overall mean or monthly calendar means
genhol(easter, center = "mean")
genhol(easter, center = "calendar")

### replicating X-13's built-in Easter adjustment

# built-in
m1 <- seas(x = AirPassengers,
           regression.variables = c("td1coef", "easter[1]", "ao1951.May"),
           arima.model = "(0 1 1)(0 1 1)", regression.aictest = NULL,
           outlier = NULL, transform.function = "log")
summary(m1)

# user defined variable
ea1 <- genhol(easter, start = -1, end = -1, center = "calendar")

# regression.usertype = "holiday" ensures that the effect is removed from
# the final series.
m2 <- seas(x = AirPassengers,
           regression.variables = c("td1coef", "ao1951.May"),
           xreg = ea1, regression.usertype = "holiday",
           arima.model = "(0 1 1)(0 1 1)", regression.aictest = NULL,
           outlier = NULL, transform.function = "log")
summary(m2)

all.equal(final(m2), final(m1), tolerance = 1e-06)


# with genhol, its possible to do sligtly better, by adjusting the length
# of easter

ea2 <- genhol(easter, start = -2, end = +1, center = "calendar")
m3 <- seas(x = AirPassengers,
           regression.variables = c("td1coef", "ao1951.May"),
           xreg = ea2, regression.usertype = "holiday",
           arima.model = "(0 1 1)(0 1 1)", regression.aictest = NULL,
           outlier = NULL, transform.function = "log")
summary(m3)


### Chinese New Year

data(cnprod)    # industrial production of China
data(holiday)

# de facto holiday length: http://en.wikipedia.org/wiki/Chinese_New_Year
cny1 <- genhol(cny, start = 0, end = 6, center = "none")
m1 <- seas(x = cement,
           xreg = cny1, regression.usertype = "holiday",
           regression.variables = c("ao1998.Feb", "ao2001.Feb", "ao2004.Jan",
                                    "ao2007.Mar"),
           arima.model = "(1 1 1)(0 1 1)", regression.aictest = NULL,
           outlier = NULL, transform.function = "log")

# compare to no-CNY model:
m2 <- seas(x = cement,
           regression.variables = c("ao1998.Feb", "ao2001.Feb", "ao2004.Jan",
                                    "ao2007.Mar"),
           arima.model = "(1 1 1)(0 1 1)", regression.aictest = NULL,
           outlier = NULL, transform.function = "log")

ts.plot(diff(log((cbind(final(m1), final(m2))))), col = c("red", "black"))

