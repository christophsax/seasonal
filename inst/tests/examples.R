pkgname <- "seasonal"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('seasonal')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("checkX13")
### * checkX13

flush(stderr()); flush(stdout())

### Name: checkX13
### Title: Check the installation of X-13ARIMA-SEATS
### Aliases: checkX13

### ** Examples

## Not run: 
old.path <- Sys.getenv("X13_PATH")
Sys.setenv(X13_PATH = "")  # its broken now
checkX13()

Sys.setenv(X13_PATH = old.path)  # fix it (provided it worked in the first place)
checkX13()
## End(Not run)



cleanEx()
nameEx("final")
### * final

flush(stderr()); flush(stdout())

### Name: final
### Title: Time Series from a Seasonal Adjustment Model
### Aliases: final irregular original regressioneffects residuals.seas
###   trend

### ** Examples

## Not run: 

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
## End(Not run)



cleanEx()
nameEx("genhol")
### * genhol

flush(stderr()); flush(stdout())

### Name: genhol
### Title: Generate Holiday Regression Variables
### Aliases: genhol

### ** Examples

## Not run: 

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

m1 <- seas(x = cement, xreg = cny1, regression.usertype = "holiday")

# compare to no-CNY model:
m2 <- seas(x = cement)

ts.plot(final(m1), final(m2), col = c("red", "black"))
## End(Not run)



cleanEx()
nameEx("inspect")
### * inspect

flush(stderr()); flush(stdout())

### Name: inspect
### Title: Interactively Inspect a Seasonal Adjustment Model (RStudio only)
### Aliases: inspect inspect.seas

### ** Examples

## Not run: 

m <- seas(AirPassengers)

inspect(m)

# pass arbitrary spec-arguments to inspect:
m2 <- seas(AirPassengers, estimate.maxiter = 1000)
inspect(m2)
## End(Not run)



cleanEx()
nameEx("out")
### * out

flush(stderr()); flush(stdout())

### Name: out
### Title: Diagnostical Re-Evaluation
### Aliases: out revisions slidingspans

### ** Examples

## Not run: 
m <- seas(AirPassengers)

# exit from the viewer with [q]
out(m)
out(m, search = "regARIMA model residuals")

slidingspans(m)
revisions(m)

# plot method for slingspans and revisions (see ?plot.seas)
plot(slidingspans(m))
plot(revisions(m))

## End(Not run)



cleanEx()
nameEx("outlier")
### * outlier

flush(stderr()); flush(stdout())

### Name: outlier
### Title: Outlier Time series
### Aliases: outlier

### ** Examples

## Not run: 
x <- seas(AirPassengers)
outlier(x)
## End(Not run)



cleanEx()
nameEx("plot.seas")
### * plot.seas

flush(stderr()); flush(stdout())

### Name: plot.seas
### Title: Seasonal Adjustment Plots
### Aliases: monthplot.seas plot.revisions plot.seas plot.slidingspans
###   residplot

### ** Examples

## Not run: 

m <- seas(AirPassengers)

plot(m)
plot(m, outliers = FALSE)
plot(m, trend = TRUE)

residplot(m)
residplot(m, outliers = FALSE)

monthplot(m)

plot(slidingspans(m))
plot(revisions(m))

# use R functions to analyze "seas" models
pacf(resid(m))
spectrum(diff(resid(m)))
plot(density(resid(m)))
qqnorm(resid(m))
## End(Not run)



cleanEx()
nameEx("qs")
### * qs

flush(stderr()); flush(stdout())

### Name: qs
### Title: Diagnostical Statistics
### Aliases: arimamodel fivebestmdl qs spc

### ** Examples

## Not run: 

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
## End(Not run)



cleanEx()
nameEx("seas")
### * seas

flush(stderr()); flush(stdout())

### Name: seas
### Title: Seasonal Adjustment with X-13ARIMA-SEATS
### Aliases: seas

### ** Examples

## Not run: 
m <- seas(AirPassengers)
summary(m)

# invoke X-13ARIMA-SEATS options as 'spec.argument'
# (consult the X-13ARIMA-SEATS manual for many more options and the wiki for
# for more examples)
seas(AirPassengers, regression.aictest = c("td"))  # no easter testing
seas(AirPassengers, force.type = "denton")  # force equality of annual values
seas(AirPassengers, x11 = list())  # use x11, overrides the 'seats' spec

# turn off the automatic procedures
seas(AirPassengers, regression.variables = c("td1coef", "easter[1]",
                                             "ao1951.May"), arima.model = "(0 1 1)(0 1 1)", regression.aictest = NULL,
     outlier = NULL, transform.function = "log")

# static replication of the first call
static(m)  # this also tests the equivalence of the static call
static(m, test = FALSE)  # no testing (useful for debugging)
static(m, coef = TRUE)  # also fixes the coefficients

# extractor functions
final(m)
original(m)
resid(m)
coef(m)

# five best models
fivebestmdl(m)

# replicating the default plots in Win X-13
plot(m)
plot(m, trend = TRUE)
monthplot(m)
monthplot(m, choice = "irregular")
spectrum(diff(final(m)) )
spectrum(diff(original(m)))
residplot(m)

# user defined regressors
# a temporary level shift in R base
tls <- ts(0, start = 1949, end = 1965, freq = 12)
window(tls, start = c(1955, 1), end = c(1957, 12)) <- 1
seas(AirPassengers, xreg = tls, outlier = NULL)
# identical to a X-13ARIMA-SEATS specification of the the level shift
seas(AirPassengers, regression.variables = c("tl1955.01-1957.12"),
     outlier = NULL)

# input and output from and to X-13ARIMA-SEATS
spc(m)  # analyzing the .spc file (for debugging)
out(m)  # display the .out file (see ?out, for details)

# NA handling
AirPassengersNA <- window(AirPassengers, end = 1962, extend = TRUE)
final(seas(AirPassengersNA, na.action = na.omit))    # no NA in final series
final(seas(AirPassengersNA, na.action = na.exclude)) # NA in final series
# final(seas(AirPassengersNA, na.action = na.fail))    # fails

# inspect tool
inspect(m)
## End(Not run)



cleanEx()
nameEx("static")
### * static

flush(stderr()); flush(stdout())

### Name: static
### Title: Static Call of a seas Object
### Aliases: static

### ** Examples

## Not run: 

m <- seas(AirPassengers)
static(m)
static(m, test = FALSE)
## End(Not run)



cleanEx()
nameEx("summary.seas")
### * summary.seas

flush(stderr()); flush(stdout())

### Name: summary.seas
### Title: Summary of a X13-ARIMA-SEATS seasonal adjustment
### Aliases: print.summary.seas summary.seas

### ** Examples

## Not run: 
x <- seas(AirPassengers)
summary(x)
## End(Not run)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
