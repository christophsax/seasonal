# list with options for inspect:
#
# 

sta <- function

x <- seas(AirPassengers)
          
lc <- as.list(x$call)  

# keep elements if they are int the vector


keep <- c("", "x", "xreg", "force.type", 
  "x11.mode", "x11.trendma", "x11.sigmalim", "x11.appendfcst", "x11.appendbcst", 
  "x11.final"
)

as.call(lc[names(lc) %in% keep])





names(x$call)



as.call(lc)





(x$call)


names

class(x$call)

methods(names)


?match.call


tls <- ts(0, start = 1949, end = 1965, freq = 12)
window(tls, start = c(1955, 1), end = c(1957, 12)) <- 1

x <- seas(AirPassengers, xreg = tls,
     x11 = list(),
     regression = list(),
     outlier.types = "none",
     identify.diff = c(0, 1),
     identify.sdiff = c(0, 1)
) 

static(x)

library(stringr)

# library(seasonal)
devtools::load_all(".")

# x <- seas(austres)
x <- seas(AirPassengers)
final(x)
static(x)
static(x, static.coeff = T)

x$mdl

x <- seas(AirPassengers, regression.variables = c("td1coef", "easter[1]", "ao1951.May"), arima.model = "(0 1 1)(0 1 1)", regression.b = c("-0.00294969914f", "0.01776737356f", "0.1001558243f", "0.1"), arima.ma = c("0.1156204208f", "0.4973600212f"), transform = NULL, regression.aictest = NULL, outlier.types = "none")

x$spc

x$mdl

tls <- ts(0, start = 1949, end = 1965, freq = 12)
window(tls, start = c(1955, 1), end = c(1957, 12)) <- 1

x <- seas(AirPassengers, xreg = tls,
          x11 = list(),
          regression = list(),
          outlier.types = "none",
          identify.diff = c(0, 1),
          identify.sdiff = c(0, 1),
) 

mdl(x)

mdl(x2)


seas(AirPassengers)
  

static(x, test=T)

x2 <- seas(AirPassengers, regression.variables = "", 
           arima.model = "(0 1 1)(0 1 1)", 
           x11 = list(), regression.aictest = NULL, outlier.types = "none")




spc(x2)
mdl(x2)






final(x2)-final(x)


spc(x2)
mdl(x2)


spc(x)
mdl(x)



plot(x)
monthplot(x, choice = "irregular")
spec.ar(final(x))


spec.ar(AirPassengers)


inspect(AirPassengers)

seas(AirPassengers, regression.variables = c("td1coef", "easter[1]", "ao1951.May"), arima.model = "(0 1 1)(0 1 1)", regression.b = c("-0.00294969914f", "0.01776737356f", "0.1001558243f"), arima.ma = c("0.1156204208f", "0.4973600212f"), transform = NULL, regression.aictest = NULL, outlier.types = "none")

s <- static(x)



is.expression(s)
exp <- as.expression(s)

eval(exp)

eval(s)

