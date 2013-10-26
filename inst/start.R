# list with options for inspect:
#
# 
# autoprocedures:

# automdl
# pickmdl
# manual

# aic test
# chi2test


# library(seasonal)
devtools::load_all(".")

# x <- seas(austres)

summary(x)

mdl(x)

library(stringr)

x <- seas(AirPassengers)

plot(x)
monthplot(x)

x <- seas(austres)
resid(x)

outlier(x)

x$data
read_data_file("~/tmp/iofile.rsd")



x <- seas(x = AirPassengers, outlier.critical = 2.91)
residplot(x)




x <- seas(AirPassengers, estimate.save = "residuals", dir = "~/tmp/")

x <- seas(austres, dir = "~/tmp")

ol <- x$mdl$regression$variables[str_detect(x$mdl$regression$variables, "\\.")]


inspect(AirPassengers)

ol.ts <- OutlierTimeSeries(final(x), ol)


x$spc

inspect(AirPassengers)

ts(0, start(AirPassengers), end = 1965 = 12)

x <- seas(AirPassengers, pickmdl = list())

inspect(AirPassengers, pickmdl = list())

coef(x)


mdl(x)

static(x, static.coeff=T)



x <- seas(x = AirPassengers, regression.variables = c("td1coef", "easter[1]", 
                                                 "ao1951.May"), arima.model = "(0 1 1)(0 1 1)", regression.chi2test = "no", 
     outlier.types = "none", transform.function = "log", regression.b = c("-0.00294969914f", 
                                                                          "0.01776737356f", "0.1001558243f"), arima.ma = c("0.1156204208f", 
                                                                                                                           "0.4973600212f"), dir = "~/tmp")




file <- "~/tmp/iofile.est"



est$reg$fixed

library(stringr)



arima(lh, order = c(1,0,0))

getAnywhere(print.Arima)

static(x)


coefficients

mod <- arima(LakeHuron, order = c(2,0,0), xreg = time(LakeHuron)-1920)

summary(mod)

sm <- summary(mod1)
  
  
x <- summary(mod1)



inspect(AirPassengers)
final(x)

static(x, static.coeff = T)


seas(AirPassengers, save.out = T)

output

seas(x = austres, 
     regression.variables = c("ao1974.4", "ls1990.3", 
    "ls1992.1"), arima.model = "(1 2 0)(0 1 1)", regression.chi2test = "no", 
     outlier.types = "none", transform.function = "log", 
     regression.b = c("0.000792151787f", "-0.002424193975f", "-0.001581068458f"), 
     arima.ma = "0.7583853142f", 
     arima.ar = "-0.3472735644f")

inspect(austres)

x2 <- seas(x = AirPassengers, regression.variables = c("td1coef", "easter[1]", 
                                                 "ao1951.May"), arima.model = "(0 1 1)(0 1 1)", transform.function = "log", 
     regression.b = c("-0.00294969914f", "0.01776737356f", "0.1001558243f"
     ), arima.ma = c("0.1156204208f", "0.4973600212f"))

mdl(x2)


mdl(x)

deparse



str

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

page(noquote(x$output))
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

