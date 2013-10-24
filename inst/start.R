library(stringr)

# library(seasonal)
devtools::load_all(".")

x <- seas(AirPassengers, X14 = list(), X15 = list())

t <- paste(x$err[-c(1:5)], collapse = " ")


str_match_all(t, "WARNING:")

x <- seas(AirPassengers)


str_x$err




ARIMA model search
Outlier detection
Detection of trading day and Easter effects

x$data

x$spc$series$title

x$mdl$regression

summary(AirPassengers)


xreg <- cbind(a = austres, b = austres)
  
var <- ts(rnorm(200), start = 1949, frequency = 12)

x2 <- seas(AirPassengers, xreg = var, regression.aictest = c("td", "easter", "user"))



series {title = "Quarterly sales"  start = 1981.1
        data = (301 294 ...  391)  period = 4  }
regression {user = tls
            data = (0 0 0 0 0 0 0 0 0 0 0 0 ...
                    0 0 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 ... 0) }
identify   { diff = (0 1) sdiff = (0 1) }



plot(AirPassengers)

x2 <- seas(AirPassengers, 
     x11 = list(),
     regression.variables = c("tl1955.03-1957.01"),
     arima.model = "(0 1 1)(0 1 1)",
     identify.diff = c(0, 1),
     identify.sdiff = c(0, 1)
) 

static(x2)

x2 <- seas(AirPassengers, 
           x11 = list(),
           regression.variables = c("tl1955.01-1957.12"),
           regression.aictest = NULL,
           outlier.types = "none",
           identify.diff = c(0, 1),
           identify.sdiff = c(0, 1),
) 

easter1 <- ts(0, start = 1949, end = 1965, freq = 12)
window(easter1, start = c(1955, 1), end = c(1955, 2)) <- 1

easter2 <- ts(0, start = 1949, end = 1965, freq = 12)
window(easter2, start = c(1957, 11), end = c(1957, 12)) <- 1

seas(AirPassengers, xreg = cbind(easter1, easter2),
     x11regression.usertype = "holiday"
)




x1 <- seas(AirPassengers, xreg = tls,
           x11 = list(),
           regression = list(),
           outlier.types = "none",
           identify.diff = c(0, 1),
           identify.sdiff = c(0, 1),
) 

predict(x1)-predict(x2)





x2 <- seas(AirPassengers, 
           x11 = list(),
           regression.variables = c("ao1950.jan"),
           regression.aictest = NULL,
           outlier.types = "none",
) 

tls <- ts(0, start = 1949, end = 1965, freq = 12)
window(tls, start = c(1950, 1), end = c(1950, 1)) <- 1

x1 <- seas(AirPassengers, xreg = tls,
           x11 = list(),
           regression = list(),
           outlier.types = "none",
           
) 


predict(x1)-predict(x2)

x1$mdl
x2$mdl

seas(AirPassengers, 
     x11 = list(),
     transform.function = "log",
     regression.variables = c("ao1967.1", "ls1985.3", "ls1987.2", "ao1978.1", "td"),
     arima.model = "(0 1 1)(0 1 1)"
)




x <- seas(AirPassengers, 
     transform.function = "log", 
     regression.variables = c("ao1956.feb", "ao1958.feb", "ls1960.feb",
                              "ls1952.nov", "ao1954.feb"),
     arima.model = "(0 1 2)(0 1 1)",
     forecast.maxlead = 60,
     x11.seasonalma = "s3x9"
)

x2 <- seas(AirPassengers, 
     x11 = list(),
     pickmdl.mode = "fcst"
)


x2$mdl

x2$spc
x2 <- seas(AirPassengers, 
               x11 = list(),
               regression.variables = c("td", "ao1999.01"),
               arima.model = "(0,1,1)(0,1,1)",
               arima.ma = "0.25f",
               estimate.tol = 1e-4,
               estimate.maxiter = 100,
               estimate.exact = "ma"
)


x2$mdl$regression

cat(WriteText(x3$spc))

x3$mdl

x3 <-     seas(AirPassengers, 
               x11 = list(),
               regression.variables = c("td", "ao1999.01"),
               arima.model = "(0,1,1)(0,1,1)",
               arima.ma = "0.25f",
               estimate.tol = 1e-4,
               estimate.maxiter = 100,
               estimate.exact = "ma"
)

static(x3, static.coeff=TRUE)


x3 <- seas(AirPassengers,
           regression.variables = c("td", "ao1951.May"),
           arima.model = "(0 1 0)(0 1 1)",
           arima.ma = "0.25f",
           transform = NULL,
           regression.aictest = NULL, outlier.types = "none"
)

x3 <- seas(AirPassengers,
           regression.variables = c("td", "ao1951.May"),
           arima.model = "(0 1 0)(0 1 1)",
           arima.ma = "0.25f",
           transform = NULL,
           regression.aictest = NULL, outlier.types = "none"
)

x4 <- seas(AirPassengers,
     regression.variables = c("td", "ao1951.May"),
     arima.model = "(0 1 0)(0 1 1)",
     regression.b = c("-0.655149759f", "-1.446766266f", "-1.051914831f", "-2.020760853f", "1.416146773f", "1.239608635f", "10.16224917f", "23.10660807f"),
     transform = NULL,
     regression.aictest = NULL, outlier.types = "none"
)


x5 <- seas(AirPassengers,
     regression.variables = c("td", "ao1951.May"),
     arima.model = "(0 1 0)(0 1 1)",
     regression.b = c("-0.655149759f", "-1.446766266f", "-1.051914831f", "-2.020760853f", "1.416146773f", "1.239608635f", "10.16224917f", "23.10660807f"),
     arima.ma = c("0.2500000000E+00f"),
     transform = NULL,
     regression.aictest = NULL, outlier.types = "none"
)

predict(x5) - predict(x3)


static(x3, static.coeff=T)


static(x2, static.coeff = T)


x2$err

x <- seas(AirPassengers,
     x11 = list(),
     transform.aicdiff = 0.0
     )

x$mdl

x$spc
View(x$out)
x <- seas(AirPassengers)

inspect(AirPassengers)



z$data[, 'trend']

plot(z)



x<- seas(AirPassengers)

#   path <- "C:/Users/seco-sxh/github/seasonal/inst/"


seas(AirPassengers,
     regression.variables = c("td1coef", "easter[1]", "ao1950.Jan", "ao1950.Nov", "ao1951.May", "ls1953.Jun", "ao1954.Feb", "ls1960.Apr"),
     arima.model = "(0 1 0)(0 1 1)",
     regression.aictest = NULL, outlier.types = "none"
)

x <- seas(AirPassengers, outlier.critical = 2.5)

library(manipulate)



plot(x)


z <- seas(AirPassengers, regression.variables = c("td1coef", "easter[1]", "ao1951.May", "ao1954.Feb"), 
          arima.model = "(0 1 0)(0 1 1)", regression.aictest = NULL, force.type = "denton")


z <- seas(AirPassengers, transform = NULL,
          regression.variables = c("td1coef", "easter[1]", "ao1951.May", "ao1954.Feb"), 
          arima.model = "(0 1 0)(0 1 1)", regression.aictest = NULL, outlier.types = "none",
          regression.b = c("-1f", "0f", "0.01", "0.1", "-0.1f")
          )

z <- seas(AirPassengers, transform = NULL,
          regression.variables = c("td1coef", "easter[1]", "ao1951.May", "ao1954.Feb"), 
          arima.model = "(0 1 0)(0 1 1)", regression.aictest = NULL, outlier.types = "none"
)



cat(paste(x$mdl$regression$variables, collapse = '", "'))

x <- seas(AirPassengers)



seas(AirPassengers,
     regression.variables = c("td1coef", "easter[1]", "ao1951.May")
     arima.model = "(0 1 1)(0 1 1)",
     regression.aictest = NULL, outlier.types = "none"
)

paste0('seas(', x$call[[2]], ', ',
       'regression.variables = c("', paste(x$mdl$regression$variables, collapse = '", "'), '"), '
       'arima.model = "', x$mdl$regression$variables, '"',
       'regression.aictest = NULL, outlier.types = "none"'
       )

       
       



static <- function(x){
  stopifnot(inherits(x, "seas"))
  z <- paste0('seas(', x$call[[2]], ',\n',
              '  regression.variables = c("', paste(x$mdl$regression$variables, collapse = '", "'), '")\n',
              '  arima.model = "', x$mdl$arima$model, '",\n',
              '  regression.aictest = NULL, outlier.types = "none"\n)'
  )
  cat(z)
}







plot(z)

z$mdl$regression$variables

OutlierTimeSeries(AirPassengers, c("ao1951.May", "ao1954.Feb" ))




z <- seasonal(AirPassengers)
plot(z, start = 1949)





i <- 1


x <- ol.time[[1]][2]




strptime(ol.time, "%Y %B")


mod_sa <- (seasonal(AirPassengers))

((cbind(predict(z), predict(mod_sa))))

ta(cbind(predict(z), predict(mod_sa), z$data[, 'original']))




z <- seas(AirPassengers, regression.variables = c("td1coef", "ao1951.May", "ao1952.May"), 
          arima.model = "(0 1 1)(0 1 1)")

z <- seas(AirPassengers, regression.variables = c("td1coef", "ao1951.May", "ao1952.May"), 
          arima.model = "(0 1 1)(0 1 1)", regression.aictest = NULL)



z

x <- (seas(AirPassengers))
plot(x)




mod_sa

plot(mod_sa)



plot

GenSPC(AirPassengers)


x1 <- GenSPC(AirPassengers)
# q <- ModSPC(x, automdl = NULL, arima.model = "(1 1 1 0 1 1)")
# q <- ModSPC(x, regression.aictest = c("td", "easter"), regression.variables = c("ao1960.1"))

x2 <- ModSPC(x1, regression.variables = c("td1coef", "easter[1]", "ao1951.May"), 
             arima.model = "(0 1 1)(0 1 1)", automdl = NULL)

x2
x <- EnsureConsistencySPC(x1)

RunSPC(x)
y <- ReadX13()


ViewLog <- function(path = "C:/Users/seco-sxh/github/seasonal/inst/"){
  

y

length(x$mdl$arima$model)


x <- seas(AirPassengers)

x$mdl

plot(seas(AirPassengers))

plot(z)




z2 <- seasonal(AirPassengers)

z2$output.sa$stats




