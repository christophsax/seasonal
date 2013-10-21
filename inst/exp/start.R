library(tstools)

# library(seasonal)
devtools::load_all(".")
library(stringr)

z <- seas(AirPassengers, force.type = "denton")
static(z)




z

static(z)


seas(AirPassengers,
     regression.variables = c("td1coef", "easter[1]", "ao1950.Jan", "ao1950.Nov", "ao1951.May", "ls1953.Jun", "ao1954.Feb", "ls1960.Apr"),
     arima.model = "(0 1 0)(0 1 1)",
     regression.aictest = NULL, outlier.types = "none"
)

x <- seas(AirPassengers, outlier.critical = 2.5)

library(manipulate)



plot(seas)


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




