
devtools::load_all(".")

# x <- seas(austres)

summary(x)

mdl(x)

library(stringr)
library(seasonal)
x <- seas(AirPassengers)

plot(x)
monthplot(x)
inspect(AirPassengers)


x <- seas(austres)

inspect(austres)
resid(x)

outlier(x)


static(x)
x$data
file <- ("~/tmp/iofile")

read_lks(file)

lks[2,]
dim(lks)
  rownames(lks)


x <- seas(x = AirPassengers, outlier.critical = 2.91)

residplot(x)



x <- seas(x = AirPassengers, outlier = "")

spc(x)





x <- seas(AirPassengers, estimate.save = "residuals", dir = "~/tmp/")

x <- seas(austres, dir = "~/tmp")

ol <- x$mdl$regression$variables[str_detect(x$mdl$regression$variables, "\\.")]


inspect(AirPassengers)

ol.ts <- OutlierTimeSeries(final(x), ol)

coef(x)
