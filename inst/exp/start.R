library(tstools)

library(seasonal)


z <- seas(AirPassengers, method = "x11")

plot(z)





z2 <- seasonal(AirPassengers)

z2$output.sa$stats




