library(stringr)

# library(seasonal)
devtools::load_all(".")

# x <- seas(austres)
x <- seas(AirPassengers)
final(x)
static(x)
static(x, static.coeff = T)

plot(x)
monthplot(x, choice = "irregular")
spec.ar(final(x))
spec.ar(AirPassengers)


inspect(AirPassengers)


