devtools::load_all(".")

seas(AirPassengers)


y <- ReadX13(infisle)
WriteX13(y, file = outfile)

