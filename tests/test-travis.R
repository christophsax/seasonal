cat("GOOD")
Sys.setenv(X13_PATH = "./travis/x13")
library(seasonal)
cat("BETTER")
checkX13()
seas(AirPassengers)
message("LOOOKS GOOD")











