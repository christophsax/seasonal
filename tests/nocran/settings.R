
library(seasonal)
checkX13()

# import test cases
cc <- read.csv(file.path(nocran_tests, "examples/ex_run.csv"))

rr <- as.character(cc$r)
r <- rr[-c(87, 98)]        # remove known issues


# --- known issues -------------------------------------------------------------

# # 87
# seas(AirPassengers, transform.function = "none", transform.power = 0.3333)

# # 98
# data(holiday)
# easter1 <- genhol(easter, start = -10, end = -1, frequency = 12)
# easter2 <- genhol(easter, start = 0, end = 5, frequency = 12)
# seas(AirPassengers,
#      x11 = "",
#      regression.aictest = NULL,
#      xreg = cbind(easter1, easter2),
#      x11regression.aictest = "td",
#      x11regression.usertype = "holiday",
#      outlier = NULL
#      )
