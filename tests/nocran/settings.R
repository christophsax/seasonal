
library(seasonal)
checkX13()

# import test cases
cc <- read.csv(file.path(nocran_tests, "examples/ex_run.csv"))

rr <- as.character(cc$r)
# r <- rr[-c(87, 98)]        # remove known issues


r <- rr[-c(87, 91, 96, 97, 98, 99, 100, 101, 102, 103)]


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



# --- new known issues ---------------------------------------------------------

# https://github.com/christophsax/seasonal/issues/272
# seas(
#   AirPassengers,
#   regression.aictest = NULL,
#   x11.seasonalma = "s3x9",
#   x11.trendma = 23,
#   x11regression.variables = "td",
#   x11regression.aictest = "td"
# )

# seas(
#   AirPassengers,
#   x11 = "",
#   regression.aictest = NULL,
#   x11regression.variables = "td"
# )

# seas(
#   AirPassengers,
#   x11 = "",
#   regression.aictest = NULL,
#   x11regression.variables = "td",
#   x11regression.aictest = c("td", "easter")
# )

# seas(AirPassengers,
#   x11 = "",
#   regression.aictest = NULL,
#   x11regression.variables = "td",
#   x11regression.tdprior = c(1.4, 1.4, 1.4, 1.4, 1.4, 0.0, 0.0),
#   transform.function = "log"
# )

# seas(AirPassengers,
#      x11 = "",
#      regression.aictest = NULL,
#      x11regression.variables = c("td", "easter[8]"),
#      x11regression.critical = 5,
#      x11regression.b = c("0.4453f", "0.8550f", "-0.3012f", "0.2717f",
#                          "-0.1705f", "0.0983f", "-0.0082")
#      )

# seas(AirPassengers,
#      x11 = "",
#      regression.aictest = NULL,
#      x11regression.variables = c("td/1950.1/", "easter[8]",
#                                  "labor[10]", "thank[10]"),
#      x11.seasonalma = "x11default",
#      x11.sigmalim = c(1.8, 2.9),
#      x11.appendfcst = "yes"
#      )

# seas(AirPassengers,
#   x11 = "",
#   transform.function = "log",
#   regression.variables = "const",
#   regression.aictest = NULL,
#   arima.model = "(0 1 1)(0 1 1)",
#   outlier = NULL,
#   x11regression.variables = c("td", "easter[8]")
# )

