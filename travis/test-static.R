
# --- static -------------------------------------------------------------------

# 1. evaluate test cases
# 2. run static on seas model (including test run by static)


test_static <- function(x){
  ccc <- parse(text = x)

  cc <- ccc[[length(ccc)]]

  set.seed(100)  # because we have runif() in the examples  (this should be removed)

  if (length(ccc) > 1){
    for (i in 1:(length(ccc)-1)){
       eval(ccc[[i]], envir = globalenv())
    }
  }
  
  a <- eval(cc)
  static(a, fail = TRUE)
}

ll <- lapply(r, function(e) try(test_static(e), silent = TRUE))

failing <- which(sapply(ll, class) == "try-error")

if (length(failing[!failing %in% c(47L, 52L, 53L, 60L, 98L)]) > 0){
  stop("failing cases: ", paste(failing, collapse = ", "))
}


# --- known issues -------------------------------------------------------------

# slightly above tolerance limit:

# r[98]
# m <- seas(AirPassengers, 
#      x11 = "",
#      regression.aictest = NULL,
#      x11regression.variables = c("td", "easter[8]"),
#      x11regression.critical = 5, 
#      x11regression.b = c("0.4453f", "0.8550f", "-0.3012f", "0.2717f", 
#                          "-0.1705f", "0.0983f", "-0.0082")
#      )
# static(m)
# # Static series is different. Mean relative difference: 2.02737e-05


# complicated outliers are not read correctly from the mdl file

# r[47]
# m <- seas(AirPassengers,
#      transform.function = "log",
#      regression.aictest = NULL,
#      regression.variables = c("ao1950.1", "qi1950.2-1950.4", "ao1951.1", "td"),
#      arima.model = "(0 1 1)(0 1 1)",
#      outlier = NULL
#      )
# static(m, test = F)

# # r[52]
# m <- seas(AirPassengers,
#      transform.function = "log",
#      regression.variables = c("td/1952.dec/", "seasonal/1952.dec/"),
#      arima.model = "(0 1 1)",
#      x11 = "",
#      dir = "~/tmp"
#      )
# static(m, test = F)

# # r[53]
# m <- seas(AirPassengers,
#      transform.function = "log",
#      regression.variables = c("td", "td//1952.dec/", "seasonal", 
#                               "seasonal//1952.dec/"),
#      arima.model = "(0 1 1)",
#      x11 = ""
#      )
# static(m, test = F)

# # r[60]
# m <- seas(AirPassengers,
#      transform.function = "log",
#      regression.variables = c("ao1957.jan", "ls1959.jan", "ls1959.mar", 
#                             "ls1960.jan", "td"),
#      regression.b = c("-0.7946f", "-0.8739f", "0.6773f", "-0.6850f", 
#                       "0.0209", "-0.0107", "-0.0022", "0.0018", "-0.0088", 
#                       "-0.0074"),
#      regression.aictest = NULL,
#      arima.model =  "(0 1 2)(0 1 1)", 
#      x11 = ""
#      )
# static(m, test = F)





