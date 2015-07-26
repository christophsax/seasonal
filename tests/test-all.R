cat(Sys.getenv("TRAVIS"))
cat(Sys.getenv("TRAVIS_BUILD_DIR"))




Sys.setenv(X13_PATH = file.path(Sys.getenv("TRAVIS_BUILD_DIR"), "travis/x13")
library(seasonal)
cat("BETTER")
checkX13()
seas(AirPassengers)
message("LOOOKS GOOD")






cc <- read.csv("./travis/examples/ex_run.csv")  # runnable examples

# known issues:

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



rr <- as.character(cc$r)

r <- rr[-c(87, 98)]  # remove known issues

# --- numerical equality -------------------------------------------------------

z <- lapply(cl, r, function(e) try(eval(parse(text = e))))

failing <- which(sapply(z, class) == "try-error")


if (length(failing) > 0){
  stop("failing cases: ", paste(failing, collapse = ", "))
}

ff <- sapply(z, function(e) frequency(final(e)))

m.final <- do.call(cbind, lapply(z[ff == 12], final))
colnames(m.final) <- NULL

q.final <- final(z[ff == 4][[1]])
s.final <- final(z[ff == 6][[1]])


# save(m.final, q.final, s.final, file = "test0.90.0.RData")
bench <- new.env()
load("./travis/examples/test0.90.0.RData", envir = bench)

stopifnot(all.equal(m.final, bench$m.final))
stopifnot(all.equal(q.final, bench$q.final))
stopifnot(all.equal(s.final, bench$s.final))



# --- 2 way parsing ------------------------------------------------------------


test_parse <- function(x){
  tdir <-  "~/tmp"
  ccc <- parse(text = x)

  cc <- ccc[[length(ccc)]]

  if (length(ccc) > 1){
    for (i in 1:(length(ccc)-1)){
       eval(ccc[[i]])
    }
  }
  
  cc$out <- TRUE
  cc$dir <- tdir
  a <- eval(cc)
  
  bb <- import.spc(file.path(tdir, "iofile.spc"))
  b <- lapply(bb, eval)$call
  all.equal(final(a), final(b))
}

ll <- lapply(r, function(e) try(test_parse(e)))


failing <- which(sapply(ll, class) == "try-error")

 # TRUE
if (length(failing) > 0){
  stop("failing cases: ", paste(failing, collapse = ", "))
}



# --- static -------------------------------------------------------------------


test_static <- function(x){
  tdir <-  "~/tmp"
  ccc <- parse(text = x)

  cc <- ccc[[length(ccc)]]

  if (length(ccc) > 1){
    for (i in 1:(length(ccc)-1)){
       eval(ccc[[i]], envir = globalenv())
    }
  }
  
  a <- eval(cc)
  
  static(a)

}



ll <- lapply(r, function(e) try(test_static(e)))


failing <- which(sapply(ll, class) == "try-error")

if (!length(failing[!failing %in% c(47L, 52L, 53L, 60L)]) > 0){
  stop("failing cases: ", paste(failing, collapse = ", "))
}




# known static issues: complicated outliers are not read correctly from the mdl
# file

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







