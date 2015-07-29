cat(Sys.getenv("TRAVIS"))
cat(Sys.getenv("TRAVIS_BUILD_DIR"))

if (Sys.getenv("TRAVIS") != ""){
  Sys.setenv(X13_PATH = file.path(Sys.getenv("TRAVIS_BUILD_DIR"), "travis/x13"))
  bdir <- Sys.getenv("TRAVIS_BUILD_DIR")
} else {
  bdir <- "~/seasonal"  # loc on ubuntu server
  # bdir <- "~/git/seasonal"  # local

}

library(seasonal)
checkX13()


cc <- read.csv(file.path(bdir, "travis/examples/ex_run.csv"))  # runnable examples

# known issues

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
r <- rr[-c(87, 98)]        # remove known issues

# --- numerical equality -------------------------------------------------------

# 1. evaluate test cases
# 2. compare results to run from previous versions

set.seed(100)  # because we have runif() in the examples  (this should be removed)

z <- lapply(r, function(e) try(eval(parse(text = e))))

failing <- which(sapply(z, class) == "try-error")

if (length(failing) > 0){
  stop("failing cases: ", paste(failing, collapse = ", "))
}

ff <- sapply(z, function(e) frequency(final(e)))

m.final <- do.call(cbind, lapply(z[ff == 12], final))
colnames(m.final) <- NULL

q.final <- final(z[ff == 4][[1]])
s.final <- final(z[ff == 6][[1]])


# save(m.final, q.final, s.final, file = file.path(bdir, "/travis/examples/test0.90.0.RData"))
bench <- new.env()
load(file.path(bdir, "/travis/examples/test0.90.0.RData"), envir = bench)

stopifnot(all.equal(m.final, bench$m.final))
stopifnot(all.equal(q.final, bench$q.final))
stopifnot(all.equal(s.final, bench$s.final))


# --- two way parsing ----------------------------------------------------------

# 1. evaluate test cases
# 2. import spc files
# 3. evaluate parsed calls 
# 4. compare series

tdir <-  file.path(tempdir(), "x13test")
if (!file.exists(tdir)) dir.create(tdir)

test_parse <- function(x){
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

if (length(failing) > 0){
  stop("failing cases: ", paste(failing, collapse = ", "))
}


# --- static -------------------------------------------------------------------

# 1. evaluate test cases
# 2. run static on seas model (including test run by static)

test_static <- function(x){
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

ll <- lapply(r, function(e) try(test_static(e), silent = TRUE))

failing <- which(sapply(ll, class) == "try-error")

if (length(failing[!failing %in% c(47L, 52L, 53L, 60L)]) > 0){
  stop("failing cases: ", paste(failing, collapse = ", "))
}


# known issues

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


# --- spc and data import from x13 ---------------------------------------------

# 1. import a range of x-13 data formats as time series
# 2. import and parse a range of acutal x-13 spc files

spcpath <- file.path(bdir, "travis", "spc")

### import.ts

import.ts(file.path(spcpath, "datavalue1.dta"))
import.ts(file.path(spcpath, "datavalue2.dta"))

import.ts(file.path(spcpath, "free1.txt"), format = "free", start = c(1949, 1), frequency = 12)
import.ts(file.path(spcpath, "free2.txt"), format = "free", start = c(1949, 1), frequency = 12)

import.ts(file.path(spcpath, "x13save.dta"), format = "x13save")
import.ts(file.path(spcpath, "datavaluecomma.dta"), format = "datevaluecomma")
import.ts(file.path(spcpath, "freecomma1.txt"), format = "freecomma", start = c(1949, 1), frequency = 12)
import.ts(file.path(spcpath, "freecomma2.txt"), format = "freecomma",  start = c(1949, 1), frequency = 12)
import.ts(file.path(spcpath, "datavalue_q.dta"))
import.ts(file.path(spcpath, "x13save_q.dta"), format = "x13save")
import.ts(file.path(spcpath, "datavalue_mult0.dta"))
import.ts(file.path(spcpath, "datavalue_mult1.dta"))
import.ts(file.path(spcpath, "datavalue_mult1.dta"))

# this gives a harmless incomplete final line found warning
suppressWarnings(import.ts(file.path(spcpath, "x11_m1l.dat"), format = "1l", frequency = 12))

import.ts(file.path(spcpath, "x11_m2l.dat"), format = "2l", frequency = 12)
import.ts(file.path(spcpath, "x11_m2l2.dat"), format = "2l2", frequency = 12)
import.ts(file.path(spcpath, "x11_m1r.dat"), format = "1r", frequency = 12)


### import.spc

 # importing the orginal X-13 example file
 import.spc(file.path(spcpath, "Testairline.spc"))
 
 # a spc with multiple user defined regression and transformation series
 tdir <- tempdir()
 seas(x = AirPassengers, xreg = cbind(a = genhol(cny, start = 1, end = 4,
     center = "calendar"), b = genhol(cny, start = -3, end = 0,
     center = "calendar")), xtrans = cbind(sqrt(AirPassengers), AirPassengers^3),
     transform.function = "log", transform.type = "temporary",
     regression.aictest = "td", regression.usertype = "holiday", dir = tdir,
     out = TRUE)
import.spc(file.path(tdir, "iofile.spc"))

import.spc(file.path(spcpath, "urtest0.spc"))
import.spc(file.path(spcpath, "urtest1.spc"))

import.spc(file.path(spcpath, "x11", "SHOERS1982.spc"))
import.spc(file.path(spcpath, "x11", "tdtest.spc"))
import.spc(file.path(spcpath, "x11", "Mw1.spc"))   # includes composite, so it cannot be run
import.spc(file.path(spcpath, "x11", "iaiua.spc"))





