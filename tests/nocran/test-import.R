# --- spc and data import from x13 ---------------------------------------------

# 1. import a range of x-13 data formats as time series
# 2. import and parse a range of acutal x-13 spc files

spcpath <- file.path(nocran_tests, "spc")

### import.ts

a <- import.ts(file.path(spcpath, "datavalue1.dta"))
a <- import.ts(file.path(spcpath, "datavalue2.dta"))

a <- import.ts(file.path(spcpath, "free1.txt"), format = "free", start = c(1949, 1), frequency = 12)
a <- import.ts(file.path(spcpath, "free2.txt"), format = "free", start = c(1949, 1), frequency = 12)

a <- import.ts(file.path(spcpath, "x13save.dta"), format = "x13save")
a <- import.ts(file.path(spcpath, "datavaluecomma.dta"), format = "datevaluecomma")
a <- import.ts(file.path(spcpath, "freecomma1.txt"), format = "freecomma", start = c(1949, 1), frequency = 12)
a <- import.ts(file.path(spcpath, "freecomma2.txt"), format = "freecomma",  start = c(1949, 1), frequency = 12)
a <- import.ts(file.path(spcpath, "datavalue_q.dta"))
a <- import.ts(file.path(spcpath, "x13save_q.dta"), format = "x13save")
a <- import.ts(file.path(spcpath, "datavalue_mult0.dta"))
a <- import.ts(file.path(spcpath, "datavalue_mult1.dta"))
a <- import.ts(file.path(spcpath, "datavalue_mult1.dta"))

# this gives a harmless incomplete final line found warning
a <- suppressWarnings(import.ts(file.path(spcpath, "x11_m1l.dat"), format = "1l", frequency = 12))

a <- import.ts(file.path(spcpath, "x11_m2l.dat"), format = "2l", frequency = 12)
a <- import.ts(file.path(spcpath, "x11_m2l2.dat"), format = "2l2", frequency = 12)
a <- import.ts(file.path(spcpath, "x11_m1r.dat"), format = "1r", frequency = 12)

# quarterly series
a <- import.ts(file.path(spcpath, "x11_q1r.dat"), format = "1r", frequency = 4)
a <- import.ts(file.path(spcpath, "x11_q1r_2ser.dat"), format = "1r", frequency = 4)


a <- import.ts(file.path(spcpath, "tramo1.txt"), format = "tramo")



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





