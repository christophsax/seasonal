tpath <- file.path(path.package("seasonal"), "tests")

# tpath <- "/Users/christoph/git/seasonal/inst/tests/"


# import.ts
# ------------------------------------------------------------------------------

import.ts(file.path(tpath, "datavalue1.dta"))
import.ts(file.path(tpath, "datavalue2.dta"))

import.ts(file.path(tpath, "free1.txt"), format = "free", start = c(1949, 1), frequency = 12)
import.ts(file.path(tpath, "free2.txt"), format = "free", start = c(1949, 1), frequency = 12)

import.ts(file.path(tpath, "x13save.dta"), format = "x13save")
import.ts(file.path(tpath, "datavaluecomma.dta"), format = "datevaluecomma")
import.ts(file.path(tpath, "freecomma1.txt"), format = "freecomma", start = c(1949, 1), frequency = 12)
import.ts(file.path(tpath, "freecomma2.txt"), format = "freecomma",  start = c(1949, 1), frequency = 12)
import.ts(file.path(tpath, "datavalue_q.dta"))
import.ts(file.path(tpath, "x13save_q.dta"), format = "x13save")
import.ts(file.path(tpath, "datavalue_mult0.dta"))
import.ts(file.path(tpath, "datavalue_mult1.dta"))
import.ts(file.path(tpath, "datavalue_mult1.dta"))


import.ts(file.path(tpath, "x11_m1l.dat"), format = "1l", frequency = 12)
import.ts(file.path(tpath, "x11_m2l.dat"), format = "2l", frequency = 12)
import.ts(file.path(tpath, "x11_m2l2.dat"), format = "2l2", frequency = 12)
import.ts(file.path(tpath, "x11_m1r.dat"), format = "1r", frequency = 12)




# import.spc
# ------------------------------------------------------------------------------

 # importing the orginal X-13 example file
 import.spc(file.path(tpath, "Testairline.spc"))
 
 # a spc with multiple user defined regression and transformation series
 tdir <- tempdir()
 seas(x = AirPassengers, xreg = cbind(a = genhol(cny, start = 1, end = 4,
     center = "calendar"), b = genhol(cny, start = -3, end = 0,
     center = "calendar")), xtrans = cbind(sqrt(AirPassengers), AirPassengers^3),
     transform.function = "log", transform.type = "temporary",
     regression.aictest = "td", regression.usertype = "holiday", dir = tdir,
     out = TRUE)
import.spc(file.path(tdir, "iofile.spc"))


import.spc(file.path(tpath, "urtest0.spc"))
import.spc(file.path(tpath, "urtest1.spc"))


setwd(file.path(tpath, "x11"))

import.spc("SHOERS1982.spc")
import.spc("tdtest.spc")
import.spc("Mw1.spc")   # includes composite, so it cannot be run
import.spc("iaiua.spc")   




