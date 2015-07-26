tpath <- file.path(path.package("seasonal"), "tests")


# import.ts
# ------------------------------------------------------------------------------

import.ts(file.path(tpath, "datavalue1.dta"))
import.ts(file.path(tpath, "datavalue2.dta"))

import.ts(file.path(tpath, "free1.txt"), format = "free", start = c(1949, 1),
       frequency = 12)
import.ts(file.path(tpath, "free2.txt"), format = "free", start = c(1949, 1),
       frequency = 12)

import.ts(file.path(tpath, "x13save.dta"), format = "x13save")
import.ts(file.path(tpath, "datavaluecomma.dta"), format = "datevaluecomma")
import.ts(file.path(tpath, "freecomma1.txt"), format = "freecomma",
       start = c(1949, 1), frequency = 12)
import.ts(file.path(tpath, "freecomma2.txt"), format = "freecomma",
       start = c(1949, 1), frequency = 12)
import.ts(file.path(tpath, "datavalue_q.dta"))
import.ts(file.path(tpath, "x13save_q.dta"), format = "x13save")
import.ts(file.path(tpath, "datavalue_mult0.dta"))
import.ts(file.path(tpath, "datavalue_mult1.dta"))
import.ts(file.path(tpath, "datavalue_mult1.dta"))




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


