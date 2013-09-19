library(seasonal)
# syspath <- system.file(package = "seasonal")
syspath <- "C:/Users/seco-sxh/github/seasonal/inst"


x13path   <- paste0(syspath, "/x13/")
iopath   <- paste0(syspath, "/io/")

infile <- paste0(iopath, "test.spc")


stdfile <- paste0(iopath, "stdfile.spc")







x <- ReadX13(infile)
WriteX13(x, stdfile)
shell(paste0(x13path, "x13as.exe ", iopath, "stdfile"))
