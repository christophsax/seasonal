# defaults are not in m$list ---------------------------------------------------

# https://github.com/christophsax/seasonal/issues/251

m <- seas(mdeaths)

stopifnot(identical(names(m$list), "x"))
