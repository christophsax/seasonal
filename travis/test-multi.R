# call -------------------------------------------------------------------------

# this seems straigtforward

mdta <- cbind(a = AirPassengers, b = AirPassengers)

# multiple series, one spec
seas(x = mdta, x11 = "")

# R multimode
seas(x = mdta, x11 = "", multimode = "R")

# series as a list
seas(x = list(a = AirPassengers, b = AirPassengers), x11 = "")

# use with tsbox
# seas(x = tsbox::ts_tslist(tsbox::ts_c(mdeaths, fdeaths)), x11 = "")


# alternatively, using list =
seas(x = mdta, list = list(x11 = ""))

# multiple series, multiples specs
seas(
  x = mdta,
  # lengths of list must be equal to number of series
  list = list(
    list(x11 = ""),
    list()
  )
)

# this would be nice: specify SOME specs.args for all series
seas(
  x = mdta,
  regression.aictest = NULL,
  list = list(
    list(x11 = ""),
    list()
  )
)


# alternatively, use x in lists
seas(
  # lengths of list must be equal to number of series
  list = list(
    list(x = AirPassengers, x11 = ""),
    list(x = AirPassengers)
  )
)


# composite spec ---------------------------------------------------------------

# same spec for all series
m <- seas(
  cbind(mdeaths, fdeaths),
  composite = list(),           # adding an empty composite will use the seas() defaults for the indirect adjustment
  series.comptype = "add"       # may be added automatically if composite is not NULL
)

m <- seas(
  cbind(mdeaths, fdeaths),
  series.comptype = "add",
  composite = list(x11 = ""),   # use x11 for indirect adjustment
)


# different spec for all series
m <- seas(
  cbind(mdeaths, fdeaths),
  series.comptype = "add",
  composite = list(
    regression.aictest = NULL,
    x11.seasonalma = "s3x9"
  ),
  list = list(
    list(x11 = ""),
    list()
  )
)


