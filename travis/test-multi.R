# call -------------------------------------------------------------------------

# this seems straigtforward

mdta <- cbind(a = AirPassengers, b = AirPassengers)

# multiple series, one spec
seas(x = mdta, x11 = "")

# R multimode
seas(x = mdta, x11 = "", multimode = "R")
seas(x = mdta, x11 = "", multimode = "x13")

# Open question: which default to use - speed vs stability, ease of debugging

# series as a list
m <- seas(x = list(a = mdeaths, b = AirPassengers), x11 = "", list = list(list(), list(outlier.critical = 3)))

m_a <- seas(mdeaths, x11 = "")
m_b <- seas(AirPassengers, x11 = "", outlier.critical = 3)

stopifnot(all.equal(final(m$a), final(m_a)))
stopifnot(all.equal(final(m$b), final(m_b)))

stopifnot(all.equal(original(m$a), original(m_a)))
stopifnot(all.equal(original(m$b), original(m_b)))

stopifnot(all.equal(trend(m$a), trend(m_a)))
stopifnot(all.equal(trend(m$b), trend(m_b)))

stopifnot(all.equal(irregular(m$a), irregular(m_a)))
stopifnot(all.equal(irregular(m$b), irregular(m_b)))

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


# extractor functions ----------------------------------------------------------

m <- seas(x = mdta, x11 = "")

final(m)

original(m)

trend(m)

irregular(m)


