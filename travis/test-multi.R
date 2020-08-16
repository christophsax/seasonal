# call -------------------------------------------------------------------------

# this seems straigtforward

mdta <- cbind(a = AirPassengers, b = AirPassengers)

# multiple series, one spec
seas(x = mdta, x11 = "")

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

