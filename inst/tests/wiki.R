# examples from the wiki page, run for testing

# This page collects the examples from the official X-13ARIMA-SEATS [manual][manual] in the R package *seasonal*. With the exception of the currently unsupported `composite` spec, it is possible to reproduce most examples in R. All models have been tested and should run without additional data in R. If you discover an error or want to comment on an example, you may edit the wiki or send me an an [e-mail](mailto:christoph.sax@gmail.com).
# 
# ## Content
# 
# - [7.1 ARIMA](#wiki-71-arima)
# - [7.2 AUTOMDL](#wiki-72-automdl)
# - [7.3 CHECK](#wiki-73-check)
# - [7.4 COMPOSITE](#wiki-74-composite)
# - [7.5 ESTIMATE](#wiki-75-estimate)
# - [7.6 FORCE](#wiki-76-force)
# - [7.7 FORECAST](#wiki-77-forecast)
# - [7.8 HISTORY](#wiki-78-history)
# - [7.9 METADATA](#wiki-79-metadata)
# - [7.10 IDENTIFY](#wiki-710-identify)
# - [7.11 OUTLIER](#wiki-711-outlier)
# - [7.12 PICKMDL](#wiki-712-pickmdl)
# - [7.13 REGRESSION](#wiki-713-regression)
# - [7.14 SEATS](#wiki-714-seats)
# - [7.15 SERIES](#wiki-715-series)
# - [7.16 SLIDINGSPANS](#wiki-716-slidingspans)
# - [7.17 SPECTRUM](#wiki-717-spectrum)
# - [7.18 TRANSFORM](#wiki-718-transform)
# - [7.19 X11](#wiki-719-x11)
# - [7.20 X11REGRESSION](#wiki-720-x11regression)
# 
# ## 7.1 ARIMA
# 
# 
# 
# ### Example 1
# 
#     series { title  =  "Quarterly Grape Harvest" start = 1950.1
#              period =  4
#              data  = (8997 9401 ... 11346) }
#     arima { model = (0 1 1) }
#     estimate { }
# 
# #### R-code:
# 
    seas(AirPassengers,
         x11 = list(),
         arima.model = "(0 1 1)"
    )
#  
# #### Remark(s):
# - If the `x11` spec is specified (here, as an empty `list`), the default `seats` spec is automatically disabled.
# 
# 
# 
# ### Example 2
# 
#     series { title = "Monthly sales" start = 1976.jan
#              data = (138 128 ... 297) }
#     transform { function = log } arima{model =(210)(011)} estimate { }
# 
# #### R-code:
# 
    seas(AirPassengers,
         x11 = list(),
         transform.function = "log",
         arima.model = "(2 1 0)(0 1 1)"
    )

# 
# ### Example 3
# 
#     Series { Title = "Monthly Sales"  Start = 1976.jan
#              Data = (138 128 ... 297)  }
#     Transform { Function = log }
#     Regression { Variables= (seasonal const) } Arima {Model=(011)}
#     Estimate { }
# 
# #### R-code:
# 
    seas(AirPassengers,
         x11 = list(),
         transform.function = "log",
         regression.variables = c("seasonal", "const"),
         arima.model = "(0 1 1)"
    )

# 
# ### Example 4
# 
#     series{title = "Annual Olive Harvest" start = 1950
#             data = (251 271 ... 240)  }
#     arima{model  = ([2] 1 0)}
#     estimate{ }
# 
# #### R-code:
# 
    seas(AirPassengers,
         x11 = list(),
         arima.model = "([2] 1 0)"
    )

# ### Example 5
# 
#     series { title = "Monthly sales"  start = 1976.jan
#              data = (138 128 ... 297) }
#     transform { function = log }
#     regression { variables = const }
#     arima { model  = (0 1 1)12 }
#     estimate { }
# 
# #### R-code:
# 
    seas(AirPassengers,
         x11 = list(),
         transform.function = "log",
         regression.variables = c("const"),
         arima.model = "(0 1 1)12"
    )

# ### Example 6
# 
#     series { title = "Monthly sales"  start = 1976.jan
#               data = (138 128 ... 297) }
#     transform { function = log }
#     regression { variables = (const seasonal)} arima{model =(110)(100)3(001)} estimate { }
# 
# #### R-code:
# 
    seas(AirPassengers,
         x11 = list(),
         transform.function = "log",
         regression.variables = c("const", "seasonal"),
         arima.model = "(1 1 0)(1 0 0)3(0 0 1)"
    )

# ### Example 7
# 
#     series { title = "Monthly sales"  start = 1976.jan
#              data = (138 128 ...  297) }
#     transform{ function = log }
#     arima  { model = (0 1 1)(0 1 1)12
#                 ma = ( ,1.0f)}
#     estimate { }
# 
# #### R-code:
# 
    seas(AirPassengers,
         x11 = list(),
         transform.function = "log",
         arima.model = "(0 1 1)(0 1 1)12",
         arima.ma = " , 1.0f"
    )

# #### Remark(s):
# - Because the first element in the `arima.ma` argument is emptly, it has to be entered as a character string instead of a numeric vector.
# 
# 
# ## 7.2 AUTOMDL
# 
# ### Example 1
# 
#     series      { title = "Monthly sales"   start = 1976.jan
#                 file="ussales.dat"  }
#     regression { variables = (td seasonal) } automdl { }
#     estimate { }
#     x11 {}
# 
# #### R-code:
# 
    seas(AirPassengers, 
         x11 = list(),
         regression.variables = c("td", "seasonal")
    )
    
# 
# #### Remark(s):
# - If the `x11` spec is specified (here, as an empty `list`), the default `seats` spec is automatically disabled. 
# 
# ### Example 2
# 
#     series      { title = "Monthly sales"
#                   file="ussales.dat" }
#     regression  { variables = td  }
#     start = 1976.jan
#     automdl {
#     diff=(11)
#     maxorder = ( 3, ) }
#     outlier     {   }
#     estimate { } x11 {}
# 
# #### R-code:
# 
    seas(AirPassengers, 
         x11 = list(),
         regression.variables = c("td"),
         automdl.diff = c(1, 1),
         automdl.maxorder = "3, "
    )
    
# 
# #### Remark(s):
# - Because the second element in the `automdl.maxorder` argument is empty, it has to be entered as a character string instead of a numeric vector.
# 
# ### Example 3
# 
#     series      { title = "Monthly sales"
#                   file="ussales.dat" }
#     regression { aictest = td } automdl { savelog = amd } estimate { }
#     x11 {}
#     
# #### R-code:
# 
    seas(AirPassengers, 
         x11 = list(),
         regression.aictest = c("td"),
         automdl.savelog = "amd",
         dir = "~/userdir"
    )

# #### Remark(s):
# - If you are interested in non-supported output tables, use the `dir` argument to 
# copy the X-13ARIMA-SEATS output files to a user defined directory (see `?seas`).
# 
# 
# ## 7.3 CHECK
# 
# #### Remark(s):
# - Output and diagnostics are handeled by the seasonal package.
# - If you are interested in non-supported output tables, use the `dir` argument to 
# copy the X-13ARIMA-SEATS output files to a user defined directory (see `?seas`).
# 
# 
# ## 7.4 COMPOSITE
# 
# #### Remark(s):
# - The `composite` spec is currently not supported. 
# 
# 
# ## 7.5 ESTIMATE
# 
# ### Example 1
# 
#     series { title = "Monthly Sales" start = 1976.1
#               data = (138 128 ... 297) }
#     regression { variables = seasonal }
#     arima { model = (0,1,1)   ma = (0.25f) }
#     estimate { save = residuals }
#     
# #### R-code:
# 
    seas(AirPassengers, 
         x11 = list(),
         regression.variables = c("seasonal"),
         arima.model = "0, 1, 1",
         arima.ma = "0.25f"
    )

# #### Remark(s):
# - Residuals are imported by default and can be accessed with `resid`.
#     
# 
# 
# 
# 
# ### Example 2 
#     
#     series { title = "Monthly Inventory" start = 1978.12
#               data = (1209 834 ... 1002) }
#     transform { function = log }
#     regression { variables = (td ao1999.01)  }
#     arima { model = (1,1,0)(0,1,1) }
#     estimate { tol = 1e-4  maxiter = 100  exact = ma  save = mdl
#     print = (iterations roots) }
#       
# #### R-code:
# 
    m <- seas(AirPassengers, 
         x11 = list(),
         outlier = NULL, 
         transform.function = "log",
         regression.variables = c("td", "ao1959.01"),
         arima.model = "(1 1 0)(0 1 1)",
         regression.aictest = NULL,
         estimate.tol = 1e-4,
         estimate.maxiter = 100,
         estimate.exact = "ma"
    )

    
# #### Remark(s):
# - `regression.aictest` has to be turned of for fully manual variable specification.
# 
# ### Example 3
# 
#     series { title = "Monthly Inventory" start = 1978.12
#                   data = (1209 834 ... 1002) }
#     transform { function = log }
#     estimate { file = "Inven.mdl"
#          fix = all }
# 
# #### R-code:
# 
    
    seas(x = AirPassengers, 
         x11 = list(), 
         regression.variables = c("td", "ao1959.01"), 
         estimate.maxiter = 100, 
         estimate.exact = "ma", 
         arima.model = "(1 1 0)(0 1 1)", 
         regression.aictest = NULL, 
         outlier = NULL, 
         transform.function = "log", 
         regression.b = c("-0.006263974216f", "-0.003388544874f", 
                          "-0.002629579663f", "-0.002867539316f", 
                          "0.002675064631f", "0.003928729538f", 
                          "0.000897413435f"), 
         arima.ma = "0.574618733f", 
         arima.ar = "-0.2255735128f"
         )
    
    

# #### Remark(s):
# - Instead of using the `.mdl` file as an input, users should directly make a call to `seas`. The R-code above was generated by:
# 
#     static(exmaple3, coef = TRUE)
# 
# With `coef = FALSE`, the specification of the regARIMA is fixed, but the coefficients are estimated.
# - `regression.aictest` has to be turned of for fully manual variable specification.

# 
# 
# ## 7.6 FORCE
# 
# ### Example 1
# 
#     SERIES  { TITLE="EXPORTS OF TRUCK PARTS" START =1967.1
#              FILE = "X21109.ORI"   }
#     PICKMDL {   }
#     X11  { SEASONALMA = S3X9    }
#     FORCE  {  START = OCTOBER   }
# 
# #### R-code:
# 
    seas(AirPassengers, 
         pickmdl = list(), 
         x11.seasonalma = "S3X9",
         force.start = "oct"
         )

# ### Example 2
# 
#     SERIES  { TITLE="EXPORTS OF TRUCK PARTS" START =1967.1
#               FILE = "X21109.ORI"   }
#     PICKMDL {   }
#     X11  { SEASONALMA = S3X9    }
#     FORCE  {  START = OCTOBER
#               TYPE = REGRESS
#               RHO = 0.8
#     }
# 
# #### R-code:
#   
    seas(AirPassengers, 
         pickmdl = list(), 
         x11.seasonalma = "S3X9",
         force.start = "oct",
         force.type = "regress",
         force.rho = 0.8
    )

# ### Example 3
# 
#     Series  { Title="Imports Of Truck Engines" Start =1967.1
#               File = "I21110.Ori"   }
#     Pickmdl {   }
#     X11  { Seasonalma = S3X5    }
#     Force  {  Type = None }
# 
# #### R-code:
#   
    seas(AirPassengers, 
         pickmdl = list(), 
         x11.seasonalma = "S3X5",
         force.type = "none"
    )

# 
# ## 7.7 FORECAST
# 
# ### Example 1
# 
#     SERIES { TITLE = "Monthly sales"  START = 1976.JAN
#              DATA = (138 128 ... 297) }
#     TRANSFORM { FUNCTION = LOG } REGRESSION { VARIABLES = TD } ARIMA {
#       MODEL = (0 1 1)(0 1 1)12 } FORECAST { }
# 
# #### R-code:
#   
    seas(AirPassengers, 
         transform.function = "log", 
         regression.variables = "td",
         arima.model = "(0 1 1)(0 1 1)12",
         forecast = list()
    )

# ### Example 2
# 
#     Series { Title = "Monthly Sales"  Start = 1976.jan
#              Data = (138 128 ... 297) }
#     Transform { Function = Log}
#     Regression { Variables = Td }
#     Arima { Model = (0 1 1)(0 1 1)12 }
#     Estimate { }
#     Outlier { }
#     Forecast { Maxlead = 24 }
# 
# #### R-code:
#   
    seas(AirPassengers, 
         transform.function = "log", 
         regression.variables = "td",
         arima.model = "(0 1 1)(0 1 1)12",
         forecast.maxlead = 24
    )

# #### Remark(s):
# - The `estimate` and `outlier` spec are activated by default.
# 
# ### Example 3
# 
#     series { title = "Monthly sales"  start = 1976.jan
#              data = (138 128 ... 297) }
#     transform { function = log }
#     regression { variables = td }
#     arima { model = (0 1 1)(0 1 1)12 }
#     estimate { }
#     forecast { maxlead = 15
#                probability = .90
#                exclude = 10 }
# 
# #### R-code:
#   
    seas(AirPassengers, 
         transform.function = "log", 
         regression.variables = "td",
         arima.model = "(0 1 1)(0 1 1)12",
         forecast.maxlead = 15,
         forecast.probability = 0.9,
         forecast.exclude = 10
    )

# 
# ### Example 4
# 
#     series { title = "Monthly sales"  start = 1976.jan
#              data = (138 128 ... 297)
#              span = ( ,1990.mar) }
#     transform { function = log}
#     regression { variables = td }
#     arima { model = (0 1 1)(0 1 1)12 }
#     estimate { }
#     forecast { maxlead = 24 }
# 
# #### R-code:
#   
    seas(window(AirPassengers,  end = c(1958, 3)),
         transform.function = "log", 
         regression.variables = "td",
         arima.model = "(0 1 1)(0 1 1)12",
         forecast.maxlead = 24
    )

# #### Remark(s):
# - Handling the `series.span` argument with the base funcion `window` is more  elegant and R-like.
# 
# ### Example 5
# 
#     series { title = "monthly sales"  start = 2000.jan
#              file = "ussales.dat"     }
#     transform { function = log }
#     regression { variables = td }
#     arima { model = (0 1 1)(0 1 1)12 }
#     forecast {  maxback=12  }
#     x11{   }
# 
# #### R-code:
#   
    seas(AirPassengers, 
         transform.function = "log", 
         regression.variables = "td",
         arima.model = "(0 1 1)(0 1 1)12",
         forecast.maxback = 12,
         x11 = list()
    )

# ### Example 6
# 
#     Series { Title = "Monthly Sales"  Start = 1976.jan
#              Data = (138 128 ... 297) }
#     Transform { Function = Log}
#     Regression { Variables = Td }
#     Arima { Model = (0 1 1)(0 1 1)12 }
#     Estimate { }
#     Outlier { }
#     Forecast { Maxlead = 24  Lognormal = Yes }
# 
# #### R-code:
#   
    seas(AirPassengers, 
         transform.function = "log", 
         regression.variables = "td",
         arima.model = "(0 1 1)(0 1 1)12",
         forecast.maxlead = 24,
         forecast.lognormal = "yes"
    )


# 
# ## 7.8 HISTORY
# 
# ### Example 1
# 
#     Series {  Title = "Sales Of Livestock"  Start = 1967.1
#               File = "cattle.ori"  }
#     X11 {  SeasonalMA = S3X9   }
#     History  {  sadjlags = 2  }
# 
# #### R-code:
#   
    m <- seas(AirPassengers, 
              x11.seasonalma = "S3X3"
    )
    revisions(m, 
              history.sadjlags = 2
              )

# #### Remark(s):
# - For an unknown X-13-ARIMA SEATS related reason, the `S3X9` filter does not work here. 
# - For the history spec, use `revisions` with the appropriate options on the `"seas"` object.
# 
# ### Example 2
# 
#     series     {  title = "Exports of Leather goods"
#                   start = 1969.jul  file = "expleth.dat"  }
#     regression { variables = (const td ls1972.may ls1976.oct) } arima { model=(012)(110) }
#     estimate { }
#     history { estimates = fcst fstep = 1 start=1975.jan }
# 
# #### R-code:
#   
    m <- seas(AirPassengers, 
         regression.variables = c("const", "td", "ls1952.may", "ls1956.oct"),
         arima.model= "(0 1 2)(1 1 0)",
         x11.seasonalma = "S3X9"
    )
    revisions(m, 
              history.estimates = "fcst",
              history.fstep = 1,
              history.start = "1955.jan"
    )

# #### Remark(s):
# - For the history spec, use `revisions` with the appropriate options on the `"seas"` object.
# 
# ### Example 3
# 
#     series     {  title = "Exports of Leather goods"
#                   start = 1969.jul
#                   file = "expleth.dat" }
#     regression { variables = (const td ls1972.may ls1976.oct) } arima { model=(012)(110) }
#     estimate { }
#     history { estimates = fcst save = r6 start = 1975.jan }
# 
# #### R-code:
#   
    m <- seas(AirPassengers, 
         regression.variables = c("const", "td", "ls1952.may", "ls1956.oct"),
         arima.model= "(0 1 2)(1 1 0)",
    )

    revisions(m, 
              history.estimates = "fcst",
              history.save = "r6",
              history.start = "1955.jan",
              dir = "~/userdir"
    )

# #### Remark(s):
# - For the history spec, use `revisions` with the appropriate options on the 
# `"seas"` object.
# - If you are interested in non-supported output tables, use the `dir` argument to 
# copy the X-13ARIMA-SEATS output files to a user defined directory (see `?seas`).
# 
# 
# ### Example 4
# 
#     series {  title = "Housing Starts in the Midwest"
#               start = 1967.1
#               file = "hsmwtot.ori"
#               modelspan = (,0.Dec)
#               comptype=add
#     }
#     regression { variables = td } arima{model=(012)(011) }
#     x11 { seasonalMA = S3X3 }
#     history { estimates = (sadj trend) }
# 
# #### R-code:
#   
    seas(AirPassengers, 
         series.modelspan = ", 0.dec",
         regression.variables = "td",
         arima.model= "(0 1 2)(0 1 1)",
         x11.seasonalma = "S3X9", 
         history.estimates = c("sadj", "trend"),
         dir = "~/userdir"
    )

# #### Remark(s):
# - Output and diagnostics are handeled by the seasonal package. 
# - If you are interested in non-supported output tables, use the `dir` argument to 
# copy the X-13ARIMA-SEATS output files to a user defined directory (see `?seas`).
# 
# ### Example 5
# 
#     composite{ title = "Total Housing Starts in the US"
#                modelspan = (,0.Dec)
#     }
#     regression { variables = td } arima{model=(011)(011) } x11 { seasonalMA = S3X3 }
#     history { estimates = (sadj trend)
#               save = (sar iar trr)  }
# 
# #### R-code:
#   
    seas(AirPassengers, 
         regression.variables = "td",
         arima.model= "(0 1 2)(0 1 1)",
         x11.seasonalma = "S3X9", 
         history.estimates = c("sadj", "trend"),
         dir = "~/userdir"
    )

# The `composite` spec is currently not supported. 
# 
# 
# ## 7.9 METADATA
# 
# #### Remark(s):
# - Output and diagnostics are handeled by the seasonal package. No need to use this spec.
# 
# 
# ## 7.10 IDENTIFY
# 
# #### Remark(s):
# - For ACFs and PACF, use the R functions `acf` and `pacf` on the series of interest. 
# - If you are interested in non-supported output tables, use the `dir` argument to 
# copy the X-13ARIMA-SEATS output files to a user defined directory (see `?seas`).
# 
# 
# ## 7.11 OUTLIER
# 
# ### Example 1
# 
#     series {  title = "Monthly sales"  start = 1976.jan
#               data = (138 128 ... 297) }
#     arima {   model = (0 1 1)(0 1 1)12 }
#     outlier { lsrun = 5  types=(ao ls) }
# 
# #### R-code:
#   
    seas(AirPassengers, 
         arima.model= "(0 1 1)(0 1 1)12",
         outlier.lsrun = 5, 
         outlier.types = c("ao", "ls")
    )
# 
# ### Example 2
# 
#     Series { Title = "Monthly Sales"  Start = 1976.Jan
#              Data = (138 128 ... 297)
#              Span = (1980.Jan, 1992.Dec) }
#     Regression { Variables = (LS1981.Jun LS1990.Nov) }
#     Arima { Model = (0 1 1)(0 1 1)12 }
#     Estimate {  }
#     Outlier { Types = AO   Method = Addall  Critical = 4.0  }
# 
# #### R-code:
#   
    seas(window(AirPassengers, start = c(1950, 1), end = c(1959, 12)), 
         regression.variables = c("ls1951.jun", "ls1952.nov"),
         arima.model= "(0 1 1)(0 1 1)12",
         outlier.lsrun = 5, 
         outlier.types = "ao",
         outlier.method = "addall",
         outlier.critical = 4
    )
# 
# 
# ### Example 3
# 
#     series { title = "Monthly sales"  start = 1976.jan
#              data = (138 128 ... 297)
#              span = (1980.jan, 1992.dec) } arima{ model=(011)(011)12}
#     estimate { }
#     outlier { types = ls
#               critical = 3.0
#               lsrun = 2
#               span = (1987.jan, 1988.dec) }
# 
# #### R-code:
#   
    seas(window(AirPassengers, start = c(1950, 1), end = c(1959, 12)),
         outlier.types = "ls",
         outlier.critical = 3,
         outlier.lsrun = 2, 
         outlier.span = "1953.jan, 1958.dec"
    )
# 
# ### Example 4
# 
#     series { title = "Monthly sales"  start = 1976.jan
#              data = (138 128 ... 297)
#              span = (1980.jan, 1992.dec) } arima{model =(011)(011)12}
#     estimate { }
#     outlier { critical = (3.0, 4.5, 4.0)
#               types = all }
# 
# #### R-code:
#   
    seas(window(AirPassengers, start = c(1950, 1), end = c(1959, 12)), 
         arima.model= "(0 1 1)(0 1 1)12",
         outlier.critical = c(3, 4.5, 4),
         outlier.types = "all"
    )

# 
# 
# ## 7.12 PICKMDL
# 
# ### Example 1
#     series      { title = "Monthly sales"   start = 1976.jan
#                     data = (138 128  ...  297) }
#     regression { variables = (td seasonal) }
#     pickmdl { mode = fcst file = "nosdiff.mdl" } estimate { }
#     x11 {}
# 
# #### R-code:
# 
    seas(AirPassengers, 
         x11 = list(),
         pickmdl.mode = "fcst"
    )
 
# #### Remark(s):
# - If the `x11` spec is specified (here, as an empty `list`), the default `seats` spec is automatically disabled. 
# - If the `pickmdl` spec is specified, the default `automdl` spec is automatically disabled.
# 
# ### Example 2
# 
#     series      { title = "Monthly sales"   start = 1976.jan
#                     data = (138 128  ...  297) }
#     regression  { variables = td }
#     pickmdl     { mode = fcst    file = "nosdiff.mdl"
#                     method = first   fcstlim = 20   qlim = 10
#                     overdiff = 0.99  identify = all }
#     outlier     {  }
#     estimate { } 
#     x11 {}
#     
# #### R-code:
# 
    seas(AirPassengers, 
         x11 = list(),
         regression.variables = "td",
         pickmdl.mode = "fcst",
         pickmdl.method = "first",
         pickmdl.fcstlim = 20,
         pickmdl.qlim = 10,
         pickmdl.overdiff = 0.99,
         pickmdl.identify = "all"
    )
 
# 
# ### Example 3
#     series      { title = "Monthly sales"   start = 1976.jan
#                     data = (138 128  ...  297) }
#     regression  { variables = td }
#     pickmdl     { mode = fcst    file = "nosdiff.mdl"
#                     outofsample=yes  }
#     estimate    {   }
#     x11 {}
# 
# #### R-code:
# 
    seas(AirPassengers, 
         x11 = list(),
         regression.variables = "td",
         pickmdl.mode = "fcst",
         pickmdl.outofsample = "yes"
    )
# 
# 
# ## 7.13 REGRESSION
# 
# 
# ### Example 1
# 
#     SERIES      { TITLE = "Monthly sales"  START = 1976.JAN
#                      DATA = (138 128 ... 297) }
#       REGRESSION { VARIABLES = (CONST SEASONAL) }
#       ARIMA { MODEL = (0 1 1) }
#       ESTIMATE { }
# 
# #### R-code:
#   
    seas(AirPassengers, 
         regression.aictest = NULL,
         regression.variables = c("const", "seasonal"),
         arima.model = "(0 1 1)"
    )
# 
# #### Remark(s):
# - `regression.aictest` has to be turned of for fully manual variable specification.
# 
# ### Example 2
# 
#     series { title = "Irregular Component of Monthly Sales"
#              start = 1976.jan
#              file = "sales.d13"
#              format = "x13save"
#            }
#     regression { variables = (const sincos[4,5]) }
#     estimate { }
#     spectrum { savelog=peaks }
# 
# #### R-code:
#   
    seas(AirPassengers, 
         x11 = list(),
         regression.aictest = NULL,
         regression.variables = c("const", "sincos[4,5]"),
         spectrum.savelog = "peaks"
    )
#          
# #### Remark(s):
# - If you are interested in non-supported output tables, use the `dir` argument to copy the X-13ARIMA-SEATS output files to a user defined directory (see `?seas`).
# - `regression.aictest` has to be turned of for fully manual variable specification.
# 
# ### Example 3
# 
#     Series { Title = "Monthly Sales"  Start = 1976.Jan
#               Data = (138 128 ... 297) }
#     Transform { Function = Log }
#     Regression { Variables = (TD Easter[8] Labor[10] Thank[3]) }
#     Identify { Diff = (0 1) SDiff = (0 1) }
# 
# #### R-code:
#   
    seas(AirPassengers,
         transform.function = "log",
         regression.aictest = NULL,
         regression.variables = c("const", "easter[8]", "thank[3]"),
         identify.diff = c(0, 1),
         identify.sdiff = c(0, 1)
    )
# 
# #### Remark(s):
# - `regression.aictest` has to be turned of for fully manual variable specification.
# 
# ### Example 4
# 
#     series      { title = "Monthly sales"  start = 1976.jan
#                     data = (138 128 ... 297) }
#     transform { function = log }
#     regression { variables = (tdnolpyear lom easter[8] labor[10] thank[3])
#                    aictest = (lom td easter) }
#     arima { model = (0 1 1)(0 1 1) }
#     estimate { }
# 
# #### R-code:
#   
    seas(AirPassengers,
         transform.function = "log",
         regression.aictest = NULL,
         regression.variables = c("tdnolpyear", "lom", "easter[8]", "labor[10]",
                                  "thank[3]"),
         arima.model = "(0 1 1)(0 1 1)"
    )
# 
# #### Remark(s):
# - `regression.aictest` has to be turned of for fully manual variable specification.
# 
# ### Example 5
# 
#     series     {  title = "Retail inventory of food products"
#                     start = 1990.jan  data = "foodri.dat"  type = stock
#                  }
#     regression {  variables = ( tdstock1coef[31]  easterstock[8] )
#                     aictest = ( td easter )
#                  }
#     arima { model=(011)(011) } x11{ }
# 
# #### R-code:
#   
    seas(AirPassengers,
         transform.function = "log",
         regression.variables = c("tdstock1coef[31]", "easterstock[8]"),
         arima.model = "(0 1 1)(0 1 1)",
         x11 = list()
    )
# 
# 
# ### Example 6
# 
#     Series     { Title  = "Quarterly Sales"  Start = 1990.1  Period = 4
#                    Data  = (1039 1241 ...  2210)  }
#     Transform { Function = Log }
#     Regression { Variables = (AO2007.1 RP2005.2-2005.4 AO1998.1 TD) } 
#     Arima { Model=(011)(011)}
#     Estimate { }
# 
# #### R-code:
#   
    seas(AirPassengers,
         transform.function = "log",
         regression.aictest = NULL,
         regression.variables = c("ao1950.1", "rp1950.2-1950.4", "ao1951.1", "td"),
         arima.model = "(0 1 1)(0 1 1)"
    )

# #### Remark(s):
# - `regression.aictest` has to be turned of for fully manual variable specification.
# 
# 
# ### Example 7
# 
#     Series     { Title  = "Quarterly Sales"  Start = 1990.1  Period = 4
#                    Data  = (1039 1241 ...  2210)  }
#     Transform { Function = Log }
#     Regression { Variables = (AO2007.1 QI2005.2-2005.4 AO1998.1 TD) } 
#     Arima { Model=(011)(011)}
#     Estimate { }
#     
# #### R-code:
#   
    seas(AirPassengers,
         transform.function = "log",
         regression.aictest = NULL,
         regression.variables = c("ao1950.1", "qi1950.2-1950.4", "ao1951.1", "td"),
         arima.model = "(0 1 1)(0 1 1)"
    )
# 
# #### Remark(s):
# - `regression.aictest` has to be turned of for fully manual variable specification.
# 
# 
# 
# 
# ### Example 8
# 
# 
#     series {title = "Quarterly sales"  start = 1981.1
#                data = (301 294 ...  391)  period = 4  }
#     regression {user = tls
#                   data = (0 0 0 0 0 0 0 0 0 0 0 0 ...
#                           0 0 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 ... 0) }
#     identify   { diff = (0 1) sdiff = (0 1) }
# 
# #### R-code:
#   
#     # user defined regressor
    tls <- ts(0, start = 1949, end = 1965, freq = 12)
    window(tls, start = c(1955, 1), end = c(1957, 12)) <- 1
    
    seas(AirPassengers, 
         xreg = tls, 
         identify.diff = c(0, 1),
         identify.sdiff = c(0, 1),
         outlier = NULL
    )

# ### Example 9
# 
#     series {title = "Quarterly sales"  start = 1981.1
#                data = (301 294 ...  391)  period = 4  }
#     regression {  variables = tl1985.03-1987.01  }
#     identify   { diff = (0 1) sdiff = (0 1) }
# 
# #### R-code:
#   
    seas(AirPassengers, 
         regression.variables = c("tl1955.01-1957.12"),
         identify.diff = c(0, 1),
         identify.sdiff = c(0, 1),
         outlier = NULL)

# #### Remark(s):
# - `outlier` has to be turned of for manual outlier specification.
# 
# ### Example 10
# 
#     series { title = "Monthly Riverflow"      start = 1970.1
#               data  = (8.234 8.209 ...  8.104) period = 12 }
#     regression { variables = (seasonal const)
#                   user = (temp precip)
#                   file = "weather.dat"
#                   format = "(t17,2f8.2)"
#                   start = 1960.1 }
#     arima { model = (3 0 0)(0 0 0) }
#     estimate { }
# 
# #### R-code:
    temp = ts(runif(200), start = 1948, frequency = 12)
    precip = ts(runif(200), start = 1948, frequency = 12)
    seas(AirPassengers,
         x11 = list(),
         xreg = cbind(temp, precip),
         regression.variables = c("seasonal", "const"),
         arima.model = "(3 0 0)(0 0 0)",
         regression.aictest = NULL
    )
#     
# #### Remark(s):
# - use R time series as external variables.
# - `regression.aictest` has to be turned of for fully manual variable specification.
# 
# ### Example 11
# 
#     series {title = "Retail Inventory - Family Apparel"
#             start = 1967.1  period = 12
#             data = (1893 1932 ... 3201 )
#             type = stock }
#     transform {   function = log }
#     regression {  variables = (tdstock[31] ao1980.jul)
#                   aictest=tdstock }
#     arima { model = (0 1 0)(0 1 1) }
#     estimate    { }
# 
# #### R-code:
# 
    seas(AirPassengers,
         transform.function = "log",
         regression.variables = c("tdstock[31]", "ao1950.jul"),
         arima.model = "(0 1 0)(0 1 1)",
         regression.aictest = "tdstock"
    )
#     
#     
# 
# 
# 
# 
# ### Example 12
# 
#     series { title = "Retail Sales - Televisions"
#              start = 1976.1  period = 12  type = flow
#              file  = ’tvsales.ori’  }
#     transform { function = log }
#     regression {  variables = (td/1985.dec/ seasonal/1985.dec/) }
#     arima { model = (0 1 1) }
#     estimate { }
# 
# #### R-code:
# 
    seas(AirPassengers,
         transform.function = "log",
         regression.variables = c("td/1952.dec/", "seasonal/1952.dec/"),
         arima.model = "(0 1 1)",
         x11 = list()
    )
#     
# 
# ### Example 13
# 
#      series {title = "Retail Sales - Televisions"
#               start = 1976.1  period = 12  type = flow
#               file  = ’tvsales.ori’   }
#     transform  { function = log }
#     regression { variables = (td td//1985.dec/
#     seasonal seasonal//1985.dec/) } arima{ model=(011)}
#     estimate { }
# 
# #### R-code:
# 
    seas(AirPassengers,
         transform.function = "log",
         regression.variables = c("td", "td//1952.dec/", "seasonal", 
                                  "seasonal//1952.dec/"),
         arima.model = "(0 1 1)",
         x11 = list()
    )
    
# 
# ### Example 14
# 
#     Series     { Title  = "Quarterly Sales"  Start = 1993.1  Period = 4
#                    Data  = (1039 1241 ...  2210)  }
#     Transform { Function = Log }
#     Regression { Variables = (AO2001.3 LS2007.1 LS2007.3 AO2008.4) } 
#     Arima { Model=(011)(011)}
#     Estimate { }
# 
# #### R-code:
#   
    seas(AirPassengers,
         transform.function = "log",
         regression.variables = c("ao1950.1", "ls1952.2", "ls1952.3", "ao1951.1"),
         arima.model = "(0 1 1)(0 1 1)"
    )
# 
# ### Example 15
# 
#     Series     { Title  = "Quarterly Sales"  Start = 1993.1  Period = 4
#                    Data  = (1039 1241 ...  2210)  }
#     Transform { Function = Log }
#     Regression { Variables = (AO2001.3 TL2007.1-2007.2 AO2008.4) } 
#     Arima { Model=(011)(011)}
#     Estimate { }
# 
# #### R-code:
#   
    seas(AirPassengers,
         transform.function = "log",
         regression.variables = c("ao1950.1", "tl1952.2-1952.3", "ao1951.1"),
         arima.model = "(0 1 1)(0 1 1)"
    )

# ### Example 16
# 
#       Series     { Title  = "Quarterly Sales"  Start = 1993.1  Period = 4
#                    Data  = (1039 1241 ...  2210)  }
#     Transform { Function = Log }
#     Regression { Variables = (AO2001.3 LSS2007.1-2007.3 AO2008.4) } 
#     Arima { Model=(011)(011)}
#     Estimate { }
# 
# #### R-code:
#   
    seas(AirPassengers,
         transform.function = "log",
         regression.variables = c("ao1950.1", "ls1952.2-1952.3", "ao1951.1"),
         arima.model = "(0 1 1)(0 1 1)"
    )

# ### Example 17
# 
#     series     {  title = "Exports of pasta products"
#                     start = 1980.jan  data = "pasta.dat" }
#       regression {  variables = (const td)               }
#       automdl    {                                       }
#       x11        {  mode = add                           }
# 
# #### R-code:
#   
    seas(AirPassengers,
         transform.function = "none",
         regression.variables = c("const", "td"),
         x11.mode = "add"
    )
# 
# ### Example 18
# 
#     series{ title = "Retail sales of children’s apparel"
#      file = "capprl.dat" start = 1975.1  }
#     transform{   function = log }
#     regression{
#       variables = (const td ao1976.oct ls1991.dec easter[8] seasonal)
#       user = (sale88 sale89 sale90)
#       start = 1975.1   file = "promo.dat"   format = "(3f12.0)"  }
#     arima{   model = (2 1 0)   }
#     forecast{   maxlead = 24   }
#     x11{   save=seasonal  appendfcst=yes  }
# 
# #### R-code:
#   
    ser1 = ts(runif(200), start = 1948, frequency = 12)
    ser2 = ts(runif(200), start = 1948, frequency = 12)
    ser3 = ts(runif(200), start = 1948, frequency = 12)
    seas(AirPassengers,
         transform.function = "none",
         xreg = cbind(ser1, ser2, ser3),
         regression.variables = c("const", "td", "ao1956.oct", "ls1951.dec",
                                  "easter[8]", "seasonal"),
         arima.model = c(2, 1, 0),
         x11.appendfcst = "yes"
    )
# 
# ### Example 19
# 
#     series{ title = "Retail sales of children’s apparel"
#        file = "capprl.dat" start = 1975.1  }
#       transform{   function = log }
#       regression{
#         variables = (const td ao1976.oct ls1991.dec easter[8]
#                      seasonal)
#         user = (sale88 sale89 sale90)
#         start = 1975.1   file = "promo.dat"   format = "(3f12.0)"
#         usertype = ao
#       }
#       arima{   model = (2 1 0)   }
#       forecast{   maxlead = 24   }
#       x11{   save=seasonal  appendfcst=yes   }
# 
# #### R-code:
#   
    ser1 = ts(runif(200), start = 1948, frequency = 12)
    ser2 = ts(runif(200), start = 1948, frequency = 12)
    ser3 = ts(runif(200), start = 1948, frequency = 12)
    seas(AirPassengers,
         transform.function = "none",
         xreg = cbind(ser1, ser2, ser3),
         regression.usertype = "ao",
         regression.variables = c("const", "td", "ao1956.oct", "ls1951.dec",
                                  "easter[8]", "seasonal"),
         arima.model = c(2, 1, 0),
         x11.appendfcst = "yes"
    )

# ### Example 20
# 
#        series{
#            format = "2L"
#            title  = "Midwest Total Starts"
#            file   = "mwtoths.dat"
#            name   = "MWTOT "
#     }
#     transform{ function=log } 
#     arima{ model=(012)(011) } 
#     estimate{ save=mdl } 
#     regression{
#            variables = (ao1977.jan ls1979.jan ls1979.mar ls1980.jan td)
#            b = (  -0.7946F  -0.8739F  0.6773F  -0.6850F  0.0209
#                   ~0.0107   -0.0022   0.0018   ~0.0088  -0.0074  )
#     }
#     x11{ }
# 
# #### R-code:
#   
    seas(AirPassengers,
         transform.function = "log",
         regression.variables = c("ao1957.jan", "ls1959.jan", "ls1959.mar", 
                                "ls1960.jan", "td"),
         regression.b = c("-0.7946f", "-0.8739f", "0.6773f", "-0.6850f", 
                          "0.0209", "-0.0107", "-0.0022", "0.0018", "-0.0088", 
                          "-0.0074"),
         regression.aictest = NULL,
         arima.model =  "(0 1 2)(0 1 1)", 
         x11 = list()
    )
# 
# #### Remark(s):
# - use `static(m, coef = TRUE)` to extract a call with fixed coefficients.
# - `regression.aictest` has to be turned of for fully manual variable specification.
# 
# 
# ### Example 21
# 
#     Series {
#         Format="1L"   File="bdptrs.dat"   Name="BDPTRS"
#         Title="Department Store Sales"  }
#       Transform {  Function=Log  }
#       Regression {  Variables=( Td Easter[8] )
#                     Save = ( Td Holiday )  }
#       Arima {   Model=(0 1 1)(0 1 1)  }
#     Example 21
#     Outlier {
#     Estimate {
#     Check {
#     Forecast {  }
#     X11 {Mode = Mult   Seasonalma = S3X3
#       Title = ("Department Store Retail Sales Adjusted For")
#                "Outlier, Trading Day, And Holiday Effects" )
#     }
# 
# #### R-code:
#   
    seas(AirPassengers,
         transform.function = "log",
         regression.variables = c("td", "easter[8]"),
         regression.aictest = NULL,
         arima.model =  "(0 1 1)(0 1 1)", 
         x11.mode = "mult",
         x11.seasonalma = "S3X3"
    )
# 
# #### Remark(s):
# - `regression.aictest` has to be turned of for fully manual variable specification.
# 
# ### Example 22
# 
#     series{ title = "US Total Housing Starts"
#        file = "ustoths.dat" start = 1990.1
#        period = 4  save = b1}
#       transform{   function = log }
#       regression{
#         user = (s1 s2 s3)
#         usertype = seasonal
#         start = 1985.1   file = "seasreg.rmx"
#         format = "x13save"
#       }
#       outlier{   }
#       arima{   model = (0 1 1)   }
#       forecast{   maxlead = 24   }
# 
# #### R-code:
#   
    ser1 = ts(runif(200), start = 1948, frequency = 12)
    ser2 = ts(runif(200), start = 1948, frequency = 12)
    ser3 = ts(runif(200), start = 1948, frequency = 12)
    seas(AirPassengers,
         transform.function = "log",
         xreg = cbind(ser1, ser2, ser3),
         regression.usertype = "seasonal",
         regression.aictest = NULL,
         arima.model = c(0, 1, 1),
         forecast.maxlead = 24
    )
#     
# #### Remark(s):
# - use R time series as external variables.
# - `regression.aictest` has to be turned of for fully manual variable specification.
# 
# ### Example 23
# 
#     series{
#         file="serv.dat"  start=1991.jan  span=(1993.jan,)
#         title = "Payment to family nanny, taiwan"
#     }
#     transform{    function=log   }
#     regression{
#        variables = (  AO1995.Sep AO1997.Jan AO1997.Feb  )
#        user=(    Beforecny      Betweencny      Aftercny
#         Beforemoon     Betweenmoon     Aftermoon
#         Beforemidfall  Betweenmidfall  Aftermidfall  )
#        file="u1u2u3.dat"
#        format="datevalue"
#        start=1991.1
#     Example 23
#        usertype=(   holiday
#         holiday2    holiday2
#         holiday3    holiday3
#        chi2test = yes
#        savelog = chi2test
#     }
#     holiday
#     holiday2
#     holiday3  )
#     holiday
#     arima{  model=(0 1 1)(0 1 0)
#     check{  }
#     forecast{  maxlead=12  }
#     estimate{  savelog=(aic aicc bic)  }
# 
# 
# 
# 
# 
# #### R-code:
#   
#     # construct chinese new year time series with 'genhol' function
    data(holiday)
    cny1 <- genhol(cny, start = -6, end = -1, frequency = 12, center = "calendar")
    cny2 <- genhol(cny, start = 0, end = 6, frequency = 12, center = "calendar")

    seas(AirPassengers,
         transform.function = "log",
         xreg = cbind(cny1, cny2),
         regression.usertype = c("holiday", "holiday2"),
         regression.variables = c("AO1955.Sep", "AO1957.Jan", "AO1957.Feb"),
         arima.model = "(0 1 1)(0 1 0)",
         forecast.maxlead = 12,
         x11 = list()
    )
# 
# #### Remark(s):
# - use `genhol`to construct holiday time series (see `?genhol`).
# 
# 
# 
# ## 7.14 SEATS
# 
# ### Example 1 
# 
#     SERIES  { TITLE="EXPORTS OF TRUCK PARTS"
#               START =1987.1
#               FILE = "X21109.ORI"
#               PERIOD = 12
#     }
#     TRANSFORM {  FUNCTION = AUTO  }
#     REGRESSION {  AICTEST = TD  }
#     AUTOMDL {   }
#     OUTLIER {  TYPES = (AO LS TC)  }
#     FORECAST {  MAXLEAD = 36  }
#     SEATS  { SAVE = S11  }
# 
# #### R-code:
#   
    seas(AirPassengers, 
         regression.aictest = "td",
         outlier.types = c("ao", "ls", "tc"),
         forecast.maxlead = 36
    )
# 
# #### Remark(s):
# - `transform.function = "auto"`, and `automdl = list()` and `seats = list()` are 
#          activated by default.
# 
# ### Example 2 
# 
#     Series  { Title="Quarterly Exports Of Mangos"
#               Start =1990.1  File = "Xmango.Ori"  Period = 4 }
#     Transform { Function = Log } Regression { Aictest = Td } Arima{ Model=(011)(011) } Forecast { Maxlead = 12 }
#     Seats {  Finite = yes
#              Save = ( Squaredgainsaconc Timeshiftsaconc )
#              Savelog = Overunderestimation
#     }
#     History { Estimates = (Sadj Trend)
#               Save = ( Sarevisions Trendrevisions )
#     }
# 
# #### R-code:
#   
    m <- seas(AirPassengers, 
              regression.aictest = "td",
              arima.model = "(0 1 1)(0 1 1)",
              forecast.maxlead = 12,
              seats.finite = "yes"
    )
    revisions(m, 
              history.estimates = c("sadj", "trend"),
              history.save = c("sarevisions", "trendrevisions"),
              dir = "~/userdir"
    )

# #### Remark(s):
# - If you are interested in non-supported output tables, use the `dir` argument to 
# copy the X-13ARIMA-SEATS output files to a user defined directory (see `?seas`).
# - For the history spec, use `revisions` with the appropriate options on the 
# `"seas"` object.
# 
# 
# ### Example 3 
# 
#     Series  { Title="Model based adjustment of Bimonthly exports"
#               Start = 1995.1  File = "Xports6.Ori"  Period = 6 }
#     Transform { Function = Log } Regression { Variables = Td } 
#     Arima{ Model=(011)(011) } Outlier { types = (ao ls tc) } 
#     Forecast { Maxlead = 18 }
#     Seats {  save = (S11 S10 S12)  }
# 
# #### R-code:
#   
    # bimonthly data
    require(tempdisagg)
    AirPassengersBM <- ta(AirPassengers, to = 6)
    m <- seas(AirPassengersBM, 
         regression.aictest = NULL,
         outlier.types = c("ao", "ls", "tc"),
         forecast.maxlead = 18
    )
    final(m) 
# 
# #### Remark(s):
# - X-13ARIMA-SEATS needs monthly or quarterly data for trading day.
# - `regression.aictest` has to be turned of for fully manual variable specification.
# 
# 
# 
# ## 7.15 SERIES
# 
# ### Example 1
# 
#     series{
#       title = "A Simple Example"
#       start = 1967.jan    # period defaults to 12
#     data=(480 467 514 505 534 546 539 541 551 537 584 
#     854 522 506 558 538 605 583 607 624 570 609 675 861 .
#           .
#           .
#           1684 1582 1512 1508 1574 2303 1425 1386) }
# 
# #### R-code:
#   
    seas(AirPassengers)
# 
# #### Remark(s):
# - Time series specification is handled by `seasonal`. No manual input needed.
# 
# ### Example 2
# 
#     series { data = (879 899 985 ...)   # There are 216 data values
#              start = 1940.1              #       ending in 1993.4
#              period = 4                # Quarterly series
#              span = (1946.1, 1990.4)  }
# 
# #### R-code:
#   
#     seas(window(AirPassengers, start = c(1950, 1), end = c(1959, 12))
# 
# #### Remark(s):
# - Handling the `series.span` argument with the base funcion `window` is more 
# elegant and R-like.
# 
# ### Example 3
# 
#     SERIES{ TITLE = "Monthly data in an X-11 format"
#             PERIOD = 12
#             FILE = "C:\DATA\SALES1.DAT"    # a DOS path and file
#             PRECISION = 1
#             FORMAT = "1r" }
# 
# #### R-code:
#   
    seas(AirPassengers)
# 
# #### Remark(s):
# - Time series specification is handled by `seasonal`. No manual input needed.
# 
# 
# ### Example 4
# 
#     series {title = "Data read correctly in with trimzero = no"
#             start = 1980.2   period = 12
#             file = "example4.new" }    # file is in current directory
# 
# #### R-code:
#   
    seas(AirPassengers)
# 
# #### Remark(s):
# - Time series specification is handled by `seasonal`. No manual input needed.
# 
# ### Example 5
# 
#     SERIES{  TITLE = "Monthly data in a datevalue format"
#              PERIOD = 12
#              FILE = "C:\DATA\SALES1.EDT"    # a DOS path and file
#              FORMAT = "DATEVALUE"  TYPE = FLOW }
# 
# #### R-code:
#   
    seas(AirPassengers,
         series.type = "flow"
         )
# 
# ### Example 6
# 
#     SERIES{  TITLE = "Monthly data in a datevalue format"
#              PERIOD = 12
#              Example 6
#              Example 7
#              This example shows how the X-13ARIMA-SEATS program handles missing data. The same data format is used as in the previous two examples, except a missing value code is inserted for January of 1990:
#                FILE = "C:\DATA\SALES1.EDT"
#              FORMAT = "DATEVALUE"
#              COMPTYPE = ADD
#              DECIMALS = 2
#              MODELSPAN = (,1992.DEC)
#     }
# 
# #### R-code:
#   
    seas(window(AirPassengers, end = c(1952, 12)))
# 
# #### Remark(s):
# - Handling the `series.span` argument with the base funcion `window` is more 
# elegant and R-like.
# - The `composite` spec is currently not supported. Defining `comptype` has no 
# ueseful effect.
# 
# 
# ### Example 7
# 
#     SERIES{ TITLE = "Monthly data in a date-value format"
#             PERIOD = 12
#             FILE = "C:\DATA\SALES1.EDT"    # a DOS path and file
#             FORMAT = "DATEVALUE"
#     }
# 
# #### R-code:
#   
    seas(AirPassengers)
# 
# #### Remark(s):
# - Time series specification is handled by `seasonal`. No manual input needed.
# 
# ### Example 8
# 
#     SERIES{ TITLE = "Monthly data in a file saved by \thisprogram\ "
#             PERIOD = 12
#             FILE = "C:\DATA\SALES1.A11"    # a DOS path and file
#             FORMAT = "X13SAVE" }
# 
# #### R-code:
#   
    seas(AirPassengers)
# 
# #### Remark(s):
# - Time series specification is handled by `seasonal`. No manual input needed.
# 
# ### Example 9
# 
#     SERIES{  TITLE = "Monthly data in the comma variant of datevalue format"
#              PERIOD = 12
#              FILE = "C:\DATA\SALES1C.EDT"    # a DOS path and file
#              FORMAT = "DATEVALUECOMMA"  }
# 
# #### R-code:
#   
    seas(AirPassengers)
# 
# #### Remark(s):
# - Time series specification is handled by `seasonal`. No manual input needed.
# 
# 
# 
# ## 7.16 SLIDINGSPANS
# 
# ### Example 1
# 
#     SERIES { FILE = "TOURIST.DAT"   START = 1976.1   }
#     X11 {   SEASONALMA = S3X9    }
#     SLIDINGSPANS {    }
# 
# #### R-code:
#   
    m <- seas(AirPassengers, 
              x11.seasonalma = "S3X9"
    )
    slidingspans(m)     
# 
# #### Remark(s):
# - For the slidingspans spec, use `slidingspans` with the appropriate options on 
# the `"seas"` object.
# 
# 
# ### Example 2
# 
#     Series       {
#       File = "qstocks.dat"
#       Start = 1967.1
#       Title = "Quarterly stock prices on NASDAC"
#       Freq = 4
#     }
#     X11 {
#       Seasonalma = (  S3x9 S3x9 S3x5 S3x5  )
#       Trendma = 7
#       Mode = Logadd
#     }
#     Slidingspans {
#       cutseas = 5.0
#       cutchng = 5.0
#     }
# 
# #### R-code:
#   
    m <- seas(AirPassengers, 
              transform.function = "log",
              x11.seasonalma = c("S3x9", "S3x9", "S3x5", "S3x5", "S3x5", "S3x5",
                                 "S3x5", "S3x5", "S3x5", "S3x5", "S3x5", "S3x5"),
              x11.trendma = 7,
              x11.mode = "logadd"
    )
    slidingspans(m, 
                 slidingspans.cutseas = 5,
                 slidingspans.cutchng = 5
    )        
# 
# #### Remark(s):
# - For the slidingspans spec, use `slidingspans` with the appropriate options on 
# the `"seas"` object.
# 
# ### Example 3
#     
#     series {  title = "Number of employed machinists - X-11"
#               start = 1980.jan  file = "machine.emp"
#     }
#     regression { variables = (const td rp82.may-82.oct) } arima {model=(012)(011)}
#     outlier {}
#     estimate {}
#     check {}
#     forecast {}
#     x11 { mode = add save = d11}
#     slidingspans { outlier = keep
#                    length = 144 }
# 
# #### R-code:
#   
    m <- seas(AirPassengers, 
              regression.variables = c("const", "td", "rp1952.may-1952.oct"),
              arima.model = "(0 1 2)(0 1 1)",
              x11.mode = "add",
              transform.function = "none"
    )
    slidingspans(m, 
                 slidingspans.outlier = "keep",
                 slidingspans.length = 50
    )         
# 
# #### Remark(s):
# - For the slidingspans spec, use `slidingspans` with the appropriate options on 
# the `"seas"` object.
# 
# ### Example 4
# 
#     series {  title = "Number of employed machinists - SEATS"
#               start = 1980.jan  file = "machine.emp"
#     }
#     regression { variables = (const td rp82.may-82.oct) } arima {model=(012)(011)}
#     outlier {}
#     estimate {}
#     check {}
#     forecast {}
#     seats { save = s11 }
#     slidingspans { outlier = keep
#                    length = 144 }
# 
# #### R-code:
#   
    m <- seas(AirPassengers, 
              regression.variables = c("const", "td", "rp1952.may-1952.oct"),
              arima.model = "(0 1 2)(0 1 1)"
    )
    slidingspans(m, 
                 slidingspans.outlier = "keep",
                 slidingspans.length = 50
    )        

# #### Remark(s):
# - For the slidingspans spec, use `slidingspans` with the appropriate options on 
# the `"seas"` object.
# 
# ### Example 5
# 
#     series { title = "Cheese Sales in Wisconsin"
#              file = "cheez.fil"   start = 1975.1   }
#     transform { function = log }
#     regression { variables = (const seasonal tdnolpyear) } arima{ model=(310) }
#     forecast { maxlead = 60 }
#     x11 { save = seasonal appendfcst = yes } slidingspans { fixmdl = no }
# 
# #### R-code:
# 
    m <- seas(AirPassengers, 
              transform.function = "log",
              regression.variables = c("const", "seasonal", "tdnolpyear"),
              arima.model = "(3 1 0)",
              x11.appendfcst = "yes"
    )
    slidingspans(m, 
                 slidingspans.fixmdl = "no"
    )

# #### Remark(s):
# - For the slidingspans spec, use `slidingspans` with the appropriate options on 
# the `"seas"` object.
# 
# ### Example 6
# 
#     Series       {
#       File = "qstocks.dat"
#       Start = 1987.1
#       Title = "Quarterly stock prices on NASDAC"
#       Freq = 4
#     }
#     X11 {
#       Seasonalma = S3x9
#     }
#     Slidingspans {
#       Length = 40
#       Numspans = 3 }
# 
# #### R-code:
#   
    m <- seas(AirPassengers, 
              x11.seasonalma = "S3X9"
    )
    slidingspans(m, 
                 slidingspans.length = 40,
                 slidingspans.numspans = 3
    )
# 
# #### Remark(s):
# - For the slidingspans spec, use `slidingspans` with the appropriate options on 
# the `"seas"` object.
# 
# 
# 
# ## 7.17 SPECTRUM
# 
# ### Example 1
# 
#     series{ title = "Spectrum analysis of Building Permits Series"
#             start = 1967.Jan
#             file = "permits.dat"
#             format = "(12f6.0)"
#             print = none
#     } transform{
#       function = log
#       print = none }
#     spectrum{
#       start = 1987.Jan
#       print = (none +specorig)
#       savelog = all
#     }
# 
# #### R-code:
#       
    m <- seas(AirPassengers, 
              transform.function = "log"
    )
    out(m, 
        spectrum.start = "1952.jan",
        spectrum.print = c("none", "+specorig")
    )

# #### Remark(s):
# - For spectral plots, use the R function `spectrum` on the series of interest. 
# - If you are interested in specific outputs from the `spectrum` spec, you can 
# analyze the X-13ARIMA-SETATS main output file with the `out` function, adding 
# the appropriate options.
# 
# ### Example 2
# 
#     composite {  title="TOTAL ONE-FAMILY Housing Starts"
#                  name="C1FTHS" save=(indseasonal) }
#     x11 { seasonalma=(s3x9)
#           title="Composite adj. of 1-Family housing starts"
#           save=(D10) }
#     spectrum { savelog = (indpeaks indqs)
#                type = periodogram
#                save = is1 }
# 
# #### R-code:
  
    m <- seas(AirPassengers, 
              transform.function = "log"
    )
    out(m, 
        spectrum.start = "1952.jan",
        spectrum.print = c("none", "+specorig")
    )

# #### Remark(s):
# - The `composite` spec is currently not supported. 
# 
# ## 7.18 TRANSFORM
# 
# ### Example 1
# 
#     series { data = (879 899 462 670 985 973 ...)
#               start = 1967.jan }
#     transform{data =(1 1.5.75 1 1...) mode = ratio
#     adjust = lom }
# 
# #### R-code:
#   
#     # adjustment series
    tf <- ts(1:250, start = c(1945, 1), frequency = 12)
    
    m <- seas(AirPassengers, 
              xtrans = tf,
              transform.mode = "ratio",
              transform.adjust = "lom",
              transform.function = "log",
              regression.aictest = NULL
              )

# #### Remark(s):
# - Length-of-month prior adjustment cannot be specified when td is given in the aictest argument of the regression spec.
# 
# 
# ### Example 2
# 
#     series { data = (6 79 98 42 4 73 85 26 ...)
#               start = 1997.1  period=4 }
#       transform { constant=45  function = auto  }
# 
# #### R-code:
#   
    m <- seas(AirPassengers, 
              transform.constant = 45
    )
    
#     
# 
# #### Remark(s):
# - `transform.function = "auto"` is the default setting in `seas`.
# 
# 
# ### Example 3
# 
#     series {title = "Total U.S. Retail Sales --- Current Dollars"
#                file = "retail.dat"
#                start = 1980.jan }
#        transform {function = log
#                   title = "Consumer Price Index"
#                   start = 1970.jan  # adj. factors start January, 1970
#                   file = "cpi.dat"
#                   format  =  "(12f6.3)" }
#                   
# #### R-code:
#   
#     # adjustment series
    tf <- ts(runif(250), start = c(1945, 1), frequency = 12)
    
    m <- seas(AirPassengers, 
              xtrans = tf,
              transform.function = "log"
              )

# ### Example 4
# 
#     series {title = "Total U.S. Retail Sales --- Current Dollars"
#                file = "retail.dat"
#                start = 1980.jan }
#        transform {function = log
#                   title = "Consumer Price Index"
#                   start = 1970.jan  # adj. factors start January, 1970
#                   file = "cpi.dat"
#                   format  =  "1R"
#                   precision = 3
#                   name = "cpi"
#                   type = temporary
#     }
#                   
# #### R-code:
#   
#   
#     # adjustment series
    tf <- ts(runif(250), start = c(1945, 1), frequency = 12)
    
    m <- seas(AirPassengers, 
              xtrans = tf,
              transform.type = "temporary",
              transform.function = "log"
              )

# ### Example 5
# 
#      SERIES {TITLE="Annual Rainfall"
#                FILE="RAIN.DAT"
#     PERIOD=4
#                START=1901.1}
#        TRANSFORM {POWER=.3333}
#                   
# #### R-code:
#   
    m <- seas(AirPassengers, 
              transform.function = "none",
              transform.power = 0.3333
    )
    
# 
# ### Example 6
# 
#      series {title = "Retail Sales of computers --- Current Dollars"
#                file = "rscomp.dat"     start = 1980.jan
#        }
#        transform { function = log
#                title = "Consumer Price Index & Strike Effect"
#                type = (permanent temporary)
#                start = 1970.jan  # adj. factors start January, 1970
#                file = ("cpi.dat" "strike.dat")
#                format  = "1R"     precision = 3
#                name = ("cpi" "strike")
#     }
#                   
# #### R-code:
#   
#   
#     # temporary and permanent adjustment
    cpi <- ts(runif(250), start = c(1945, 1), frequency = 12)
    strike <- ts(runif(250), start = c(1945, 1), frequency = 12)
    
    m <- seas(AirPassengers, 
              xtrans = cbind(cpi, strike),
              transform.type = c("temporary", "permanent"),
              transform.function = "log"
              )

# ### Example 7
# 
#      series {title = "Total U.K. Retail Sales"
#             file = "ukretail.dat"
#             start = 1978.jan
#             }
#      transform {function = auto
#             aicdiff = 0.0
#             }
#                   
# #### R-code:
#   
    m <- seas(AirPassengers, 
              transform.aicdiff = 0.0
    )
#     
#     
# #### Remark(s):
# - `transform.function = "auto"` is the default setting in `seas`.
# 
# 
# 
# 
# ## 7.19 X11
# 
# 
# ### Example 1
# 
#     Series { File="klaatu.dat" Start = 1976.1 }
#     X11 {  }
# 
# #### R-code:

    seas(AirPassengers, 
         x11 = list()
    )    
# 
# #### Remark(s):
# - If the `x11` spec is specified (here, as an empty `list`), the default `seats` spec is automatically disabled. 
# 
# ### Example 2
# 
#     X11 { SeasonalMA =  s3x9  TrendMA = 23  }
#     X11regression { variables = td   aictest=td  }
# 
# #### R-code:
#     
    seas(AirPassengers, 
         regression.aictest = NULL,
         x11.seasonalma = "s3x9", 
         x11.trendma = 23,
         x11regression.variables = "td",
         x11regression.aictest = "td"
    )
   
# #### Remark(s):
# - Irregular component regression and regARIMA model-based trading day adjustment cannot be specified in the same run.
# 
# 
# 
# ### Example 3
# 
#     series {
#       file="qhstarts.dat"
#       start = 1967.1
#       period=4 }
#     x11    {
#       seasonalma = (s3x3 s3x3 s3x5 s3x5)
#       trendma = 7
#     }
# 
# #### R-code:
#     
#     # we have monthly data
    seas(AirPassengers, 
         x11.seasonalma = c("s3x3", "s3x3", "s3x3", "s3x3", "s3x3", "s3x3",
         "s3x3", "s3x3", "s3x3", "s3x3", "s3x5", "s3x5"),
         x11.trendma = 7
    )
# 
# ### Example 4
# 
#     SERIES { TITLE = "EXPORTS OF LEATHER GOODS"  START = 1969.JUL
#              DATA = (815 866 926 ... 942)  }
#     REGRESSION { VARIABLES = (CONST TD LS1972.MAY LS1976.OCT)  }
#     ARIMA {  MODEL=(0 1 2)(1 1 0)   }
#     ESTIMATE {  }
#     FORECAST {  MAXLEAD=0   }
#     X11 {  MODE = ADD  PRINT = ALLTABLES  SIGMALIM = (2.0 3.5)  }
# 
# #### R-code:    
    
    seas(AirPassengers, 
         transform.function = "none", 
         regression.variables = c("const", "td", "ls1960.may", "ls1960.oct"),
         arima.model = "(0 1 2)(1 1 0)",
         forecast.maxlead = 0,
         x11.mode = "add",
         x11.sigmalim = c(2.0, 3.5)
    )
#   
# #### Remark(s):
# - Automatic transformation has to be turned off.
#     
# 
# 
# ### Example 5
#     
#     series {  title = "Unit Auto Sales"  file = "autosal.dat"
#               start = 1985.1  }
#     transform {  function = log  }
#     regression  { variables = (const td)~~~user = (sale88 sale90)
#                   file = "special.dat" format = "(2f12.2)" } arima {model=(310)(011)12 }
#     forecast  { maxlead=12    maxback=12  }
#     x11  {  title = ( "Unit Auto Sales"}
# 
# #### R-code:    
#       
    seas(AirPassengers, 
         transform.function = "none", 
         regression.variables = c("const", "td", "ls1960.may", "ls1960.oct"),
         arima.model = "(0 1 2)(1 1 0)",
         forecast.maxlead = 0,
         x11.mode = "add",
         x11.sigmalim = c(2.0, 3.5)
    )
# 
# 
# ### Example 6
# 
#     series { title="NORTHEAST ONE FAMILY Housing Starts"
#              file="cne1hs.ori"   name="CNE1HS"    format="2R" }
#     transform {    function=log   }
#     regression {
#       variables=(ao1976.feb ao1978.feb ls1980.feb
#                  ls1982.nov ao1984.feb)
#     }
#     arima {  model=(0 1 2)(0 1 1)  }
#     forecast {  maxlead=60  }
#     x11 {  seasonalma=(s3x9)
#            title="Adjustment of 1 family housing starts"
#            save = e2
#     }
# 
# #### R-code:    
#       
    seas(AirPassengers, 
         transform.function = "log", 
         regression.variables = c("ao1956.feb", "ao1958.feb", "ls1960.feb",
                                  "ls1952.nov", "ao1954.feb"),
         arima.model = "(0 1 2)(0 1 1)",
         forecast.maxlead = 60,
         x11.seasonalma = "s3x9"
    )
    
# 
# 
# ## 7.20 X11REGRESSION
# 
# ### Example 1
# 
#     Series { File = "westus.dat"
#              Start = 1976.1
#     } X11 { }
#     X11Regression { Variables = td
#     }
# 
# #### R-code:
# 
         seas(AirPassengers,
              regression.aictest = NULL,
              x11 = list(),
              x11regression.variables = "td"
         )

# #### Remark(s):
# - `x11regression` regressors are not shown in `summary`. Use `out(m, search = " Regression Model")` to inspect the regression output.
#     
# 
# 
# ### Example 2
#     
#     Series { File = "westus.dat"
#              Start = 1976.1
#     } X11 { }
#     X11Regression { Variables = td
#                     Aictest = (td easter)
#     }
# 
# #### R-code:
# 
    seas(AirPassengers,
         regression.aictest = NULL,
         x11 = list(),
         x11regression.variables = "td",
         x11regression.aictest = c("td", "easter")
    )
#     
# #### Remark(s):
# - `x11regression` regressors are not shown in `summary`. Use `out(m, search = " Regression Model")` to inspect the regression output.
#     
# 
# ### Example 3
#     
#     series {
#         file = "ukclothes.dat"
#         start = 1985.Jan
#      }
#      x11 {  }
#      x11regression{
#         variables = td
#         outler = 4.0
#         user = (easter1 easter2)  file = "ukeaster.dat"
#         usertype = holiday        start = 1980.Jan
#     }
# 
# 
# #### R-code:
# 
         data(holiday)
         easter1 <- genhol(easter, start = -10, end = -1, frequency = 12)
         easter2 <- genhol(easter, start = 0, end = 5, frequency = 12)
         seas(AirPassengers, 
              regression = NULL,
              x11 = list(),
              xreg = cbind(easter1, easter2),  
              x11regression.aictest = "td",
              x11regression.usertype = "holiday",
              outlier = NULL
         )


#  
# #### Remark(s):
# - `x11regression` regressors are not shown in `summary`. Use `out(m, search = " Regression Model")` to inspect the regression output.
# 
# ### Example 4
#     
#     series {
#         file = "nzstarts.dat"  start = 1980.Jan
#      }
#      x11 {  }
#      x11regression{
#         variables = td
#         tdprior = (1.4 1.4 1.4 1.4 1.4 0.0 0.0)
#     }
# 
# 
# #### R-code:
# 
# 
         seas(AirPassengers, 
              regression.aictest = NULL,
              x11regression.variables = "td",
              x11regression.tdprior = c(1.4, 1.4, 1.4, 1.4, 1.4, 0.0, 0.0),
              transform.function = "log"
         )

# #### Remark(s):
# - `x11regression` regressors are not shown in `summary`. Use `out(m, search = " Regression Model")` to inspect the regression output.
#     
#   
# ### Example 5
#     
#     series{
#     format = ’2R’
#     title = ’MIDWEST ONE FAMILY Housing Starts’
#     name = ’CMW1HS’
#     file = ’cmw1hs.ori’
#     span = (1964.01,1989.03)
#     }
#     x11{   }
#     x11regression{
#     variables = (td easter[8])
#     b = (  0.4453f  0.8550f -0.3012f  0.2717f
#           -0.1705f  0.0983f -0.0082)
#         }
# 
# 
# #### R-code:
# 
    seas(AirPassengers, 
         x11 = list(),
         regression.aictest = NULL,
         x11regression.variables = c("td", "easter[8]"),
         x11regression.critical = 5, 
         x11regression.b = c("0.4453f", "0.8550f", "-0.3012f", "0.2717f", 
                             "-0.1705f", "0.0983f", "-0.0082")
         )
         
# 
# #### Remark(s):
# - `x11regression` regressors are not shown in `summary`. Use `out(m, search = " Regression Model")` to inspect the regression output.
# 
# 
# 
# ### Example 6
#     
#     series{
#         title = ’Motor Home Sales’
#         start = 1967.1
#         span = (1972.1, )
#         name = ’SB0562’
#         file = ’C:\final.x12\T0B05601.TXT’
#         format = ’2L’
#        }
#     X11REGRESSION {  variables = ( td/1990.1/
#        easter[8]  labor[10] thank[10] ) }
#     x11{
#         seasonalma = x11default
#         sigmalim = (1.8 2.8)
#         appendfcst = YES
#         save = (D11 D16)
#     }
# 
# #### R-code:
# 
         seas(AirPassengers, 
              regression.aictest = NULL,
              x11regression.variables = c("td/1950.1/", "easter[8]", 
                                          "labor[10]", "thank[10]"),
              x11.seasonalma = "x11default",
              x11.sigmalim = c(1.8, 2.9),
              x11.appendfcst = "yes",
         )
         
# 
# #### Remark(s):
# - `x11regression` regressors are not shown in `summary`. Use `out(m, search = " Regression Model")` to inspect the regression output.
# 
# 
# ### Example 7
#     
#     series{ title = "Automobile Sales"
#             file = "carsales.dat"
#             start = 1975.Jan }
#     transform{ function = log }
#     regression{ variables = (const)
#                 user = (strike80 strike85 strike90)
#                 file = "strike.dat"
#                 format = "(3f12.0)" }
#     arima{ model = (0 1 1)(0 1 1)12 }
#     x11{  title = ("Car Sales in US"
#                    "Adjust for strikes in 80, 85, 90")
#           save = seasonal appendfcst = yes
#           }
#     x11regression{   variables = ( td easter[8] )  }
# 
# 
# #### R-code:
# 

    seas(AirPassengers,
         x11 = list(),
         transform.function = "log",
         regression.variables = "const",
         regression.aictest = NULL,
         arima.model = "(0 1 1)(0 1 1)",
         outlier = NULL,
         x11regression.variables = c("td", "easter[8]")
    )
  
# #### Remark(s):
# - For an unknown X-13-ARIMA SEATS related reason, user defined regressors do not work well with both `x11regression` and `regression` specified.
# - `x11regression` regressors are not shown in `summary`. Use `out(m, search = " Regression Model")` to inspect the regression output.
# 
# 
# 
# 
#     
# [manual]: http://www.census.gov/ts/x13as/docX13AS.pdf "Reference Manual"
