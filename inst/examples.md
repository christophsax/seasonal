This page collects the R translations of the examples in the X-13ARIMA-SEATS [manual][manual]. It is not yet exhaustive, but will grow over time. If you have reproduced an example, you are invited to add it to the wiki.

### 7.1 ARIMA

#### Example 1

    series { title  =  "Quarterly Grape Harvest" start = 1950.1
             period =  4
             data  = (8997 9401 ... 11346) }
    arima { model = (0 1 1) }
    estimate { }

R-code:

    seas(AirPassengers,
         x11 = list(),
         arima.model = "(0 1 1)"
    )
    
If the `x11` spec is specified (here, as an empty `list`), the default `seats` spec is automatically removed. 

#### Example 2

    series { title = "Monthly sales" start = 1976.jan
             data = (138 128 ... 297) }
    transform { function = log } arima{model =(210)(011)} estimate { }

R-code:

    seas(AirPassengers,
         x11 = list(),
         transform.function = "log",
         arima.model = "(2 1 0)(0 1 1)"
    )


#### Example 3

    Series { Title = "Monthly Sales"  Start = 1976.jan
             Data = (138 128 ... 297)  }
    Transform { Function = log }
    Regression { Variables= (seasonal const) } Arima {Model=(011)}
    Estimate { }

R-code:

    seas(AirPassengers,
         x11 = list(),
         transform.function = "log",
         regression.variables = c("season", "const"),
         arima.model = "(0 1 1)"
    )


#### Example 4

    series{title = "Annual Olive Harvest" start = 1950
            data = (251 271 ... 240)  }
    arima{model  = ([2] 1 0)}
    estimate{ }

R-code:

    seas(AirPassengers,
         x11 = list(),
         transform.function = "log",
         regression.variables = c("season", "const"),
         arima.model = "([2] 1 0)"
    )

#### Example 5

    series { title = "Monthly sales"  start = 1976.jan
             data = (138 128 ... 297) }
    transform { function = log }
    regression { variables = const }
    arima { model  = (0 1 1)12 }
    estimate { }

R-code:

    seas(AirPassengers,
         x11 = list(),
         transform.function = "log",
         regression.variables = c("variables", "const"),
         arima.model = "(0 1 1)12"
    )

#### Example 6

    series { title = "Monthly sales"  start = 1976.jan
              data = (138 128 ... 297) }
    transform { function = log }
    regression { variables = (const seasonal)} arima{model =(110)(100)3(001)} estimate { }

R-code:

    seas(AirPassengers,
         x11 = list(),
         transform.function = "log",
         regression.variables = c("const", "seasonal"),
         arima.model = "(110)(100)3(001)"
    )

#### Example 7

    series { title = "Monthly sales"  start = 1976.jan
             data = (138 128 ...  297) }
    transform{ function = log }
    arima  { model = (0 1 1)(0 1 1)12
                ma = ( ,1.0f)}
    estimate { }

R-code:

    seas(AirPassengers,
         x11 = list(),
         transform.function = "log",
         arima.model = "(0 1 1)(0 1 1)12",
         arima.ma = "( ,1.0f)"
    )

### 7.2 AUTOMDL

#### Example 1

    series      { title = "Monthly sales"   start = 1976.jan
                file="ussales.dat"  }
    regression { variables = (td seasonal) } automdl { }
    estimate { }
    x11 {}

R code:
    seas(AirPassengers, 
         x11 = list()
         regression.variables = c("td", "seasonal")
    )
    
If the `x11` spec is specified (here, as an empty `list`), the default `seats` spec is automatically removed. 

#### Example 2

    series      { title = "Monthly sales"
                  file="ussales.dat" }
    regression  { variables = td  }
    start = 1976.jan
    automdl {
    diff=(11)
    maxorder = ( 3, ) }
    outlier     {   }
    estimate { } x11 {}

R code:
    seas(AirPassengers, 
               x11 = list(),
               regression.variables = c("td"),
               automdl.diff = c(1, 1),
               automdl.maxorder = "3, "
    )
    
Because the second element in the `automdl.maxorder` argument is emptly, it has to be entered as a character string instead of a numeric vector.

#### Example 3

    series      { title = "Monthly sales"
                  file="ussales.dat" }
    regression { aictest = td } automdl { savelog = amd } estimate { }
    x11 {}
    
R code:
    seas(AirPassengers, 
               x11 = list(),
               regression.aictest = c("td")
    )
    
No manual modifications of the output should be performed.

### 7.3 CHECK
### 7.4 COMPOSITE
### 7.5 ESTIMATE
### 7.6 FORCE
### 7.7 FORECAST
### 7.8 HISTORY
### 7.9 METADATA
### 7.10 IDENTIFY
### 7.11 OUTLIER
### 7.12 PICKMDL

#### Example 1
    series      { title = "Monthly sales"   start = 1976.jan
                    data = (138 128  ...  297) }
    regression { variables = (td seasonal) }
    pickmdl { mode = fcst file = "nosdiff.mdl" } estimate { }
    x11 {}

R code:
    seas(AirPassengers, 
         x11 = list(),
         pickmdl.mode = "fcst"
    )
    
If the `x11` spec is specified (here, as an empty `list`), the default `seats` spec is automatically removed. If the `pickmdl` spec is specified, the default `automdl` spec is automatically removed. No manual modifications of the output should be performed.

#### Example 2

    series      { title = "Monthly sales"   start = 1976.jan
                    data = (138 128  ...  297) }
    regression  { variables = td }
    pickmdl     { mode = fcst    file = "nosdiff.mdl"
                    method = first   fcstlim = 20   qlim = 10
                    overdiff = 0.99  identify = all }
    outlier     {  }
    estimate { } 
    x11 {}
    
R code:
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
    
#### Example 3
    series      { title = "Monthly sales"   start = 1976.jan
                    data = (138 128  ...  297) }
    regression  { variables = td }
    pickmdl     { mode = fcst    file = "nosdiff.mdl"
                    outofsample=yes  }
    estimate    {   }
    x11 {}
    
R code:
    seas(AirPassengers, 
         x11 = list(),
         regression.variables = "td",
         pickmdl.mode = "fcst",
         pickmdl.outofsample = "yes"
    )

### 7.13 REGRESSION
### 7.14 SEATS
### 7.15 SERIES
### 7.16 SLIDINGSPANS
### 7.17 SPECTRUM
### 7.18 TRANSFORM
### 7.19 X11


#### Example 1

    Series { File="klaatu.dat" Start = 1976.1 }
    X11 {  }

R code:
    seas(AirPassengers, 
         x11 = list()
    )
    
If the `x11` spec is specified (here, as an empty `list`), the default `seats` spec is automatically removed. 

#### Example 2

    X11 { SeasonalMA =  s3x9  TrendMA = 23  }
    X11regression { variables = td   aictest=td  }

R-code:
    
    seas(AirPassengers, 
         x11.seasonalma = "s3x9", 
         x11.trendma = 23,
         x11regression.variables = "td",
         x11regression.aictest = "td"
    )
    
If the `x11regression` spec is specified, the `regression` spec is automatically removed.
    
#### Example 3

    series {
      file="qhstarts.dat"
      start = 1967.1
      period=4 }
    x11    {
      seasonalma = (s3x3 s3x3 s3x5 s3x5)
      trendma = 7
    }

R-code:
      
    seas(AirPassengers, 
         x11.seasonalma = c("s3x3", "s3x3", "s3x5", "s3x5"),
         x11.trendma = 7
    )

#### Example 4

    SERIES { TITLE = "EXPORTS OF LEATHER GOODS"  START = 1969.JUL
             DATA = (815 866 926 ... 942)  }
    REGRESSION { VARIABLES = (CONST TD LS1972.MAY LS1976.OCT)  }
    ARIMA {  MODEL=(0 1 2)(1 1 0)   }
    ESTIMATE {  }
    FORECAST {  MAXLEAD=0   }
    X11 {  MODE = ADD  PRINT = ALLTABLES  SIGMALIM = (2.0 3.5)  }

R code:    
    
    seas(AirPassengers, 
         transform.function = NULL, 
         regression.variables = c("const", "td", "ls1960.may", "ls1960.oct"),
         arima.model = "(0 1 2)(1 1 0)",
         forecast.maxlead = 0,
         x11.mode = "add",
         x11.sigmalim = c(2.0, 3.5)
    )
    
Automatic transformation has to be turned off.
    
#### Example 5
    
    series {  title = "Unit Auto Sales"  file = "autosal.dat"
              start = 1985.1  }
    transform {  function = log  }
    regression  { variables = (const td)~~~user = (sale88 sale90)
                  file = "special.dat" format = "(2f12.2)" } arima {model=(310)(011)12 }
    forecast  { maxlead=12    maxback=12  }
    x11  {  title = ( "Unit Auto Sales"}

R code:    
      
    seas(AirPassengers, 
         transform.function = NULL, 
         regression.variables = c("const", "td", "ls1960.may", "ls1960.oct"),
         arima.model = "(0 1 2)(1 1 0)",
         forecast.maxlead = 0,
         x11.mode = "add",
         x11.sigmalim = c(2.0, 3.5)
    )


#### Example 6

    series { title="NORTHEAST ONE FAMILY Housing Starts"
             file="cne1hs.ori"   name="CNE1HS"    format="2R" }
    transform {    function=log   }
    regression {
      variables=(ao1976.feb ao1978.feb ls1980.feb
                 ls1982.nov ao1984.feb)
    }
    arima {  model=(0 1 2)(0 1 1)  }
    forecast {  maxlead=60  }
    x11 {  seasonalma=(s3x9)
           title="Adjustment of 1 family housing starts"
           save = e2
    }

R code:    
      
    seas(AirPassengers, 
         transform.function = "log", 
         regression.variables = c("ao1956.feb", "ao1958.feb", "ls1960.feb",
                                  "ls1952.nov", "ao1954.feb"),
         arima.model = "(0 1 2)(0 1 1)",
         forecast.maxlead = 60,
         x11.seasonalma = "s3x9"
    )
    

### 7.20 X11REGRESSION

#### Example 1

    Series { File = "westus.dat"
             Start = 1976.1
    } X11 { }
    X11Regression { Variables = td
    }

R-code:

    seas(AirPassengers,
         x11regression.variables = "td"
    )

If the `x11regression` spec is specified, the `regression` spec is automatically removed.
    
#### Example 2
    
    Series { File = "westus.dat"
             Start = 1976.1
    } X11 { }
    X11Regression { Variables = td
                    Aictest = (td easter)
    }
    
R-code:

    seas(AirPassengers,
         regression = NULL,
         x11regression.variables = "td",
         x11regression.aictest = c("td", "easter")
    )
    
#### Example 3
    
Currently, no exogenous variables. 


[manual]: http://www.census.gov/ts/x13as/docX13AS.pdf "Reference Manual"