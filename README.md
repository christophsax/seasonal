seasonal: R interface to X-13ARIMA-SEATS
----------------------------------------

**seasonal** is an easy-to-use R-interface to **X-13ARIMA-SEATS**, a seasonal adjustment software **produced, distributed, and maintained by the United States Census Bureau**. X-13ARIMA-SEATS combines and extends the capabilities of the older X-12ARIMA (developed by the Census Bureau) and the TRAMO-SEATS (developed by the Bank of Spain) software packages. 

If you are new to seasonal adjustment and X-13ARIMA-SEATS, you may use the automated procedures to quickly produce seasonal adjustments of some time series. The default settings in the core function generally do a very good job. Start with the *installation* and *getting started* section and skip the rest. 

If you are familiar with seasonal adjustment and already know something about X-13ARIMA-SEATS, you may benefit from the close relationship between the syntax in seasonal and X-13ARIMA-SEATS. Study the *X-13ARIMA-SEATS syntax* section and have a look at the [wiki][examples], where most examples from the original X-13ARIMA-SEATS manual are reproduced in R. For more details on X-13ARIMA-SEATS, as well as for explanations on the X-13ARIMA-SEATS syntax, see the [manual][manual] or the [quick reference][qref].


### Installation

To install directly from github, use the devtools package:

    require(devtools)
    install_github('seasonal', 'christophsax')
    
seasonal currently includes the binary executables of X-13ARIMA-SEATS for **Windows** (official compilation from the Census Bureau) and **Mac OS-X** (my own compilation). Both should work without additional installations. The Linux version will be included in the future.


### Getting started

`seas` ist the core function of the seasonal package. By default, `seas` calls the automatic procedures of X-13ARIMA-SEATS to perform a seasonal adjustment that works very well in most circumstances. It returns an object of class `seas` that contains all necessary information on the adjustment process, as well as the series. The `final` method for `seas` objects returns the adjusted series, the `plot` method shows a plot with the unadjusted and the adjusted series. 

     x <- seas(AirPassengers)
     final(x)
     plot(x)
     
The first argument must be a time series of class `ts`. By default, `seas` calls the SEATS adjustment procedure. If you prefer the X11 adjustment procedure, use the following option (see the next section for details on the syntax):

     seas(AirPassengers, x11 = list())
     
Besides performing seasonal adjustment with SEATS, a default call of `seas` invokes the following automatic procedures of X-13ARIMA-SEATS:
  - transformation section (log, no log)
  - Detection of trading day and Easter effects
  - Outlier detection
  - ARIMA model search

Alternatively, all inputs may be entered manually, as in the following example:

    seas(x = AirPassengers, arima.model = "(0 1 1)(0 1 1)", regression.chi2test = "no", 
        outlier.types = "none", transform.function = "log")

The `static` command reveals the static call from above that is needed to replicate an automatic seasonal adjustment procedure:

    static(x)
    static(x, coef = TRUE)  # also fixes the coefficients
    
If you are using R Studio, the `inspect` command offers a way to analyze and modify a seasonal adjustment procedure (see the section below for details):

    inspect(x)


### X-13ARIMA-SEATS syntax

Seasonal uses the same syntax as X-13ARIMA-SEATS. It is possible to invoke most options that are available in X-13ARIMA-SEATS. For details on the options, see the [manual][manual]. The X-13ARIMA-SEATS syntax uses *specs* and *arguments*, while each spec may contain some arguments. **An additional spec/argument can be added to the `seas` function by separating spec and argument by a dot (`.`).** For example, in order to set the `variable` argument of the `regression` spec equal to `td` and `ao1999.jan`, the input to `seas` looks like this:

    x <- seas(AirPassengers, regression.variable = c("td", "ao1965.jan"))
   
Note that R vectors may be used as an input. If a `spec` is added without any arguments, the `spec` should be set equal to an empty `list()`. Several defaults of `seas` are such empty lists, like the default `seats = list()`. See the help page (`?seas`) for more details on the defaults.

It is possible to manipulate almost all inputs to X-13ARIMA-SEATS this way. Most examples in the [manual][manual] are replicable in R. For instance, example 1 in section 7.1,

    series { title  =  "Quarterly Grape Harvest" start = 1950.1
           period =  4
           data  = (8997 9401 ... 11346) }
    arima { model = (0 1 1) }
    estimate { }

translates to R in the following way:

    seas(AirPassengers,
         x11 = list(),
         arima.model = "(0 1 1)"
    )
    
`seas` takes care of the `series` spec, so no input beside the time series has to be provided. As `seas` uses the SEATS procedure by default, the use of X11 has to be specified manually. When the `x11` spec is added as an input (as above), the mutually exclusive and default `seats` spec is automatically disabled. With `arima.model`, an additional spec/argument entry is added to the input of X-13ARIMA-SEATS. As the spec cannot be used with the default `automdl` spec, the latter is automatically disabled. The best way to learn about the relationship between the syntax of X-13ARIMA-SEATS and seasonal is to study the growing list of examples in the [wiki][examples].


#### Priority rules

There are several mutually exclusive specs in X-13ARIMA-SEATS. If more than one mutually exclusive specs are included, X-13ARIMA-SEATS leads to an error. In contrast, `seas` follows a set of priority rules, where a lower priority is overwritten by a higher priority. Usually, the default has the lowest priority, and is overwritten if one or several of the following `spec` inputs are provided:

Model selection
  1. `arima`
  2. `pickmdl`
  3. `automdl` (default)

Adjustment procedure
  1. `x11`
  2. `seats` (default)
  
Regression procedure
  1. `x11regression`
  2. `regression` (default)
  

### Graphs

All plots from Win X-13 should be reproducible in R. The main plot function draws the seasonally adjusted and unadjusted series, as well as the outliers. Optionally, it also draws the trend of the seasonal decomposition.

    x <- seas(AirPassengers, regression.aictest = c("td", "easter"))
    plot(x)
    plot(x, outliers = FALSE)
    plot(x, trend = TRUE)

The `monthplot` function allows for a monthwise (or quarterwise) plot of the data. There is a method for `seas` objects:

    monthplot(x)
    monthplot(x, choice = "irregular")

With `spectrum`, the spectral density of a series can be estimated:

    spectrum(final(x))
    spectrum(original(x))


### Inspect tool

the `inspect` function is a powerful tool for choosing a good seasonal adjustment model. It uses the `manipulate` package and can only be used with the (free) [RStudio IDE][rstudio]. The function uses a `ts` object as its first argument:

    inspect(x)
    
Optionally, you can pass arbitrary spec/arguments to inspect. Here, the maximum of iterations during estimation is increased from 500 to 1000:

    inspect(AirPassengers, estimate.maxiter = 1000) 
    
The inspect function opens an interactive window that allows for the manipulation of a number of arguments. It offers several views to analyze the series graphically. With each change, the adjustment process and the visualizations are recalculated. Summary statics are shown in the R console. With the 'Show static call' option, a replicable static call is also shown in the console. Note that the last option will double the time for recalculation, as the static function tests the static call each time.


### License

seasonal is free and open source, licensed under GPL-3. The package contains the X-13ARIMA-SEATS binary files from the United States Census Bureau, which are in the public domain. According to the [manual][manual] (page 1):

> When it is released, the X-13ARIMA-SEATS program will be in the public domain, and may be copied or transferred.

This is a very new package, and it may still contain bugs. Please report them on github or send me an [e-mail](mailto:christoph.sax@gmail.com). Thank you!

[manual]: http://www.census.gov/ts/x13as/docX13AS.pdf "Reference Manual"

[qref]: http://www.census.gov/ts/x13as/pc/qrefX13ASpc.pdf "Quick Reference"

[examples]: https://github.com/christophsax/seasonal/wiki/Examples-of-X-13ARIMA-SEATS-in-R "Wiki: Examples of X-13ARIMA-SEATS in R"

[rstudio]: http://www.rstudio.com/ide/





