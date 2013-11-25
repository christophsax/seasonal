**seasonal**: R interface to X-13ARIMA-SEATS
--------------------------------------------

**seasonal** is an easy-to-use R-interface to X-13ARIMA-SEATS, a seasonal adjustment software produced, distributed, and maintained by the [United States Census Bureau][census]. X-13ARIMA-SEATS combines and extends the capabilities of the older X-12ARIMA (developed by the Census Bureau) and the TRAMO-SEATS (developed by the Bank of Spain) software packages. 

If you are new to seasonal adjustment or X-13ARIMA-SEATS, you may use the automated procedures to quickly produce seasonal adjustments of some time series. Start with the [Getting started](#getting-started) section and skip the rest. 

If you are already familiar with X-13ARIMA-SEATS, you may benefit from the equivalent use of its syntax in **seasonal**. Read the [Syntax equivalence](#syntax-equivalence-to-x-13arima-seats) section and have a look at the [wiki][examples], where most examples from the original X-13ARIMA-SEATS manual are reproduced in R. For more details on X-13ARIMA-SEATS, as well as for explanations on the X-13ARIMA-SEATS syntax, see the [manual][manual] or the [quick reference][qref].


### Installation

To install directly from github, use the devtools package:

    require(devtools)
    install_github('seasonal', 'christophsax')
    
**seasonal** does not includes the binary executables of X-13ARIMA-SEATS. They need to be installed separately from [here][census_win] (Windows) or [here][census_linux]  (Linux). Make sure you use the standard and not the html verision. My own compilation for Mac OS-X can be obtained up on [request](mailto:christoph.sax@gmail.com).

As a first step, you need to tell **seasonal** where to find the binary executables of X-13ARIMA-SEATS, by setting the specific environmental variable `X13_PATH`. This may be done temporarily in R:

    Sys.setenv(X13_PATH = "YOUR_PATH_TO_X13" )
    
Exchange `YOUR_PATH_TO_X13` with the path to your installation of X-13ARIMA-SEATS. Note that on Windows the path `C:\something\somemore` hat to be entered UNIX-like `C:/something/somemore` or `C:\\something\\somemore`.

If you want to set the environmental variable permanently, you may do so by adding it tho the `.Renviron` file, which is typically located in your home directory (see ?Startup to learn about environmental variables in the start-up process). Both on Windows and UNIX systems, you may add the following line to  `.Renviron` using your favorite editor:

    X13_PATH = YOUR_PATH_TO_X13

of simply add the line via terminal:

    echo 'X13_PATH = YOUR_PATH_TO_X13' >> .Renviron


### Getting started

`seas` ist the core function of the **seasonal** package. By default, `seas` calls the automatic procedures of X-13ARIMA-SEATS to perform a seasonal adjustment that works very well in most circumstances. It returns an object of class `"seas"` that contains all necessary information on the adjustment process, as well as the series. The `final` method for `seas` objects returns the adjusted series, the `plot` method shows a plot with the unadjusted and the adjusted series. 

    x <- seas(AirPassengers)
    final(x)
    plot(x)
     
The first argument must be a time series of class `"ts"`. By default, `seas` calls the SEATS adjustment procedure. If you prefer the X11 adjustment procedure, use the following option (see the next section for details on the syntax):

    seas(AirPassengers, x11 = list())
     
Besides performing seasonal adjustment with SEATS, a default call of `seas` invokes the following automatic procedures of X-13ARIMA-SEATS:

  - Transformation selection (log / no log)
  - Detection of trading day and Easter effects
  - Outlier detection
  - ARIMA model search

Alternatively, all inputs may be entered manually, as in the following example:

    seas(x = AirPassengers, regression.variables = c("td1coef", "easter[1]",
    "ao1951.May"), arima.model = "(0 1 1)(0 1 1)", regression.aictest = NULL,
    outlier = NULL, transform.function = "log")

The `static` command reveals the static call from above that is needed to replicate an automatic seasonal adjustment procedure:

    static(x)
    static(x, coef = TRUE)  # also fixes the coefficients
    
If you are using R Studio, the `inspect` command offers a way to analyze and modify a seasonal adjustment procedure (see the section below for details):

    inspect(AirPassengers)


### Syntax equivalence to X-13ARIMA-SEATS

The X-13ARIMA-SEATS syntax uses *specs* and *arguments*, while each spec may contain some arguments. For details, see the [manual][manual]. These spec-argument combinations can be added to `seas` by separating spec and argument by a dot (`.`). For example, in order to set the `variables` argument of the `regression` spec equal to `td` and `ao1999.jan`, the input to `seas` looks like this:

    x <- seas(AirPassengers, regression.variables = c("td", "ao1965.jan"))
   
Note that R vectors may be used as an input. If a spec is added without any arguments, the spec should be set equal to an empty `list()`. Several defaults of `seas` are such empty lists, like the default `seats = list()`. See the help page (`?seas`) for more details on the defaults.

It is possible to manipulate almost all inputs to X-13ARIMA-SEATS this way. For instance, example 1 in section 7.1 from the [manual][manual],

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
    
`seas` takes care of the `series` spec, so no input beside the time series has to be provided. As `seas` uses the SEATS procedure by default, the use of X11 has to be specified manually. When the `x11` spec is added as an input (as above), the mutually exclusive and default `seats` spec is automatically disabled. With `arima.model`, an additional spec-argument entry is added to the input of X-13ARIMA-SEATS. As the spec cannot be used with the default `automdl` spec, the latter is automatically disabled. The best way to learn about the relationship between the syntax of X-13ARIMA-SEATS and seasonal is to study the growing list of examples in the [wiki][examples].


There are several mutually exclusive specs in X-13ARIMA-SEATS. If more than one mutually exclusive specs are included, X-13ARIMA-SEATS leads to an error. In contrast, `seas` follows a set of priority rules, where the lower priority is overwritten by the higher priority. Usually, the default has the lowest priority, and is overwritten if one or several of the following `spec` inputs are provided:

- Model selection
    1. `arima`
    2. `pickmdl`
    3. `automdl` (default)

- Adjustment procedure
    1. `x11`
    2. `seats` (default)
  
- Regression procedure
    1. `x11regression`
    2. `regression` (default)
  

### Graphs

All plots from Win X-13, a graphical Windows version of X-13ARIMA-SEATS, should be reproducible in R. The main plot function draws the seasonally adjusted and unadjusted series, as well as the outliers. Optionally, it also draws the trend of the seasonal decomposition.

    x <- seas(AirPassengers, regression.aictest = c("td", "easter"))
    plot(x)
    plot(x, outliers = FALSE)
    plot(x, trend = TRUE)

The `monthplot` function allows for a monthwise (or quarterwise) plot of the data. There is a method for `seas` objects:

    monthplot(x)
    monthplot(x, choice = "irregular")

With `spectrum`, the spectral density of a series can be estimated:

    spectrum(diff(final(x)))
    spectrum(diff(original(x)))


### Inspect tool

the `inspect` function is a powerful tool for choosing a good seasonal adjustment model. It uses the `manipulate` package and can only be used with the (free) [RStudio IDE][rstudio]. `inspect` uses a `"ts"` object as its first argument:

    inspect(AirPassengers)
    
Optionally, you can pass arbitrary spec-arguments to inspect. Here, the maximum of iterations during estimation is increased from 500 to 1000:

    inspect(AirPassengers, estimate.maxiter = 1000) 
    
The inspect function opens an interactive window that allows for the manipulation of a number of arguments. It offers several views to analyze the series graphically. With each change, the adjustment process and the visualizations are recalculated. Summary statics are shown in the R console. With the 'Show static call' option, a replicable static call is also shown in the console. Note that the last option will double the time for recalculation, as the static function tests the static call each time.


### License

**seasonal** is free and open source, licensed under GPL-3. The binary packages for X-13ARIMA-SEATS from the United States Census Bureau and are in the public domain. Both may be freely copied and transferred. 

This is a very new package, and it may still contain bugs. Please report them on github or send me an [e-mail](mailto:christoph.sax@gmail.com). Thank you!

[manual]: http://www.census.gov/ts/x13as/docX13AS.pdf "Reference Manual"

[qref]: http://www.census.gov/ts/x13as/pc/qrefX13ASpc.pdf "Quick Reference"

[census]: http://www.census.gov/ts/x13as "United States Census Bureau"

[census_win]: http://www.census.gov/srd/www/x13as/x13down_pc.html "Combined X-13ARIMA-SEATS archives"

[census_linux]: http://www.census.gov/srd/www/x13as/x13down_unix.html "Combined X-13ARIMA-SEATS archives"

[examples]: https://github.com/christophsax/seasonal/wiki/Examples-of-X-13ARIMA-SEATS-in-R "Wiki: Examples of X-13ARIMA-SEATS in R"

[rstudio]: http://www.rstudio.com/ide/





