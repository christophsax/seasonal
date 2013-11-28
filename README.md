**seasonal**: R interface to X-13ARIMA-SEATS
--------------------------------------------

**seasonal** is an easy-to-use R-interface to X-13ARIMA-SEATS, a seasonal adjustment software developed by the [United States Census Bureau][census]. X-13ARIMA-SEATS combines and extends the capabilities of the older X-12ARIMA (developed by the Census Bureau) and the TRAMO-SEATS (developed by the Bank of Spain) software packages. 

If you are new to seasonal adjustment or X-13ARIMA-SEATS, you may use the automated procedures to quickly produce seasonal adjustments of time series. Start with the [Getting started](#getting-started) section and skip the rest. 

If you are already familiar with X-13ARIMA-SEATS, you may benefit from the equivalent use of its syntax in **seasonal**. Read the [Syntax equivalence](#syntax-equivalence-to-x-13arima-seats) section and have a look at the [wiki][examples], where most examples from the original X-13ARIMA-SEATS manual are reproduced in R. For more details on X-13ARIMA-SEATS, as well as for explanations on the X-13ARIMA-SEATS syntax, see the [manual][manual] or the [quick reference][qref].

### Installation

**seasonal** does not includes the binary executables of X-13ARIMA-SEATS. They need to be installed separately from [here][census_win] (Windows) or [here][census_linux]  (Linux). Make sure you download the standard and not the html verision. My own compilation for Mac OS-X can be obtained [upon request](mailto:christoph.sax@gmail.com).

Download the file, unzip it and copy it to the desired location in your file system. Next, you need to tell **seasonal** where to find the binary executables of X-13ARIMA-SEATS, by setting the specific environmental variable `X13_PATH`. This may be done during your active session in R:

    Sys.setenv(X13_PATH = "YOUR_X13_DIRECTORY")
 
Exchange `YOUR_X13_DIRECTORY` with the path to your installation of X-13ARIMA-SEATS. Note that the Windows path `C:\something\somemore` has to be entered UNIX-like `C:/something/somemore` or `C:\\something\\somemore`. You can always check your installation with:

    checkX13()

If you want to set the environmental variable permanently, you may do so by adding it tho the `Renviron.site` file, which is located in the `etc` subdirectory of your R home directory (use `R.home()` in R to reveal the home directory). `Renviron.site` does not exist by default; if not, you have to create a simple text file with the name `Renviron.site`. Add the following line to the file using your favorite text editor:

    X13_PATH = YOUR_PATH_TO_X13

Alternatively, use the terminal/command prompt both on UNIX systems and on Windows (on Windows, the `cd` command requires `\` instead of `/`):

    cd YOUR_R_HOME_DIRECTORY/etc
    echo X13_PATH = YOUR_PATH_TO_X13 >> Renviron.site

There are other ways to set an environmental variable permanently in R, see `?Startup`.

### Getting started

`seas` ist the core function of the **seasonal** package. By default, `seas` calls the automatic procedures of X-13ARIMA-SEATS to perform a seasonal adjustment that works very well in most circumstances. It returns an object of class `"seas"` that contains all necessary information on the adjustment process, as well as the series. The `final` method for `"seas"` objects returns the adjusted series, the `plot` method shows a plot with the unadjusted and the adjusted series. 

    x <- seas(AirPassengers)
    final(x)
    plot(x)
     
The first argument has to be a time series of class `"ts"`. By default, `seas` calls the SEATS adjustment procedure. If you prefer the X11 adjustment procedure, use the following option (see the next section for more details about the syntax):

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

The `static` command reveals the static call from above that is needed to replicate the automatic seasonal adjustment procedure:

    static(x)
    static(x, coef = TRUE)  # also fixes the coefficients
    
If you are using RStudio, the `inspect` command offers a way to analyze and modify a seasonal adjustment procedure (see the section below for details):

    inspect(AirPassengers)


### Syntax equivalence to X-13ARIMA-SEATS

The X-13ARIMA-SEATS syntax uses *specs* and *arguments*, with each spec optionally containing some arguments. For details, see the [manual][manual]. These spec-argument combinations can be added to `seas` by separating spec and argument by a dot (`.`). For example, in order to set the `variables` argument of the `regression` spec equal to `td` and `ao1999.jan`, the input to `seas` looks like this:

    x <- seas(AirPassengers, regression.variables = c("td", "ao1965.jan"))
   
Note that R vectors may be used as an input. If a spec is added without any arguments, the spec should be set equal to an empty `list()`. Several defaults of `seas` are empty lists, such as the default `seats = list()`. See the help page (`?seas`) for more details on the defaults.

It is possible to manipulate almost all inputs to X-13ARIMA-SEATS in this way. For instance, example 1 in section 7.1 from the [manual][manual],

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
    
`seas` takes care of the `series` spec, and no input beside the time series has to be provided. As `seas` uses the SEATS procedure by default, the use of X11 has to be specified manually. When the `x11` spec is added as an input (like above), the mutually exclusive and default `seats` spec is automatically disabled. With `arima.model`, an additional spec-argument entry is added to the input of X-13ARIMA-SEATS. As the spec cannot be used in the same call as the `automdl` spec, the latter is automatically disabled. The best way to learn about the relationship between the syntax of X-13ARIMA-SEATS and seasonal is to study the growing list of examples in the [wiki][examples].


There are several mutually exclusive specs in X-13ARIMA-SEATS. If more than one mutually exclusive specs is included, X-13ARIMA-SEATS would return an error. In contrast, `seas` follows a set of priority rules, where the lower priority is overwritten by the higher priority. Usually, the default has the lowest priority and is overwritten if one or several of the following `spec` inputs are provided:

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

All plots from Win X-13, a graphical Windows version of X-13ARIMA-SEATS, are reproducible in R. The main plot function draws the seasonally adjusted and unadjusted series, as well as the outliers. Optionally, it also draws the trend of the seasonal decomposition:

    x <- seas(AirPassengers, regression.aictest = c("td", "easter"))
    plot(x)
    plot(x, outliers = FALSE)
    plot(x, trend = TRUE)

The `monthplot` function allows for a monthwise plot (or quarterwise, with the identical function name) of the data. There is a method for `"seas"` objects:

    monthplot(x)
    monthplot(x, choice = "irregular")

With `spectrum`, the spectral density of any series can be estimated and plotted:

    spectrum(diff(final(x)))
    spectrum(diff(original(x)))


### Inspect tool

The `inspect` function is a powerful tool for choosing a good seasonal adjustment model. It uses the `manipulate` package and can only be used with the (free) [RStudio IDE][rstudio]. `inspect` uses a `"ts"` object as its first argument:

    inspect(AirPassengers)
    
Optionally, you can pass arbitrary spec-arguments to inspect. Here, the maximum of iterations during estimation is increased from 500 to 1000:

    inspect(AirPassengers, estimate.maxiter = 1000) 
    
The inspect function opens an interactive window that allows for the manipulation of a number of arguments. It offers several views to analyze the series graphically. With each change, the adjustment process and the visualizations are recalculated. Summary statics are shown in the R console. With the 'Show static call' option, a replicable static call is also shown in the console. Note that the last option will double the time for recalculation, as the static function also tests the static call each time (this is a beta feature of seasonal, which allows intensive testing; it may be disabled in more stable versions).


### License

**seasonal** is free and open source, licensed under GPL-3. It has been developed for the use at the Swiss State Secretariat of Economic Affairs and is completely independent of X-13ARIMA-SEATS, which is in the Public Domain.

This is a very new package, and it may still contain bugs. Please report them on Github or send me an [e-mail](mailto:christoph.sax@gmail.com). Thank you!

[manual]: http://www.census.gov/ts/x13as/docX13AS.pdf "Reference Manual"

[qref]: http://www.census.gov/ts/x13as/pc/qrefX13ASpc.pdf "Quick Reference"

[census]: http://www.census.gov/srd/www/x13as/ "United States Census Bureau"

[census_win]: http://www.census.gov/srd/www/x13as/x13down_pc.html "Combined X-13ARIMA-SEATS archives"

[census_linux]: http://www.census.gov/srd/www/x13as/x13down_unix.html "Combined X-13ARIMA-SEATS archives"

[examples]: https://github.com/christophsax/seasonal/wiki/Examples-of-X-13ARIMA-SEATS-in-R "Wiki: Examples of X-13ARIMA-SEATS in R"

[rstudio]: http://www.rstudio.com/ide/





