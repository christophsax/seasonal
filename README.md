seasonal: R interface to X-13ARIMA-SEATS
----------------------------------------

**seasonal** is an easy-to-use R-interface to **X-13ARIMA-SEATS**, a seasonal adjustment software **produced, distributed, and maintained by the United States Census Bureau**. X-13ARIMA-SEATS combines and extends the capabilities of the older X-12ARIMA (developed by the Census Bureau) and the TRAMO-SEATS (developed by the Bank of Spain) software packages. 

If you are new to seasonal adjusmtent and X-13ARIMA-SEATS, you may use the automated procedures to quickly produce seasonal adjustements of some time series. The default settings in the core function generally do a very good job. Start with the *installation* and *getting started* section and skip the rest. 

If you are familiar with seasonal adjusmtent and already know something about X-13ARIMA-SEATS, you may benefit from the very close relationship between the syntax in seasonal and X-13ARIMA-SEATS. Study the *X-13ARIMA-SEATS syntax* section and have a look at the [wiki][examples], where most examples from the original X-13ARIMA-SEATS manual are reproduced in R. For more details on X-13ARIMA-SEATS, as well as for explanations on the X-13ARIMA-SEATS syntax, see the [manual][manual] or the [quick reference][qref].


### Installation

To install directly from github to R, substitute your github `'USERNAME'` and `'PASSWORD'`:

    require(devtools)
    install_github('seasonal', 'christophsax', auth_user = 'USERNAME', password = 'PASSWORD')
    
seasonal includes the binary files of X-13ARIMA-SEATS. **No separate download of the binaries is needed.**


### Getting started

`seas` ist the core function of the seasonal package. By default, `seas` calls the automatic procedures of X-13ARIMA-SEATS to perform a seasonal adjustment that works very well in most circumstances. It returns an object of class `seas` that contains all necessary information on the adjustment process, as well as the series. The `predict` method for `seas` objects returns the adjusted series, the `plot` method shows a plot with the unadjusted and the adjusted series. 

     x <- seas(AirPassengers)
     predict(x)
     plot(x)
     
The first argument must be a time series of class `ts`. By default, `seas` calls the SEATS adjustment procedure. If you prefer the X11 adjustment filter, use the following option (see the next section for details on the syntax):

     seas(AirPassengers, x11 = list())
     
Besides performing seasonal adjustment with SEATS, a default call of `seas` invokes the following automatic procedures of X-13ARIMA-SEATS:
  - ARIMA model search
  - Outlier detection
  - Detection of trading day and Easter effects

Alternatively, all inputs may be entered manually, as in the following example:

    seas(AirPassengers,
         regression.variables = c("td1coef", "easter[1]", "ao1951.May"),
         arima.model = "(0 1 1)(0 1 1)",
         regression.aictest = NULL, outlier.types = "none"
    )

The `static` command reveals the static call from above that is needed to replicate an automatic seasonal adjustment procedure:

    static(x)
    static(x, static.coeff = TRUE)  # also fixes the coefficients
    
If you are using R Studio, the `inspect` command offers a way to analyze and modify a seasonal adjustment procedure (see the section below for details):

    inspect(x)


### X-13ARIMA-SEATS syntax


Seasonal uses the same syntax as X-13ARIMA-SEATS. It is possible to invoke most options that are available in X-13ARIMA-SEATS. For details on the options, see the [manual][manual]. The X-13ARIMA-SEATS syntax uses *specs* and *arguments*, while each spec may contain some arguments. **An additional spec/argument can be added to the `seas` function by separating spec and argument by a `.`.** For example, in order to set the `variable` argument of the `regression` spec equal to `td` and `ao1999.jan`, the input to `seas` looks like this:

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
    
`seas` takes care of the `series` spec, so no input beside the time series has to be provided. As `seas` uses the SEATS procedure by default, the use of X11 has to be specified manually. When the `x11` spec is added as the input (as above), the mutually exlusive and default `seats` spec is automatically disabled. With `arima.model`, an additional spec/argument entry is added to the input of X-13ARIMA-SEATS. As the spec cannot be used with the default `automdl` spec, the latter is automatically disabled. The best way to learn about the relationship between the syntax of X-13ARIMA-SEATS and seasonal is to study the growing list of examples in the [wiki][examples].


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
  

### Output


`seas` returns an object of class `seas`, which is basically a list with the following elements:


Element    | Description
-----------|-----------------------------------------------
`data`     | An object of class `ts`, containing the seasonally adjusted data, the raw data, the trend component, the irregular component and the seasonal component. Accessing `data` is for the advanced user. In general, the adjusted series should be extracted with `predict`.
`spc`      | An object of class `spclist`, a list containing everything that is send to X-13ARIMA-SEATS. Each *spec* is on the first level, each *argument* is on the second level. Checking `spc` is useful for debugging.
`mdl`      | A list with the model specification, similar to `spc`. It typically contains `regression`, which contains the regressors and parameter estimates, and `arima`, which contains the ARIMA specification and the parameter estimates. The `summary` function gives an overview of the model.


### Graphs




### Inspect tool





### License

When released, the R code in seasonal is licensed under GPL-3. The package contains the X-13ARIMA-SEATS binary files from the United States Census Bureau, which are in the public domain. According to the [manual][manual] (page 1):

> When it is released, the X-13ARIMA-SEATS program will be in the public domain, and may be copied or transferred.

[manual]: http://www.census.gov/ts/x13as/docX13AS.pdf "Reference Manual"

[qref]: http://www.census.gov/ts/x13as/pc/qrefX13ASpc.pdf "Quick Reference"

[examples]: https://github.com/christophsax/seasonal/wiki/Examples-of-X-13ARIMA-SEATS-in-R "Wiki: Examples of X-13ARIMA-SEATS in R"





