seasonal: an R interface to X-13ARIMA-SEATS
-------------------------------------------

seasonal is an easy-to-use interface to X-13ARIMA-SEATS. X-13ARIMA-SEATS is a seasonal adjustment software **produced, distributed, and maintained by the United States Census Bureau**. X-13ARIMA-SEATS combines and extends the capabilities of the older X-12ARIMA (developed by the Census Bureau) and the TRAMO-SEATS (developed by the Bank of Spain) software packages. For information on X-13ARIMA-SEATS, see the [manual][manual] or the [quick reference][qref].


### Installation

To install directly from github to R, substitute your github 'USERNAME' and 'PASSWORD':

    require(devtools)
    install_github('seasonal', 'christophsax', auth_user = 'USERNAME', password = 'PASSWORD')
    
seasonal includes the binary files of X-13ARIMA-SEATS, so no separte installation is needed.


### Introductiory session

`seas` ist the core function of seasonal. By default, `seas` calls the automatic procedures of X-13ARIMA-SEATS to perform a seasonal adjustement that works very well in most circumstances. It returns an object of class `seas` that contains all necessary information on the adjustment process, as well as the series. The `predict` method for `seas` objects returns the adjusted series, the `plot` method shows a plot with the unadjusted and the adjusted series. 

     x <- seas(AirPassengers)
     predict(x)
     plot(x)
     
The first argument must a be time series of class `ts`. By default, `seas` calls the SEATS adjustemnt procedure. If you prefer the X11 adjustemnt filter, use the following option:

     seas(AirPassengers, method = "x11")
     
The default invoces the automatic procedures of X-13ARIMA-SEATS. They include:
  - ARIMA model search
  - outlier detection
  - detection of trading day and easter effects

Alternatively, all inputs can be entered manually, as in the following example (see the section below for details on on the syntax):

The `static` command reveals the static call that is needed to replicate a a seasonal adjustment procedure. E.g.

    static(x)
    
If you are using R Studio, the `inspect` command offers a way to analyze and modify a seasonal adjustment procedure (see the section below for details):

    inspect(x)


### X-13ARIMA-SEATS syntax

Whenever possible, seasonal uses the same syntax as X-13ARIMA-SEATS. Thus, it should be possible to invoce (almost) all options that are available in X-13ARIMA-SEATS. For details on the options, see the [manual][manual]. The X-13ARIMA-SEATS syntax uses *Specs* and *Arguments*, while each Spec may contain some Arguments. An additional Spec/Argument can be added to the `seas` function by separating Spec and Argument by a `.`. For example, in order to set the `variable` argument of the `regression` spec equal to `td` and `ao1999.01)`, the input to `seas` looks like this:

    x <- seas(AirPassengers, regression.variable = c("td", "ao1965.jan"))
   
Note that R vectors can used as an input. It is possible to manipulate almost all inputs to X-13ARIMA-SEATS that way. Most examples from the [manual][manual] should be replicable. For instance, example 1 from the manual

    series { title  =  "Quarterly Grape Harvest" start = 1950.1
           period =  4
           data  = (8997 9401 ... 11346) }
    arima { model = (0 1 1) }
    estimate { }

translates to R the following way:

    seas(X-13ARIMA-SEATS,
         arima.model = "(0 1 1)")
    
`seas` takes care of the series argument. with `arima.model` an addtional Spec/Argument entry is added to the input file to X-13ARIMA-SEATS. As the Spec cannot be used with the default automdl spec, the latter is removed.


### Graphs, Output



### Inspect tool



### Licence

When released, the R code in seasonal will be GPL-3. The package contains the X-13ARIMA-SEATS binary files from the United States Census Bureau. According to the manual, may

When it is released, the X-13ARIMA-SEATS program will be in the public domain, and may be copied or transferred

[manual]: http://www.census.gov/ts/x13as/docX13AS.pdf "Reference Manual"

[qref]: http://www.census.gov/ts/x13as/pc/qrefX13ASpc.pdf "Quick Reference"

