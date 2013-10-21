seasonal: R interface to X-13ARIMA-SEATS
----------------------------------------

seasonal is an easy-to-use interface to X-13ARIMA-SEATS. X-13ARIMA-SEATS is a seasonal adjustment software produced, distributed, and maintained by the United States Census Bureau. X-13ARIMA-SEATS combines and extends the capabilities of the older X-12ARIMA (developed by the Census Bureau) and the TRAMO-SEATS (developed by the Bank of Spain) software packages. 


### An basic session

`seas` ist the core function of seasonal. By default, `seas` calls the automatic procedures of X-13ARIMA-SEATS to perform a seasonal adjustement that works very well in most circumstances. It returns an object of class `seas` that contains all necessary information on the adjustment process, as well as the series. The `predict` method for `seas` objects returns the adjusted series, the `plot` method shows a plot with the unadjusted and the adjusted series. 

     x <- seas(AirPassengers)
     predict(x)
     plot(x)
     
By default, `seas` calls the SEATS adjustemnt procedure. If you prefer the X11 adjustemnt filter, use the following option:

     seas(AirPassengers, method = "x11")
     
The default invoces the automatic procedures of X-13ARIMA-SEATS. They include:
  - ARIMA model search
  - outlier detection
  - detection of trading day and easter effects

Alternatively, all inputs can be entered manually, as in the following example (see the section below foron on the syntax)


Fo




As far as possible, seasonal uses the same syntax as X-13ARIMA-SEATS, and allows the inclusion of (almost) all options



### X-13ARIMA-SEATS






#### Parsing

- Universal functions to parse X13 files to an R list (DONE).

#### Run x13 from R (DONE)

- Basic Call:

    C:\Users\seco-sxh\software\x13as\x13as.exe C:\Users\seco-sxh\software\x13as\Testairline.spc

- ts to series spc as a first step towards an interface


#### Automatic Specification

- Outlier

- Model Selection

- Trading Days


#### Graphs, Analytics

- List of Graphs from WinX13

- Graphs form seasonaladjustment.com 

- Implementation Concept

#### Documentation


