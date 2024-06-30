1.10.0
--------------------------------------------------------------------------------

minor changes

  - Improved messages on rerun in series()
  - Spec list in ?series is automatically parsed from the X-13 manual
  - Save output files if 'dir' is specified
  - Improvements when reading spc files #320 #323 #321


1.9.0
--------------------------------------------------------------------------------

bug fix
  - seas() works within forking parallelization (e.g., mclapply) #276
    Thanks to @jonlachmann!
  - annual series are read correctly #264
  - Jan 1 can be used in genhol() #261
  - genhol() produces correct result in 1931 #193
  - out() works on composite objects #278
  - series() works on composite objects #278
  - Keep series specification also in `noinst/specs/specs.csv`
  - Adjust specs to include composite series, some additional seats series


1.8.4 2021-09-17
--------------------------------------------------------------------------------

bug fix
  - Avoid setting X13_PATH environment variable. Fix M1 startup issue #267


1.8.3  2021-06-14
--------------------------------------------------------------------------------

bug fix
  - Make sure example(seas) does not leave anything behind
  - escape tests on M1


1.8.2  2021-03-23
--------------------------------------------------------------------------------

bug fix
  - CRAN M1: rm detritus in the temp directory


1.8.1  2021-03-13
--------------------------------------------------------------------------------

bug fix
  - CRAN: do not run example on Solaris


1.8.0  2021-03-05
--------------------------------------------------------------------------------

new features
  - support for multiple time series and the composite spec

enhancements
  - static() prints a prettier output

bug fix
  - as.data.frame() fix date conversion for frequency 4, 2 (#256)
  - static() drop occasional warning (#235)
  - fix some backslash confusion on Windows (#237)


1.7.1  2020-06-06
--------------------------------------------------------------------------------

bug fix
  - specify convert.type(as.is = TRUE), to comply with next R


1.7.0  2018-12-19
--------------------------------------------------------------------------------

minor changes

  - like update(), static now uses m$list, rather than m$call. This makes it
    usable in lapply or other functions.

bug fix
  - fix dates for Chinese New Year in 2015 and 2017 (#219)
  - outlier(): reverted workaround for problem with byte compiler in R 3.4.0,
    which has been fixed in R. Fixes incomplete outliers in view() and bug
    in identify. (#211, #212)
  - fix error in summary when no seasonal adjustment is requested (#226)

documentation
  - article in Journal of Statistical Software as vignette
  - README.md providing a short overview of the package
  - citation referring to publication in Journal of Statistical Software


1.6.1  2017-05-02
--------------------------------------------------------------------------------

enhancements

  - A second, more verbose run after a non-zero exit, to detect Fortran run time
    errors (https://github.com/christophsax/seasonal/issues/194)

bug fix
  - deal with new file in build 1.1.39 (macOS)
  - outlier(): workaround for problem with byte compiler in R 3.4.0
    https://github.com/christophsax/seasonal/issues/209
  - import.spc(): fixed parsing of explicitly specified NULL entries
    E.g., regression{aictest=()} results in now regression.aictest = NULL
    (https://github.com/christophsax/seasonal/issues/202)
  - series(), out(): fix if model was updated using update()
    (https://github.com/christophsax/seasonal/issues/191)
  - some X-13 notes were not shown in summary
    (https://github.com/christophsax/seasonal/issues/200)
  - static parsing error with change of regime
    (https://github.com/christophsax/seasonal/issues/205)


1.5.1  2017-02-12
--------------------------------------------------------------------------------

bug fix
  - removing 'udg' classification and as.data.frame() method from udg() output,
    because it messes up with several other functions.


1.5.0  2017-02-12
--------------------------------------------------------------------------------

defunct
  - inspect() is defunct, use view() instead. Shiny application code removed
    from package.

enhancements
  - as.data.frame() methods to coerce output of seas, summary.seas() and udg()
    to a data.frame. Useful for further processing (experimental). Replaces the
    experimental 'data.frame' argument in summary.seas() and udg() from the last
    version.


1.4.0  2016-12-23
--------------------------------------------------------------------------------

new features
  - seasonal now uses the graphical user interface from the new seaonalview
    package. To install from CRAN, use: install.packages('seasonalview')
    Use the 'view()' function instead of 'inspect', which is deprecated and will
    be defunct soon.

enhancements
  - new update() method, updates a "seas" object, works with R generic.
  - new predict() method, returns the final series of an (updated) "seas"
    object, works with R generic.
  - static() function has new argument 'x11.static'. If TRUE, moving averages
    of X-11 are fixed as well.
    (Thanks to Brian Monsell)
  - static() function has new argument 'evaluate'. If TRUE, it returns an
    evaluated call, an object of class "seas".
  - import.spc() function has new argument 'text', to accept the content of an
    X-13 .spc file as a character vector.
  - udg() and summary.seas() gain an optional 'data.frame' argument, which
    returns their info as a data.frame. Useful for further processing.
    (experimental)

bug fixes
  - import.spc imports arima models with commas. E.g., (0, 1, 1).
    (Thanks to Brian Monsell)
  - substitute colons in series name to avoid reading error.
  - shorten long series names to avoid X-13 error.
    (Both thanks to Bill Dunlap)
  - missing udg element in summary no longer leads to an error.
  - xreg specification ensures regression spec is used.
    (Thanks to Jose Pavia)
  - outlier plotted correctly for bi-annual data (SEATS only).

minor changes
  - U.S. unemployment as example series (experimental).
  - 'plot', 'residplot' and 'monthplot' are easier to customize.
  - import.spc() and import.ts() no longer experimental. They are here to stay.

documentation
  - reflect switch from inspect() to view()
  - documentation for update.seas() and predict.seas()
  - updated vignette
  - example on 'composite' in ?seas


1.3.0  2016-08-07
--------------------------------------------------------------------------------

new features
  - udg(): utility function to extract type converted udg statistics
    e.g., M quality statistics of X-11
  - summary() can be customized, e.g, to include M quality statistics
  - methods for R generics: AIC(), BIC(), nobs(), logLik()

deprecated
  - non-html binaries: startup message to announce drop of support
  - arimamodel(): use the more general 'udg(x, "x13mdl")'

under the hood
  - statistics used by other functions (e.g., summary) are imported by udg()
  - 'lks' output of X-13 is not used and produced anymore, as the content can be
    accessed by udg()

documentation
  - updated documentation to include udg()
  - various minor adjustments

bug fixes
  - estimate.armacmatrix can now be imported
    (https://github.com/christophsax/seasonal/issues/160)
  - minor fixes in inspect()
    (https://github.com/christophsax/seasonal/issues/162)
  - range outliers (e.g. TL) show up in plot.seas() with start date, rather
    than causing an error.
    (https://github.com/christophsax/seasonal/issues/163)


1.2.1  2016-02-11
--------------------------------------------------------------------------------

bug fixes
  - If the startup check of x13binary has failed, this prevented seasonal from
    being loaded and installed. Now the startup check tells you what to do in
    such a case. This affects, e.g., Windows computers with library location set
    to an UNC path. (https://github.com/x13org/x13binary/issues/30)
  - temporarily change working directory on Windows, as X-13 (sometimes) writes
    to it. (https://github.com/x13org/x13binary/issues/28)


1.2.0  2016-01-22
--------------------------------------------------------------------------------

major changes
  - No separate binary download required anymore! seasonal now depends on the
    x13binary package to access pre-built binaries of X-13ARIMA-SEATS for all
    platforms. Many thanks to Dirk Eddelbuettel for the fantastic work on
    x13binary!

    Installing seasonal and the binaries is now as easy as:

    install.packages('seasonal')

enhancements
  - basic X-13 tests on all CRAN platforms (except Solaris)
  - The return value of the (experimental) import.spc function has a better name
    for the main component ('$seas', instead of '$call').

documentation
  - updated documentation to reflect the new installation process
  - new example in ?genhol: replicate X-13 Arima model in R
  - improved description of examples in ?import.spc

bug fixes
  - fix for rare convergence error messages
  - annual series are now recognized
  - 'inspect' call evaluated in global env. This solves an issue with series
    that had the same name as primitive functions (e.g., 'prod')
  - seas can be imported and used without loading the namespace
    (e.g. seasonal::seas())
  - plot.seas: fixed wrong sign in transfom = "PC" and "PCY"

defunct
  - slidingspans, revisions, regressioneffects have been defunct for a while and
    were removed from the package


1.1.0  2015-10-08
--------------------------------------------------------------------------------

bug fixes
  - single series are dimension-less ts objects, rather than n times 1 matrices
  - more precise error msg in parse_spc

documentation
  - updated links to vignette and examples
  - new example in series


1.0.0  2015-08-14
--------------------------------------------------------------------------------

new features
  - experimental import.spc, to read existing .spc files into R.
    Thanks, Brian Monsell.
  - experimental import.ts, to read existing X-13 data files into R.
  - with 'seats = NULL', seasonal adjustment can be completely turned of, to use
    X-13 for other purposes than seasonal adjustment.

enhancements
  - within-session parallelization: the temporary location name of the X13 run
    now changes each time, which makes the function suitable for within-session
    parallelization (e.g., with mclapply). Cluster parallelization using
    parlApply works as before and is recommended on Windows machines.
    Thanks, Vidur Dhanda.
  - automated testing of all wiki examples on travis
  - error messages in inspect are html formatted

under the hood
  - rewritten parsing for spc and mdl files

bug fixes
  - some X13 warnings were not fully shown in summary
  - binary name now also shown in checkX13 on windows
  - fixes for error message parsing in non-html version


0.90.0  2015-05-08
--------------------------------------------------------------------------------

enhancements
  - completely redesigned inspect() graphical user interface, with the look and
    feel of the demo website: www.seasonal.website
  - support for the new X13 version 1.1 build 19 (released April 2, 2015)

under the hood
  - temporary files are cleaned up after execution

documentation
  - extended documentation for inspect()
  - list argument: no longer experimental

bug fixes
  - fixed parsing problem with build 19 in qs() that affected summary()
  - fixed parsing problem with exogenous regressors when using the list arg
  - input longer than 133 char is broken into multiple lines to avoid X-13 error
  - names of ARMA coefficients contain lag rather than period
  - X13 warnings are shown in summary (again)
  - better parsing breaking errors


0.80.0  2015-02-05
--------------------------------------------------------------------------------

enhancements
  - improved parsing and integration of X-13 error messages
  - checkX13 performs more tests and returns an error report.
  - spec-arguments can also be entered as a named list (experimental)

under the hood
  - automated tests for all calls in the wiki

documentation
  - new vignette sections on user defined holidays and production use
  - new example on Indian Diwali in ?genhol
  - dates of Indian Diwali and Indian industrial production
  - more detailed documentation for ?summary.seas explaining output details

bug fixes
  - genhol() can be used inside a seas() call
  - arimamodel() shows model as expected

defunct
  - slidingspans, revisions, regressioneffects changed from deprecated to defunct


0.70.1  2014-10-02
--------------------------------------------------------------------------------

new features
  - support for the HTML version of X-13. The 'out' function displays the output
    of X-13 in the browser.
  - The 'inspect' function is based on Shiny and can be used without RStudio. It
    offers access to all series that can be produced by X-13, as well as access
    to the HTML output of X-13.

enhancements
  - empty specs can be specified as empty strings. This is more intuitive than
    the previously employed empty lists (which are still working). Empty
    strings are also used as defaults and in the documentation.
  - improved parsing of X-13 messages.
  - documentation improvements.

under the hood
  - flags are used when calling X-13. This makes some early g77 compilations for
    OS-X unusable.
  - use of the .udg file to extract summary statistics.
  - support for HTML and non-HTML versions of X-13.

bug fixes
  - numerical specification of arima.model led to wrong seasonal part.
  - restrict start year > 999, to avoid some millenium bugs.
  - si-ratio was wrongly extracted if no trend was estimated
  - 'series' drops an error if the requested series is not compatible with the
    adjustment method, rather than adjusting the method. E.g., if x11.trend is
    requested from a seats model, an error will be returned, rather than
    switching the adjustment to x11. The old behavior was confusing.


0.60.0  2014-05-09
--------------------------------------------------------------------------------

new features
  - views of 'inspect' are now customizable
  - 'identify' method for outlier selection by point and click
  - demo(seas) for a demonstration of basic functionality


0.50.0  2014-03-27
--------------------------------------------------------------------------------

new features
  - 'series' function, which allows the import of all tables produced by X-13
    (except composite spec)

enhancements
  - new na.action function: NA handling by X-13 (including interpolation)
  - 'out' function also reads '.log'  and '.err' files
  - improved help pages
  - all examples from the X-13 manual are now in the wiki (except composite)
  - 'static' replicates a wider set of calls
  - summary shows decomposition method (X11, SEATS)
  - more flexible arima model specification (numeric vectors also allowed)

deprecated
  - the functionality of 'slidingspans', 'revisions' and 'regressioneffects' is
    now part of the new 'series' function. The old functions are deprecated.

code improvements
  - code takes advantage of the series function, simplified import from X-13


0.40.0  2014-02-12
--------------------------------------------------------------------------------

new features
  - support for 'slidingspans' and 'history' spec
  - genhol, a function to generate holiday regression variables
  - user defined variable for 'transform' spec
  - NA handling like in standard R
  - inspect function with model and transform selection, new views

enhancements
  - new diagnostical statistics
  - summary shows some useful statistics
  - monthplot also shows SI ratios
  - easier access to X-13 .out content
  - regression effects can be extracted
  - checkX13 performs a test run
  - 'regression' and 'x11regression' specs can be combined
  - fewer, more structured help pages
  - comprehensive list of examples from the official manual

bug fixes
  - quotes in series name are now allowed
  - support for unorthodox frequencies (e.g. 2, 'seats' only)
  - spaces in folder names on windows are now allowed

code improvements
  - uses only standard R functions, no dependency on 'stringr'
  - clearer code structure
