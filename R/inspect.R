#' Interactively Inspect a Seasonal Adjustment Model
#' 
#' Interactively inspect a \code{"seas"} object. The goal of \code{inspect} is 
#' to summarize all relevant options, plots and statistics that should be 
#' usually considered.
#' 
#' Frequently used options can be modified using the drop down selectors in the
#' upper left window. Each change will result in a re-estimation of the seasonal
#' adjustment model. The R-call, the output and the summary are updated
#' accordingly.
#'
#' Alternatively, the R-Call can be modified manually in the lower left window.
#' Press 'Run Call' to re-estimate the model and to adjust the option selectors,
#' the output, and the summary. With the 'Close and Import' button, inspect is 
#' closed and the call is imported to R. The 'static' button substitutes 
#' automatic procedures by the automatically chosen 
#' spec-argument options, in the same way as \code{\link{static}}.
#'
#' The views in the upper right window can be selected from the drop down menu.
#'
#' The lower right panel shows the summary, as descibed in the help page of
#' \code{\link{summary.seas}}. The 'Full X-13 output' button opens the complete 
#' output of X-13 in a separate tab or window.
#' 
#' The views in \code{inspect} can be customized via the \code{fun} argument.
#' One or several plot functions may be supplied. The plot functions should have
#' a \code{"seas"} object as their only argument. Several functions must be 
#' wrapped in a list (see examples).
#' 
#' @param x an object of class \code{"seas"}. 
#' @param fun a function or a list of functions (see details)
#' @param check.version logical, should the version of shiny be checked
#' @param quiet logical, if \code{TRUE} (default), error messages from calls in 
#'   inspect are not shown in the console
#' @param ... further arguments, passed on to 
#'   \code{\link[shiny]{runApp}}. (The \code{launch.browser} argument of 
#'   version 0.8 can be still used that way)
#'   
#' @return an object of class \code{"seas"}, the modified model.
#'
#' @seealso \code{\link{seas}} for the main function of seasonal.
#'
#' @examples
#' \dontrun{
#' 
#' m <- seas(AirPassengers)
#' 
#' inspect(m)
#' 
#' m2 <- inspect(m)  # save the model after closing the GUI
#' 
#' ### customizing inspect
#' 
#' # a single function
#' # a new item 'User Plot' can be chosen in the 'View' menu
#' inspect(m, fun = function(x) hist(resid(x)))  
#' 
#' # more than one function collected in a list
#' myfun <- list()
#' myfun[['Spectum X-13']] <- function(m){
#'   plot(series(m, "spectrum.specorig", verbose = FALSE)[,-1], t = "l")
#' }
#' myfun[['Spectum R']] <- function(m){
#'   spectrum(diff(log(AirPassengers)), method = "ar", main = "")
#' }
#' inspect(m, myfun)
#' 
#' # and a bonus example
#' spaghetti <- function(m, back = 10){
#' ser <- original(m)
#' tx <- time(ser)[(length(ser) - back):length(ser)]
#' z <- NULL
#' for (txi in tx){
#'   assign(as.character(m$call$x), window(ser, end = txi))
#'   z <- cbind(z, final(eval(m$call)))
#' }
#' ts.plot(window(z, start = time(ser)[(length(ser) - back- 15)]), 
#'         col = rainbow(back + 1))
#' }
#' inspect(m, spaghetti)
#' 
#' }
#' @export
inspect <- function(x, fun = NULL, check.version = TRUE, quiet = TRUE, ...){ 

  if (!requireNamespace("shiny", quietly = TRUE)){
    stop("the inspect function depends on the 'shiny' package. To install it from CRAN, type: \n\n  install.packages('shiny')\n ")
  }

  if (packageVersion("shiny") < "0.11.1" && check.version){
    stop("You need to have at least shiny version 0.11.1 installed to run inspect smoothly. To ignore this test, use the 'check.version = FALSE' argument. To update shiny from CRAN, type:  \n\n  install.packages('shiny')\n")
  }

  if (!inherits(x, "seas")){
    stop("first argument must be of class 'seas'")
  }

  cat("Press ESC (or Ctrl-C) to get back to the R session\n")
  
  init.model <- x
  init.icstr <- format_seascall(init.model$call)

  # --- List with views --------------------------------------------------------

  # should be loaded for use with genhol
  data(holiday, envir = environment())  # avoid side effects

  # lookup table for output specification
  INSPDATA <- NULL 
  data(inspectdata, envir = environment())  # avoid side effects
  
  lSeries <- list()
  lSeries$MAIN <- c("Original and Adjusted Series" = "main", 
                    "Original and Adjusted Series (%)" = "mainpc", 
                    "SI-ratio" = "monthplot")


  INSPDATA2 <- INSPDATA[INSPDATA$seats, ]
  INSPDATA2$long <- gsub("seats.", "", INSPDATA2$long)
  INSPDATA2$spec <- gsub("seats", "seats/x11", INSPDATA2$spec)

  sp <- unique(INSPDATA2$spec)
  for (spi in sp){
    argi <- INSPDATA2[INSPDATA2$spec == spi, ]$long
    names(argi) <- INSPDATA2[INSPDATA2$spec == spi, ]$descr
    lSeries[[toupper(spi)]] <- argi
  }

  # --- User defined views -----------------------------------------------------

  lUserView <- list()
  if (!is.null(fun)){
    if (is.function(fun)){
      lUserView[["User Plot"]] <- fun
    } else if (is.list(fun)){
      for (i in 1:length(fun)){
        if (is.function(fun[[i]])){
          if (is.null(names(fun)[i])){  # allways use a name
            names(fun)[i] <- paste("User Plot", i)
          } else if (is.na(names(fun)[i])){
            names(fun)[i] <- paste("User Plot", i)
          }
          lUserView[[names(fun)[i]]] <- fun[[i]]
        } else {
          warning(paste(names(fun)[i], "is not a function, skipping it."))
        }
      }
    } else {
      stop("fun argument must be either a function or a list of functions.")
    }
  }

  if (length(lUserView) > 0){
    lSeries$USER <- structure(names(lUserView), names = names(lUserView))
  }

  # --- List with options ------------------------------------------------------

  lFOpts <- list()
  lFOpts$method <- c("SEATS", "X11")

  lFOpts$transform <- 
    list("AUTOMATIC" = list("AIC Test" = "auto"), 
         "MANUAL" = list("Logarithmic" = "log", 
                         "Square Root" = "sqrt",
                         "No Transformation" = "none"))

  lFOpts$arima <- 
    list("AUTOMATIC" = list("Auto Search" = "auto"))

  lFOpts$outlier <- 
    list("AUTOMATIC" = list("Auto Critical Value" = "auto", 
                            "Low Critical Value (3)" = "cv3", 
                            "Medium Critical Value (4)" = "cv4",
                            "High Critical Value (5)" = "cv5"), 
         "MANUAL" = list("No detection" = "none"))

  lFOpts$easter <- 
    list("AUTOMATIC" = list("AIC Test Easter" = "easter.aic"), 
        "MANUAL" = list("1-Day before Easter" = "easter[1]", 
                        "1-Week before Easter" = "easter[8]", 
                        "Chinese New Year" = "cny",
                        "Indian Diwali" = "diwali",
                        "No Adjustment" = "none"))
  lFOpts$td <- 
    list("AUTOMATIC" = list("AIC Test" = "td.aic"), 
        "MANUAL" = list("1-Coefficient" = "td1coef", 
                        "6-Coefficients" = "td", 
                        "No Adjustment" = "none"))

  lFOpts.unlist <- lapply(lFOpts, unlist)

  lFOpts.user <- lFOpts
  for (i in 2:length(lFOpts)){
     lFOpts.user[[i]]$MANUAL$User <- "user"
  }

  shiny::runApp(list(
    ui = shiny::bootstrapPage(
      shiny::includeCSS(file.path(system.file("inspect", package="seasonal"), 
                                  "www/css/docs.css")),
      shiny::includeHTML(file.path(system.file("inspect", package="seasonal"), 
                                   "www/index.html"))
      ),
    server = function(input, output) {

      # --- reactive model estimation ------------------------------------------

      # reactive values
      rUploadUpd <- shiny::reactiveValues(upd = 0)  
      rTerminalUpd <- shiny::reactiveValues(upd = 0)  
      rPlotUpd <- shiny::reactiveValues(upd = 0)  
      rTerminalError <- shiny::reactiveValues(upd = 0)   
      rModel <- shiny::reactiveValues() 
      rFOpts <- list()

      gLastView <- "main"
      gFiveBestMdl <- fivebestmdl(init.model)


      rSelectUpd <- shiny::reactiveValues(upd = 0)  

      # --- initialisazion -----------------------------------------------------

      rModelCall  <- shiny::reactiveValues(cstr = init.icstr)
      gTerminalCall <- init.icstr


      # model evaluation, updated by rModelCall$cstr or rUploadUpd$upd
      shiny::observe({
        cstr <- rModelCall$cstr
        z <- EvalOrFail(cstr)
        if (inherits(z, "try-error")){
          rTerminalError$error <- z

        } else {

          if (!is.null(shiny::isolate(rTerminalError$error))){
            rTerminalError$error <- NULL
          }

          gfo <- GetFOpts(z)
          rFOpts$method <- gfo$method
          rFOpts$transform <- gfo$transform
          rFOpts$arima <- gfo$arima
          rFOpts$outlier <- gfo$outlier
          rFOpts$easter <- gfo$easter
          rFOpts$td <- gfo$td
          rModel$m <- z
          gTerminalCall <<- cstr
          rTerminalUpd$upd <- shiny::isolate(rTerminalUpd$upd) + 1
        }
      })


      # --- auto input elements ------------------------------------------------------

      output$oFOpts <- shiny::renderUI({
        fopts <- GetFOpts(rModel$m)

        # update if new fivebestmdl are available, otherwise, use last fivebestmdl
        if (is.null(rModel$m$spc$automdl$print)){
          fbm <- gFiveBestMdl
        } else {
          fbm <- fivebestmdl(rModel$m)
          gFiveBestMdl <<- fbm
        }

        if (!fopts$arima %in% c("auto", fbm$arima)){
          fopts$arima <- "user"
        }

        lFOpts2 <- lFOpts

        is.user <- sapply(fopts, identical, "user")
        lFOpts2[is.user] <- lFOpts.user[is.user]

        ll <- as.list(fbm$arima)
        names(ll) <- ll

        lFOpts2$arima$MANUAL <- c(ll, lFOpts2$arima$MANUAL)

        list(
          shiny::selectInput("iMethod", "Adjustment Method", 
            choices = lFOpts2$method, selected = fopts$method, width = '100%'),
          shiny::selectInput("iTransform", "Pre-Transformation", 
            choices = lFOpts2$transform, selected = fopts$transform, width = '100%'),
          shiny::selectInput("iArima", "Arima Model", 
            choices = lFOpts2$arima, selected = fopts$arima, width = '100%'),
          shiny::selectInput("iOutlier", "Outlier", 
            choices = lFOpts2$outlier, selected = fopts$outlier, width = '100%'),
          shiny::selectInput("iEaster", "Holiday", 
            choices = lFOpts2$easter, selected = fopts$easter, width = '100%'),
          shiny::selectInput("iTd", "Trading Days", 
            choices = lFOpts2$td, selected = fopts$td, width = '100%')
          )
      })

      # auto update
      shiny::observe({ 
        rFOpts$method <- (input$iMethod)
        rFOpts$transform <- (input$iTransform)
        rFOpts$arima <- (input$iArima)
        rFOpts$outlier <- (input$iOutlier)
        rFOpts$easter <- (input$iEaster)
        rFOpts$td <- (input$iTd)

        if (is.null(shiny::isolate(rModel$m))){
          m <- x
        } else {
          m <- shiny::isolate(rModel$m)
        }

        cl <- AddFOpts(m, shiny::isolate(rFOpts))

        cstr <- format_seascall(cl)
        gTerminalCall <<- cstr

        # assignment will trigger reevaluation
        rModelCall$cstr <- cstr
       })

      # terminal update
      shiny::observe({ 
        if (input$iEvalCall > 0){
          # assignment will trigger reevaluation
          rModelCall$cstr <- shiny::isolate(input$iTerminal)

        }
       })

      shiny::observe({ 
        if (input$iOutput > 0){
          out(shiny::isolate(rModel$m))
        }
       })

      # --- manual input elements ----------------------------------------------

      # display terminal, updated by rTerminalCall$cstr
      output$oTerminal <- shiny::renderUI({
        rTerminalUpd$upd
        cstr <- gTerminalCall
        shiny::tags$textarea(id="iTerminal", class="form-control", rows=13, 
                             cols=60, cstr)
      })

      # display error message and revert button, updated by rCall$error
      output$oRevert <- shiny::renderUI({
        if(!is.null(rTerminalError$error)){
           pp <- shiny::HTML(paste0('<div class="alert alert-danger alert-dismissible fade in" role="alert">
            <button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>
            ', err_to_html(rTerminalError$error), '<br>
            <p>
              <button id="iRevert" type="button" class="btn action-button btn-danger" style = "margin-right: 4px;">Revert</button>
            </p>
          </div>'))
          return(pp)
        } else {
          return(NULL)
        }
      })


      # revert rTerminalCall$cstr, updated by input$iRevert
      shiny::observe({ 
        if (!is.null(input$iRevert)){
          if (input$iRevert > 0){
            # increasing the reactive variable will update the Terminal
            rTerminalUpd$upd <- shiny::isolate(rTerminalUpd$upd) + 1  
            rTerminalError$error <- NULL
          }
        }
       })


      # --- output: view -------------------------------------------------------

      # view selector
      output$oSeries <- shiny::renderUI({
        if (is.null(input$iMethod)){
          cc <- lSeries
        } else if (input$iMethod == "X11"){
          cc <- lSeries
          cc$FORECAST <- c(cc$FORECAST, "Backcasts" = "forecast.backcasts")
        } else {
          cc <- lSeries
        }
        view <- gsub("x11.", "", gLastView, fixed = TRUE)
        view <- gsub("seats.", "", view, fixed = TRUE)
        shiny::selectInput("iSeries", NULL, choices = cc, selected = view)
      })

      # update call if required by view
      shiny::observe({ 
        iSeries <- input$iSeries
        if (!is.null(iSeries)){

          # no call updates for user defined view functions
          if (iSeries %in% names(lUserView)) {
            rPlotUpd$upd <- shiny::isolate(rPlotUpd$upd) + 1
            gLastView <<- iSeries
            return(NULL)
          }

          # remove existing save INSPDATA
          m <- shiny::isolate(rModel$m)
          lcl <- as.list(m$call)


          if (iSeries %in% c("irregular", "seasonal", "trend")){
            iSeries <- paste0(tolower(input$iMethod), ".", iSeries)
            if (input$iMethod == "SEATS"){
              lcl$x11 <- NULL
            } else {
              lcl$x11 <- ""
            }
          }

          gLastView <<- iSeries

          # remove backcast additionals if unused
          if (iSeries != "forecast.backcasts"){
            lcl$forecast.maxback <- NULL
            lcl$forecast.backcasts <- NULL
          } else {
            if (input$iMethod == "SEATS"){
              lcl$x11 <- NULL
              lcl$forecast.maxback <- NULL
              lcl$forecast.backcasts <- NULL
              iSeries <- "main"
              gLastView <<- "main"
            }
          }

          cl <- as.call(lcl[!(names(lcl) %in% paste0(INSPDATA$spec, ".save"))])

          if (!iSeries %in% c("main", "mainpc", "monthplot")){ 
            if (iSeries %in% c("irregular", "seasonal", "trend")){
              iSeries <- paste0(tolower(input$iMethod), ".", iSeries)
            }
            cstr <- format_seascall(AddSeriesToCall(cl, iSeries, INSPDATA))
            rModelCall$cstr <- cstr
          } 
          rPlotUpd$upd <- shiny::isolate(rPlotUpd$upd) + 1
        }
      })


      output$oMainPlot <- shiny::renderPlot({
        rPlotUpd$upd  # trigger on demand

        par(mar = c(3, 3, 1.7, 1.7) -1)

        iSeries <- shiny::isolate(input$iSeries)
        if (is.null(iSeries)) {iSeries <- gLastView}
        if (iSeries == "main") return(plot(rModel$m, main = ""))
        if (iSeries == "mainpc") return(plot(rModel$m, main = "", 
                                        transform = "PC"))
        if (iSeries == "monthplot"){
          # instead of the seas method, to hide the main title
          shiny::validate(shiny::need(("seasonal" %in% colnames(rModel$m$data)), 
                    "This view is not available for the model. Change view or model."))

          monthplot(rModel$m$data[,'seasonal'], ylab = "", lwd = 2, col = "red", 
                    main = "")
          return(monthplot(siratio(rModel$m), col = "blue", type = "h", 
                 add = TRUE))
        }

        if (iSeries %in% c("irregular", "seasonal", "trend")){
          iSeries <- paste0(tolower(input$iMethod), ".", iSeries)
        } 

        if (iSeries %in% names(lUserView)){
          return(lUserView[[iSeries]](rModel$m))
        } 

        s <- series(rModel$m, iSeries, reeval = FALSE)
        shiny::validate(shiny::need(inherits(s, "ts"), 
          "This view is not available for the model. Change view or model."))
        if (inherits(s, "ts")){
          return(ts.plot(s, ylab = ""))
        }

      })

      # --- output: other ------------------------------------------------------

      output$oSummary <- shiny::renderPrint({
          PrintSummarySeas(summary(rModel$m))
      })

      # --- close and import ---------------------------------------------------

      shiny::observe({
        if (input$iClose > 0){
          shiny::stopApp(returnValue = shiny::isolate(rModel$m))
        }
      })


      shiny::observe({
        if (input$iStatic > 0){
          m <- shiny::isolate(rModel$m)
          scl <- static(m, test = FALSE)
          # fix to avoid reevalation after sorting by AddFOpts
          if (!is.null(scl$regression.variables)){
            rv <- scl$regression.variables
            eav <- c("easter[1]", "easter[8]", "easter[15]")
            tdv <- c("td", "td1coef")
            rv <- c(rv[!rv %in% eav], rv[rv %in% eav])
            rv <- c(rv[!rv %in% tdv], rv[rv %in% tdv])
            scl$regression.variables <- rv
          }
          cstr <- format_seascall(scl)
          gTerminalCall <<- cstr
          rModelCall$cstr <- cstr
        }
      })

    }
  ), quiet = quiet, ...)

}
