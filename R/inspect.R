#' Interactively Inspect a Seasonal Adjustment Model
#' 
#' Interactively inspect a \code{"seas"} object. The goal of \code{inspect} is 
#' to summarize all relevant options, plots and statistics that should be 
#' usually considered.
#' 
#' The \code{inspect} function opens an interactive window that allows for the 
#' manipulation of a number of arguments. It offers several views to analyze the
#' series graphically. With each change, the adjustment process and the 
#' visualizations are recalculated.
#' 
#' Summary statistics are shown in the first tab. The last tab offers access to
#' all series that can be produced with X-13.
#' 
#' The views in \code{inspect} may be customized via the \code{fun} argument.
#' One or several plot functions may be supplied. The plot functions should have
#' a \code{"seas"} object as their only argument. Several functions must be 
#' wrapped in a list (see examples).
#' 
#' @param x an object of class \code{"seas"}
#' @param launch.browser how to open the app. Argument passed on to 
#'   \code{\link[shiny]{runApp}}.
#' @param fun a function or a list of functions (see details)
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
#' 
#' ### customizing inspect
#' 
#' # a single function
#' fc <- function(m){
#'   ts.plot(series(m, "fct", verbose = FALSE))
#' }
#' inspect(m, fc)
#' 
#' # more than one function collected in a list
#' myfun <- list()
#' myfun[['Spectum X-13']] <- function(m){
#'   plot(series(m, "spectrum.specorig", verbose = FALSE)[,-1], t = "l")
#' }
#' myfun[['Spectum R']] <- function(m){
#'   spectrum(diff(log(AirPassengers)), method = "ar")
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
inspect <- function(x, fun = NULL, launch.browser = getOption("shiny.launch.browser", interactive())){  
  
  if (!suppressWarnings(require(shiny))){
    stop("the inspect function depends on the 'shiny' package. It can be installed from CRAN: \n\n  install.packages('shiny')\n ")
  }
  
  icl <- match.call()
  # --- global -----------------------------------------------------------------
  
  SPECS <- NULL 
  data(specs, envir = environment())  # avoid side effects
  SPECS <- SPECS[-c(296:317),]  # dont show x11regression
  
  # construct a list with plots
  vl <- list()
  vl[['Main Plot']] <- plot
  vl[['SI Component']] <- monthplot
  if (!is.null(fun)){
    if (is.function(fun)){
      vl[[deparse(substitute(fun))]] <- fun
    } else if (is.list(fun)){
      for (i in 1:length(fun)){
        if (is.function(fun[[i]])){
          if (is.null(names(fun)[i])){  # allways use a name
            names(fun)[i] <- paste("User Plot", i)
          } else if (is.na(names(fun)[i])){
            names(fun)[i] <- paste("User Plot", i)
          }
          vl[[names(fun)[i]]] <- fun[[i]]
        } else {
          warning(paste(names(fun)[i], "is not a function, skipping it."))
        }
      }
    } else {
      stop("fun argument must be either a function or a list of functions.")
    }
  }
  
  # use 'computation on the language' to construct the main panel
  tab.expr <- 'mainPanel(tabsetPanel(
            tabPanel("Summary", verbatimTextOutput("modelSummary")),'
  for (i in 1:length(vl)){
    tab.expr <- paste0(tab.expr, 'tabPanel("', names(vl)[i], '", plotOutput("vl', i, '")),\n')
  }
  tab.expr <- paste0(tab.expr, 'tabPanel("All Series", selectInput("userview", label = NULL, choices=  SPECS$long[SPECS$is.series])
, textOutput("moreText", tags$em), plotOutput("morePlot"), helpText("To reproduce the data in R, type:"), verbatimTextOutput("moreRepro")), type = "pills"))')
  main.panel <- eval(parse(text = tab.expr))
  
  # list with arima models
  am <- unique(c(x$model$arima$model, fivebestmdl(x)[,1]))
  arima.models <- as.list(am)
  names(arima.models) <- am

  # list with aic tests
  aic.tests <- list("trading days" = "td", "easter" = "easter")

  # adj of input model
  adj.method <- if (!is.null(x$spc$seats)) {"seats"} else {"x11"}
  
  runApp(list(
    # --- UI -------------------------------------------------------------------
    ui = fluidPage(
      titlePanel("seasonal: X13-ARIMA-SEATS interface"),
      sidebarLayout(
        sidebarPanel(
          radioButtons("method", "Adjustment method:",
                       choices = c("SEATS" = "seats", "X-11" = "x11"), 
                       selected = if (!is.null(x$spc$seats)) {"seats"} else {"x11"}),
          selectInput("model", "ARIMA Model:",
                      arima.models, selected = x$model$arima$model),
          selectInput("aictest", "AIC-test for:",
                      aic.tests, selected = aic.tests, multiple = TRUE),
          sliderInput("outlier.critical", "Critical outlier value", 2.5, 5, value = 4),
          if (getOption("htmlmode") == 1) {checkboxInput("outBox", "Update X-13 Output")} else {NULL},
          actionButton("stopButton", "Close and import Call to R", icon = icon("download"))
        ),
        main.panel
      )
    ),
    
    # --- Server ---------------------------------------------------------------
    server = function(input, output, session) {
    
      if (getOption("htmlmode") == 1){
        observe({
          if (input$outBox){
            out(mod())
          }
        })
      }

      observe({
        if (input$stopButton > 0){
          stopApp(returnValue = static(mod()))
        }
      })
      
      mod <- reactive({
        lc <- as.list(x$call)
        lc$outlier.critical <- input$outlier.critical    
        lc$arima.model <- input$model
        if (input$method == "x11"){
          lc$x11 = ""
        } else {
          lc$x11 = NULL
        }
        if (is.null(input$aictest)){
          lc['regression.aictest'] <- input$aictest
          names(lc['regression.aictest']) <- "regression.aictest"
        } else {
          lc$regression.aictest <- input$aictest
        }
        z <- eval(as.call(lc))
        if (!is.null(z$spc$seats)){
          updateRadioButtons(session, "method",
                             selected = "seats")
           }
        if (!is.null(z$spc$x11)){
          updateRadioButtons(session, "method",
                             selected = "x11")
        }
        z
      })
      
      output$modelSummary <- renderPrint({
        summary(mod())
      }) 
      
      SeriesRun <- reactive({
        mod <- mod()
        z <- list()
        z$dta <- try(series(mod, input$userview, verbose = FALSE),  silent = TRUE)
        if (inherits(z$dta, "try-error")){
          z$msg <- gsub("^.*:", "", z$dta)
          z$msg <- gsub("\n", "", z$msg)
          z$msg <- gsub("^\\s+|\\s+$", "", z$msg)
          z$dta <- NULL
          return(z)
        }
        if (is.null(z$dta)) {
          z$msg <- "no output has been generated by X13-ARIMA-SEATS."
        } else if (!inherits(z$dta, "ts") & 
                     !(input$userview %in% c("check.acf", "check.acfsquared", 
                                           "check.pacf", "identify.acf", 
                                           "identify.pacf"))
                   ) {
          z$msg <- "data is not a time series or another displayable data type."
        } else {
          z$msg <- NULL
        }
        z
      })
      
      output$moreText <- renderText({
        SeriesRun()$msg
      })
      
      output$morePlot <- renderPlot({
        dta <- SeriesRun()$dta
        if (inherits(dta, "ts")){
          nc <- NCOL(dta)
          ncol <- rainbow(nc)
          ts.plot(dta, col = ncol)
          if (nc > 1){
            legend("topleft", colnames(dta), lty = 1, col = ncol, bty = "n", horiz = TRUE)
          }
        } else if (input$userview %in% c("check.acf", "check.acfsquared", "check.pacf", "identify.acf", "identify.pacf")){
          plot(dta[,1:2], type = "l")
          lines(dta[,3], col = "red")
          lines(-dta[,3], col = "red")
        }
      })
      
      output$moreRepro <- renderText({
        paste('series(', as.character(icl$x), ', "', input$userview, '")', sep = "")
      })
      
      # server structure for user defined plots
      for (i in 1:length(vl)){
        expr <- paste0("output$vl", i, " <- renderPlot(vl[[",i ,  "]](mod()))")
        eval(parse(text = expr))
      }
    }
    
  ), launch.browser = launch.browser)
}
