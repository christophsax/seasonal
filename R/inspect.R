#' Interactively Inspect a Seasonal Adjustment Model (Experimental Shiny
#' Version)
#' 
#' Interactively inspect a \code{"seas"} object. The goal of \code{inspect} is
#' to summarize all relevant options, plots and statistics that should be
#' usually considered.
#' 
#' \bold{This version is based on shiny 0.9.1.9015. As shiny is still under
#' heavy development, this function may change considerably in the future.}
#' 
#' The \code{inspect} function opens an interactive window that allows for the 
#' manipulation of a number of arguments. It offers several views to analyze the
#' series graphically. With each change, the adjustment process and the 
#' visualizations are recalculated.
#' 
#' The views in \code{inspect} may be customized via the \code{fun} argument. 
#' One or several plot functions may be supplied. The plot functions should have
#' a \code{"seas"} object as their only argument. Several functions must be 
#' wrapped in a list (see examples).
#' 
#' @param x an object of class \code{"seas"}
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
#' @import shiny
inspect <- function(x, fun = NULL, 
                     launch.browser = if (Sys.getenv("RSTUDIO") == "1") rstudio::viewer else getOption("shiny.launch.browser", interactive())
){  
  
  require(shiny)
  # --- Initialize -------------------------------------------------------------
  
  data(specs)
#   
#   require(shiny)
  
  vl <- list()
  vl[['Series']] <- plot
  vl[['SI']] <- monthplot
  
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
  
  tab.expr <- 'mainPanel(tabsetPanel(
            tabPanel("Summary", verbatimTextOutput("someText")),'
  
  for (i in 1:length(vl)){
    tab.expr <- paste0(tab.expr, 'tabPanel("', names(vl)[i], '", plotOutput("vl', i, '")),\n')
  }
  
  tab.expr <- paste0(tab.expr, 'tabPanel("More", selectInput("userview", label = NULL, choices=  SPECS$long[SPECS$is.series])
,plotOutput("morePlot")), type = "pills"))')
  main.panel <- eval(parse(text = tab.expr))
  
  elements <- as.list(eval(static(x))$call)[-c(1:2)]
  
  elements <- paste(names(elements), "=", deparse(unlist(elements)))
  
  fb <- unique(c(x$model$arima$model, fivebestmdl(x)[,1]))
  fb.list <- as.list(fb)
  names(fb.list) <- fb

  ca.list <- list("trading days" = "td", "easter" = "easter")
  
  runApp(list(
    
    # --- UI -------------------------------------------------------------------
    
    
    ui = (fluidPage(
      
      
      # Application title
      titlePanel("seasonal: X13-ARIMA-SEATS interface"),
      
      # Sidebar with a slider input for number of bins
      sidebarLayout(
        sidebarPanel(
          # , inline = TRUE
          radioButtons("method", "Adjustment method:",
                       c("SEATS" = "seats",
                         "X-11" = "x11")),
          selectInput("model", "ARIMA Model:",
                      fb.list, selected = x$model$arima$model),
          selectInput("aictest", "AIC-test for:",
                      ca.list, selected = ca.list, multiple = TRUE),
          sliderInput("outlier.critical", "Critical outlier value", 2.5, 5, value = 4),
          checkboxInput("outBox", "Update X-13 Output"),
          actionButton("stopButton", "Close and import Call to R", icon = icon("download"))
        ),
        
        main.panel
      )
    )
    ),
    
    
    # --- Server ---------------------------------------------------------------
    
    server = function(input, output, session) {
      observe({
        if (input$outBox){
          if (getOption("htmlmode") == 1){
            out(mod())
          }
        }
      })
      
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
          lc$x11 = list()
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
      
      output$someText <- renderPrint({
        mod <- mod()
        summary(mod)
      }) 
      
#       output$distPlot <- renderPlot({
#         mod <- mod()
#         view <- plot
#         view(mod)
#       })
#       
      output$morePlot <- renderPlot({
        mod <- mod()
        dta <- series(mod, input$userview, verbose = FALSE)
        if (is.null(dta)) stop("no output generated.")
        
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
        } else if (grepl("spectrum", input$userview)){
          plot(dta[,-1], t = "l")
        } else {
          stop("data not displayable.")
        }

      })

      
      for (i in 1:length(vl)){
        expr <- paste0("output$vl", i, " <- renderPlot(vl[[",i ,  "]](mod()))")
        eval(parse(text = expr))
      }
      
      
      
    }
  ), launch.browser = launch.browser)
  
}
