InspectServer <- function(input, output) {
  # --- reactive model estimation ----------------------------------------------

  require(shiny)

  # reactive values
  rUploadUpd <- reactiveValues(upd = 0)  
  rTerminalUpd <- reactiveValues(upd = 0)  
  rPlotUpd <- reactiveValues(upd = 0)  
  rTerminalError <- reactiveValues(upd = 0)   
  rModel <- reactiveValues() 
  rFOpts <- list()

  gLastView <- "main"
  gFiveBestMdl <- structure(list(arima = c("(0 1 0)(0 1 1)", "(1 1 1)(0 1 1)", "(0 1 1)(0 1 1)", "(1 1 0)(0 1 1)", "(0 1 2)(0 1 1)"), bic = c(-4.007, -3.986, -3.979, -3.977, -3.97)), .Names = c("arima", "bic"), row.names = c(NA, -5L), class = "data.frame")

  rSelectUpd <- reactiveValues(upd = 0)  

  # --- initialisazion ---------------------------------------------------------
  rModelCall  <- reactiveValues(cstr = init.icstr)
  gTerminalCall <- init.icstr

  # model evaluation, updated by rModelCall$cstr or rUploadUpd$upd
  observe({
    cstr <- rModelCall$cstr
    z <- EvalOrFail(cstr)
    if (inherits(z, "try-error")){
      rTerminalError$error <- z

    } else {

      if (!is.null(isolate(rTerminalError$error))){
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
      rTerminalUpd$upd <- isolate(rTerminalUpd$upd) + 1
    }
  })

  # --- auto input elements ----------------------------------------------------
  output$oFOpts <- renderUI({
    fopts <- GetFOpts(rModel$m)

    # update if new fivebestmdl are available, otheœrwise, use last fivebestmdl
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
      selectInput("iMethod", "Adjustment Method", choices = lFOpts2$method, selected = fopts$method, width = '100%'),
      selectInput("iTransform", "Prior-Transformation", choices = lFOpts2$transform, selected = fopts$transform, width = '100%'),
      selectInput("iArima", "Arima Model", choices = lFOpts2$arima, selected = fopts$arima, width = '100%'),
      selectInput("iOutlier", "Outlier", choices = lFOpts2$outlier, selected = fopts$outlier, width = '100%'),
      selectInput("iEaster", "Holiday", choices = lFOpts2$easter, selected = fopts$easter, width = '100%'),
      selectInput("iTd", "Trading Days", choices = lFOpts2$td, selected = fopts$td, width = '100%')
      )
  })

  output$oFOpts <- renderUI({
    fopts <- GetFOpts(rModel$m)

    # update if new fivebestmdl are available, otheœrwise, use last fivebestmdl
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
      selectInput("iMethod", "Adjustment Method", choices = lFOpts2$method, selected = fopts$method, width = '100%'),
      selectInput("iTransform", "Pre-Transformation", choices = lFOpts2$transform, selected = fopts$transform, width = '100%'),
      selectInput("iArima", "Arima Model", choices = lFOpts2$arima, selected = fopts$arima, width = '100%'),
      selectInput("iOutlier", "Outlier", choices = lFOpts2$outlier, selected = fopts$outlier, width = '100%'),
      selectInput("iEaster", "Holiday", choices = lFOpts2$easter, selected = fopts$easter, width = '100%'),
      selectInput("iTd", "Trading Days", choices = lFOpts2$td, selected = fopts$td, width = '100%')
      )
  })

  # auto update
  observe({ 
    rFOpts$method <- (input$iMethod)
    rFOpts$transform <- (input$iTransform)
    rFOpts$arima <- (input$iArima)
    rFOpts$outlier <- (input$iOutlier)
    rFOpts$easter <- (input$iEaster)
    rFOpts$td <- (input$iTd)

    if (is.null(isolate(rModel$m))){
      m <- x
    } else {
      m <- isolate(rModel$m)
    }

    cl <- AddFOpts(m, isolate(rFOpts))
    cstr <- format_seascall(cl)

    gTerminalCall <<- cstr

    # assignment will trigger reevaluation
    rModelCall$cstr <- cstr
   })

  # terminal update
  observe({ 
    if (input$iEvalCall > 0){
      # assignment will trigger reevaluation
      rModelCall$cstr <- isolate(input$iTerminal)

    }
   })

  observe({ 
    if (input$iOutput > 0){
      out(init.model)
    }
   })

  # --- manual input elements ----------------------------------------------------

  # display terminal, updated by rTerminalCall$cstr
  output$oTerminal <- renderUI({
    rTerminalUpd$upd
    cstr <- gTerminalCall
    # hell: chrome
    # sublime: monokai
    tags$textarea(id="iTerminal", class="form-control", rows=10, cols=60, cstr)
    # aceEditor("iTerminal", mode = "r", theme = "chrome", value = cstr, height = "160px")
  })

  #  display error message and revert button, updated by rCall$error
  output$oRevert <- renderUI({
    if(!is.null(rTerminalError$error)){
       pp <- HTML(paste0('<div class="alert alert-danger alert-dismissible fade in" role="alert">
        <button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>
        <h4>Error</h4>
        <p>', rTerminalError$error, '</p>
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
  observe({ 
    if (!is.null(input$iRevert)){
      if (input$iRevert > 0){
        # increasing the reactive variable will update the Terminal
        rTerminalUpd$upd <- isolate(rTerminalUpd$upd) + 1  
        rTerminalError$error <- NULL
      }
    }
   })


  # --- output: view -----------------------------------------------------------

  # view selector
  output$oSeries <- renderUI({
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
    selectInput("iSeries", NULL, choices = cc, selected = view)
  })

  # update call if required by view
  observe({ 
    iSeries <- input$iSeries
    if (!is.null(iSeries)){

      # no call updates for user defined view functions
      if (iSeries %in% names(lUserView)) {
        rPlotUpd$upd <- isolate(rPlotUpd$upd) + 1
        gLastView <<- iSeries
        return(NULL)
      }

      # remove existing save specs
      m <- isolate(rModel$m)
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

      cl <- as.call(lcl[!(names(lcl) %in% paste0(SPECS$spec, ".save"))])

      if (!iSeries %in% c("main", "mainpc", "monthplot")){ 
        if (iSeries %in% c("irregular", "seasonal", "trend")){
          iSeries <- paste0(tolower(input$iMethod), ".", iSeries)
        }
        cstr <- format_seascall(AddSeriesToCall(cl, iSeries))
        rModelCall$cstr <- cstr
      } # else {
      #   rPlotUpd$upd <- isolate(rPlotUpd$upd) + 1
      # }
      rPlotUpd$upd <- isolate(rPlotUpd$upd) + 1
    }
  })

  output$oMainPlot <- renderPlot({
    rPlotUpd$upd  # trigger on demand

    par(mar = c(3, 3, 1.7, 1.7) -1)


    iSeries <- isolate(input$iSeries)
    if (is.null(iSeries)) {iSeries <- gLastView}
    if (iSeries == "main") return(plot(rModel$m, main = ""))
    if (iSeries == "mainpc") return(plot(rModel$m, main = "", transform = "PC"))
    if (iSeries == "monthplot"){
      # instead of the seas method, to hide the main title
      monthplot(rModel$m$data[,'seasonal'], ylab = "", lwd = 2, col = "red", main = "")
      return(monthplot(seasonal:::siratio(rModel$m), col = "blue", type = "h", add = TRUE))
    }

    if (iSeries %in% c("irregular", "seasonal", "trend")){
      iSeries <- paste0(tolower(input$iMethod), ".", iSeries)
    } 

    if (iSeries %in% names(lUserView)){
      return(lUserView[[iSeries]](rModel$m))
    } 

    s <- series(rModel$m, iSeries, reeval = FALSE)
    validate(need(inherits(s, "ts"), "This view is not available for the model. Change view or model."))
    if (inherits(s, "ts")){
      return(ts.plot(s, ylab = ""))
    }

  })


  # --- output: other ----------------------------------------------------------

  output$oSummary <- renderPrint({
      PrintSummarySeas(summary(rModel$m))
  })

  # --- close and import -------------------------------------------------------

  shiny::observe({
    if (input$iClose > 0){
      shiny::stopApp(returnValue = isolate(rModel$m$call))
    }
  })
}

