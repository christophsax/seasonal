# === functions used by shiny app ==============================================

# modified print method for website
PrintSummarySeas <- function (x, digits = max(3, getOption("digits") - 3), 
                              signif.stars = getOption("show.signif.stars"), ...) {
  
  # cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n",
  #     sep = "")
  
  if (is.null(coef(x))) {
    cat("No Coefficients\n")
  } else {
    cat("Coefficients:\n")
    coefs <- coef(x)
    printCoefmat(coefs, digits = digits, signif.stars = signif.stars,
                 na.print = "NA")
  }
  
  cat("\n")
  if (!is.null(x$spc$seats)){
    cat("SEATS adj.")
  }
  if (!is.null(x$spc$x11)){
    cat("X11 adj.")
  }
  
  cat("  ARIMA:", x$model$arima$model)

  cat("  Obs.:", formatC(x$lks['nobs'], format = "d"))
  cat("  Transform:", x$transform.function)
  cat("\nAICc:", formatC(x$lks['Aicc'], digits = digits))
  cat(", BIC:", formatC(x$lks['bic'], digits = digits))

  # QS Test
  qsv <- qs(x)[c('qssadj'), ]
  qsstars <- symnum(as.numeric(qsv['p-val']), 
                    corr = FALSE, na = FALSE, legend = FALSE,
                    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                    symbols = c("***", "**", "*", ".", " "))
  cat("  QS (no seasonality in final):", formatC(as.numeric(qsv['qs']), digits = digits)," ", qsstars, sep = "")
  
  if (!is.null(x$resid)){
    # Box Ljung Test
    bltest <- Box.test(x$resid, lag = 24, type = "Ljung")
    blstars <- symnum(bltest$p.value, 
                      corr = FALSE, na = FALSE, legend = FALSE,
                      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                      symbols = c("***", "**", "*", ".", " "))
    cat("\nBox-Ljung (no autocorr.):", 
        formatC(bltest$statistic, digits = digits), blstars)
    
    # Normality
    swtest <- shapiro.test(x$resid)
    swstars <- symnum(swtest$p.value, 
                      corr = FALSE, na = FALSE, legend = FALSE,
                      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                      symbols = c("***", "**", "*", ".", " "))
    cat(" Shapiro (normality):", formatC(swtest$statistic, digits = digits), swstars)
  }
  if (is.null(x$err)){
    cat("\n")
    print(x$err)
  }
  invisible(x)
}

format_seascall <- function(x){
  stopifnot(inherits(x, "call"))
  if (deparse(x[[1]]) != "seas"){
    stop('format_seascall() can only be applied to calls of the seas()')
  }
  # call in which all arguments are specified by their full names
  x <- match.call(definition = seas, x)
  xl <- as.list(x)
  z <- list()
  for (i in 2:length(xl)){
    z[[i - 1]] <- paste(names(xl)[i], "=", deparse(xl[[i]]))
  }
  argstr <- do.call(paste, c(z, sep = ",\n"))
  z <- paste("seas(", argstr, ")", sep = "\n")
  z
}



IsCallSave <- function(cl){
  fun.allowed <- c("window", "genhol", "ts", "seas", "c", "list", "-", "+", "*", "/")


  # does the call have subcalls, if not, it is save
  is.scl <- sapply(cl, class) == "call"
  if (all(!(is.scl))){
    return(TRUE)
  }
  # if it has subcalls, they must be further analyzed
  scl <- cl[is.scl]
  scl.names <- unlist(lapply(scl, function(e) as.character(e[[1]])))
  if (!all(scl.names %in% fun.allowed)){
    not.allowed <- scl.names[!scl.names %in% fun.allowed]
    return(FALSE)
  } else {
    rr <- unlist(lapply(scl, IsCallSave))
    if (all(rr)){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}


EvalOrFail <- function(cstr){
  pcl <- try(parse(text = cstr))
  if (inherits(pcl, "try-error")){
    z <- "Invalid call."
    class(z) <- "try-error"
  } else if (inherits(try(as.call(pcl)[[1]][[1]], silent = TRUE), "try-error")){
    z <- "Invalid call."
    class(z) <- "try-error"
  } else if (as.call(pcl)[[1]][[1]] != "seas"){
    z <- "Only calls to seas() are allowed."
    class(z) <- "try-error"
  } else if (!IsCallSave(as.call(pcl)[[1]])){
    z <- "Call is not save and thus not allowed."
    class(z) <- "try-error"
  } else {
    z <- try(eval(pcl), silent = TRUE)
  }
  z
}




AddSeriesToCall <- function(cl, series){
  SP <- SPECS[SPECS$long == series, ]

  lcl <- as.list(cl)

  # add save arg
  lcl[[paste0(SP$spec, ".save")]] <- SP$short
  
  # add requirements if secifed
  if (SP$requires != ""){
    rl <- eval(parse(text = paste("list(", SP$requires, ")")))
    # ignore requirements that are already fulfilled
    rl <- rl[!names(rl) %in% names(lcl)]
    if (length(rl) > 0){
      lcl <- c(lcl, rl)
    }
  }

  as.call(lcl)
}


GetFOpts <- function(x){
 if (missing(x)){
  return(list(method = "user", 
              transform = "user", 
              arima = "user",
              outlier = "user",
              easter = "user", 
              td = "user"))
 } 

 z <- list()

  # method
  if (!is.null(x$spc$x11)){
    z$method <- "X11"
  } else {
    z$method <- "SEATS"
  }

  # transform
  if (x$spc$transform$`function` %in% c("log", "auto", "none", "sqrt")){
    z$transform <- x$spc$transform$`function`
  } else {
    z$transform <- "user"
  }

  # arima
  if (!is.null(x$spc$automdl)) {
    z$arima <- "auto"
  } else {
    if (!is.null(x$model$arima$model)){
      z$arima <- x$model$arima$model
    } else {
      z$arima <- "user"
    }
  }

  # outlier
  if (!is.null(x$spc$outlier)) {
    if (!is.null(x$spc$outlier$critical)){
      if (x$spc$outlier$critical %in% c(3, 4, 5)){
        z$outlier <- paste0("cv", x$spc$outlier$critical)
      } else {
        z$outlier <- "user"
      }
    } else {
      z$outlier <- "auto"
    }
  } else {
     z$outlier <- "none"
  }
  
  # easter and td: common preparations
  if (!is.null(x$spc$regression$aictest)){
     aic <- x$spc$regression$aictest
  } else {
     aic <- ""
  }
  
  if (!is.null(x$spc$regression$variables)){
     v <- x$spc$regression$variables
  } else {
     v <- ""
  }
  
  # easter
  g <- grepl("easter[", x$spc$regression$variables, fixed = TRUE)

  if (sum(g) > 1){
    z$easter <- "user"
  } else if ("easter" %in% aic & sum(g) == 0){
    z$easter <- "easter.aic"
  } else if (!"easter" %in% aic & sum(g) == 0 %in% v){
    z$easter <- "none"
  } else if (!"easter" %in% aic & "easter[1]" %in% v){
    z$easter <- "easter[1]"
  } else if (!"easter" %in% aic & "easter[8]" %in% v){
    z$easter <- "easter[8]"
  } else if (!"easter" %in% aic & "easter[15]" %in% v){
    z$easter <- "easter[15]"
  } else {
    z$easter <- "user"
  }

  if (z$easter == "none" & isTRUE(x$spc$regression$usertype == "holiday")){
    if (isTRUE(try(as.character(x$call$xreg[[1]]) == "genhol"))){
      if (x$call$xreg$start == 0 & x$call$xreg$end == 0 & x$call$xreg$center == "calendar"){
        if (x$call$xreg[[2]] == "cny"){
          z$easter <- "cny"
        } else if (x$call$xreg[[2]] == "diwali"){
          z$easter <- "diwali"
        } else {
          z$easter <- "user"
        }
      } else {
        z$easter <- "user"
      }
    }
  }

  g <- grepl("td", x$spc$regression$variables, fixed = TRUE)

  if (sum(g) > 1){
    z$td <- "user"
  } else if ("td" %in% aic & sum(g) == 0){
    z$td <- "td.aic"
  } else if (!"td" %in% aic & sum(g) == 0 %in% v){
    z$td <- "none"
  } else if (!"td" %in% aic & "td1coef" %in% v){
    z$td <- "td1coef"
  } else if (!"td" %in% aic & "td" %in% v){
    z$td <- "td"
  } else {
    z$td <- "user"
  }

  stopifnot(length(z) == 6)
  z
}


AddFOpts <- function(x, FOpts){

  # call in which all arguments are specified by their full names
  lc <- as.list(match.call(definition = seas, x$call))

  # x$call
  # lc <- as.list(x$call)

  if (is.null(FOpts$method)) FOpts$method <- "user"
  if (is.null(FOpts$transform)) FOpts$transform <- "user"
  if (is.null(FOpts$arima)) FOpts$arima <- "user"
  if (is.null(FOpts$outlier)) FOpts$outlier <- "user"
  if (is.null(FOpts$easter)) FOpts$easter <- "user"
  if (is.null(FOpts$td)) FOpts$td <- "user"

  # convert 'call' objects in ccharacter vectors
  # dont know why this is needed, but it seems to convert some lists into character vectors
  is.cl <- lapply(lc, class) == "call"

  # but not for genhol or window
  is.cl[is.cl] <- !sapply(lc[is.cl], function(e) as.character(e[[1]])) %in% c("window", "genhol")
  # browser()

  if (length(is.cl) > 0){
    lc[is.cl] <- lapply(lc[is.cl], function(e){as.character(e)[-1]})
  }

  if (FOpts$method == "X11"){
    lc$x11 <- ""
  } else if (FOpts$method == "SEATS"){
    lc$x11 <- NULL
    # lc$forecast.maxback <- NULL
    # lc$forecast.backcasts <- NULL
  }

  if (FOpts$transform == "auto"){
    lc$transform.function <- NULL
  } else if (FOpts$transform != "user"){
    lc$transform.function <- FOpts$transform
  }

  if (FOpts$arima == "auto"){
    lc$arima.model <- NULL
  } else if (FOpts$arima != "user"){
    lc$arima.model <- FOpts$arima
  } 

  if (FOpts$outlier == "auto"){
    lc$outlier <- NULL
    lc$outlier.critical <- NULL
  } else if (FOpts$outlier == "none"){
    lc['outlier'] <- NULL
    lc$outlier.critical <- NULL
    names(lc['outlier']) <- "outlier"
  } else if (FOpts$outlier != "user"){
    lc$outlier.critical <- as.numeric(substr(FOpts$outlier, 3, 3))
  } 

  if (FOpts$easter %in% c("cny", "diwali")){
    lc$xreg <- as.call(parse(text = paste0('genhol(', FOpts$easter,', start = 0, end = 0, center = "calendar")')))[[1]]
    lc$regression.usertype = "holiday"
    FOpts$easter <- "none"
  } else if (FOpts$easter != "user"){
    lc$xreg <- NULL
    lc$regression.usertype = NULL
  }

  if (FOpts$easter %in% c("easter[1]", "easter[8]", "easter[15]", "none")){
    g <- grepl("easter[", lc$regression.variables, fixed = TRUE)
    if (sum(g) > 0){
      lc$regression.variables <- lc$regression.variables[!g]
    }
    if (FOpts$easter != "none"){
      lc$regression.variables <- union(lc$regression.variables, FOpts$easter)
    }

    if ("regression.aictest" %in% names(lc)){ # non default, specified
      lc$regression.aictest <- setdiff(lc$regression.aictest, "easter")
      if (length(lc$regression.aictest) == 0){
        lc['regression.aictest'] <- NULL
        names(lc['regression.aictest']) <- "regression.aictest"
      }
    } else {
      lc$regression.aictest <- "td"
    }

  } else if (FOpts$easter == "easter.aic") {
    g <- grepl("easter[", lc$regression.variables, fixed = TRUE)
    if (sum(g) > 0){
      lc$regression.variables <- lc$regression.variables[!g]
    }
    if (identical(lc$regression.aictest, "td")){
      # set default settings
      lc$regression.aictest <- NULL
    } else if ("regression.aictest" %in% names(lc)){ # non default, specified
      lc$regression.aictest <- union(lc$regression.aictest, "easter")
    }
  }

  if (FOpts$td %in% c("td", "td1coef", "none")){
    g <- grepl("td", lc$regression.variables)
    if (sum(g) > 0){
      lc$regression.variables <- lc$regression.variables[!g]
    }
    if (FOpts$td != "none"){
      lc$regression.variables <- union(lc$regression.variables, FOpts$td)
    }

    if ("regression.aictest" %in% names(lc)){ # non default, specified
      lc$regression.aictest <- setdiff(lc$regression.aictest, "td")
      if (length(lc$regression.aictest) == 0){
        lc['regression.aictest'] <- NULL
        names(lc['regression.aictest']) <- "regression.aictest"
      }
    } else {
      lc$regression.aictest <- "easter"
    }

  } else if (FOpts$td == "td.aic") {
    g <- grepl("td", lc$regression.variables)
    if (sum(g) > 0){
      lc$regression.variables <- lc$regression.variables[!g]
    }
    if (identical(lc$regression.aictest, "easter")){
      # set default settings
      lc$regression.aictest <- NULL
    } else if ("regression.aictest" %in% names(lc)){ # non default, specified
      lc$regression.aictest <- union(lc$regression.aictest, "td")
    }
  }

  if (length(lc$regression.variables) == 0){
    lc$regression.variables <- NULL
  }

  as.call(lc)
}




# === Session intitialization ==================================================

library(seasonal)
data(holiday)

load(file = file.path(system.file("inspect", package="seasonal"), "specs.rdata"))

inter.session.file <- paste0(gsub("[a-zA-Z0-9]+$", "", tempdir()), "intersession.RData")


load(file = inter.session.file)
init.icstr <- format_seascall(init.model$call)
print(init.icstr)





# --- List with views ----------------------------------------------------------


lSeries <- list()
lSeries$MAIN <- c("Original and Adjusted Series" = "main", 
                  "Original and Adjusted Series (%)" = "mainpc", 
                  "SI-ratio" = "monthplot")

SPECS2 <- SPECS[SPECS$seats, ]
SPECS2$long <- gsub("seats.", "", SPECS2$long)
SPECS2$spec <- gsub("seats", "seats/x11", SPECS2$spec)

sp <- unique(SPECS2$spec)
for (spi in sp){
  argi <- SPECS2[SPECS2$spec == spi, ]$long
  names(argi) <- SPECS2[SPECS2$spec == spi, ]$descr
  lSeries[[toupper(spi)]] <- argi
}


# --- User defined views -------------------------------------------------------

# List with user definded functions
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


# --- List with options --------------------------------------------------------

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


