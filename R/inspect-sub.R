
PrintSummarySeas <- function(x, digits = max(3, getOption("digits") - 3), 
                              signif.stars = getOption("show.signif.stars"), ...) {
  
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
  if (!is.null(x$err)){
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
    z[[i - 1]] <- paste(names(xl)[i], "=", deparse(xl[[i]], width.cutoff = 500))
  }
  argstr <- do.call(paste, c(z, sep = ",\n"))
  z <- paste("seas(", argstr, ")", sep = "\n")
  z
}


# no limits on local machines
IsCallSave <- function(cl){
  TRUE
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
    z <- try(eval(pcl, envir = globalenv()), silent = TRUE)
  }
  z
}




AddSeriesToCall <- function(cl, series, INSPDATA){
  SP <- INSPDATA[INSPDATA$long == series, ]

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
  
  # easter and td: common preparations
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
  
  # easter
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
    if (inherits(x$call$xreg, "name")){
      z$easter <- "user"
    } else if (isTRUE(try(as.character(x$call$xreg[[1]]) == "genhol"))){
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
    } else if (!is.null(x$call$xreg)) {
      z$easter <- "user"
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

  if (is.null(FOpts$method)) FOpts$method <- "user"
  if (is.null(FOpts$transform)) FOpts$transform <- "user"
  if (is.null(FOpts$arima)) FOpts$arima <- "user"
  if (is.null(FOpts$outlier)) FOpts$outlier <- "user"
  if (is.null(FOpts$easter)) FOpts$easter <- "user"
  if (is.null(FOpts$td)) FOpts$td <- "user"

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

  # calls to not work well with union, so covert them to character before
  C2C <- function(x){
    eval(parse(text = deparse(x)))
  }

  if (FOpts$easter %in% c("easter[1]", "easter[8]", "easter[15]", "none")){
    g <- grepl("easter[", lc$regression.variables, fixed = TRUE)
    if (sum(g) > 0){
      lc$regression.variables <- lc$regression.variables[!g]
    }
    if (FOpts$easter != "none"){
      lc$regression.variables <- union(C2C(lc$regression.variables), FOpts$easter)
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
      lc$regression.aictest <- union(C2C(lc$regression.aictest), "easter")
    }
  }

  if (FOpts$td %in% c("td", "td1coef", "none")){
    g <- grepl("td", lc$regression.variables)
    if (sum(g) > 0){
      lc$regression.variables <- lc$regression.variables[!g]
    }
    if (FOpts$td != "none"){
      lc$regression.variables <- union(C2C(lc$regression.variables), FOpts$td)
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
      lc$regression.aictest <- union(C2C(lc$regression.aictest), "td")
    }
  }

  if (length(lc$regression.variables) == 0){
    lc$regression.variables <- NULL
  }

  as.call(lc)
}







err_to_html <- function(txt){
  # format error message as html 

  # e1 <- 'Error: unexpected \')\' in "ts(dsfasdfasdfa))"'
  # e2 <- "Error in is.data.frame(data) : object 'dsfasdfasdfa' not found"
  # e3 <- "Error: X-13 run failed, with the following message(s):\n\nErrors:\n- dregression is not a valid spec name.\n\n"

  # err_to_html(e1)
  # err_to_html(e2)
  # err_to_html(e3)

  stopifnot(length(txt) == 1)

  if (grepl("\\n\\n", txt)){
    
    # seaprate title and body
    rm <- gregexpr("^.*?\\n\\n", txt)
    title <- regmatches(txt, rm)[[1]]
    title <- paste("<p>", gsub("\\n", "", title), "</p>")
    body0 <- regmatches(txt, rm, invert = TRUE)[[1]]
    body0 <- body0[body0 != ""]


    # separte in Errors, Warnings, Notes

    tp <- c("Notes:.+$", "Warnings:.+$", "Errors:.+$")
    names(tp) <- c("note", "warning", "error")

    body1 <- list()
    for (i in 1:length(tp)){
      rm <- gregexpr(tp[i], body0)
      body1[[names(tp)[i]]] <- regmatches(body0, rm)[[1]]
      body0 <- regmatches(body0, rm, invert = TRUE)[[1]]
    }

    # format as html
    body2 <- strsplit(unlist(body1), "\n- ")

    # browser()
    body3 <- lapply(body2, function(e) gsub("\\n", "", e))

    bullet_to_html <- function(x) {
      paste0("<p>", x[1], "</p>", "<ul>", paste(paste("<li>", x[-1], "</li>"), collapse = " "), "</ul>")
    }

    body <- paste(unlist(lapply(body3, bullet_to_html)), collapse = " ")

    z <- paste(title, body)

  } else {
    body <- paste0("<p>", txt, "</p>")
    z <- paste(body)
  }

  z
}



