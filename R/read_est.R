read_est <- function(file){
  # read and parse a .est file
  # 
  # file  full path without file ending
  #
  # return a list with coefficients, standard errors and some statistics
  
  est <-  readLines(paste0(file, ".est"))
  if (length(est) == 0){
    return(NULL)
  }
  
  # lines on which a section (or subsection) starts
  dollars <- grep("\\$", est)

  z <- list()
  
  # regression section
  reg.start <- grep("\\$regression\\$estimates", est)
  if (length(reg.start) > 0){
    reg.end <- dollars[which(dollars==reg.start) + 1] - 1
    z$reg <- read.table(text = est[(reg.start + 1):reg.end], sep = "\t", 
                        header = TRUE, stringsAsFactors = FALSE)[-1,]
    
    reg.names <- z$reg$variable
    reg.coef <- as.numeric(z$reg$estimate)
    if (is.null(z$reg$standard.error)){
      reg.se <- rep(0, length(reg.coef))
    } else {
      reg.se <- as.numeric(z$reg$standard.error)
    }
    names(reg.coef) <- reg.names
    names(reg.se) <- reg.names
  } else {
    reg.coef <- NULL
    reg.se <- NULL
  }
  
  # arima section
  arima.start <- grep("\\$arima\\$estimates", est)
  if (length(arima.start) > 0){
    arima.end <- dollars[which(dollars==arima.start) + 1] - 1
    z$arima <- read.table(text = est[(arima.start + 1):arima.end], sep = "\t", 
                          header = TRUE, stringsAsFactors = FALSE)[-1,]

    arima.names <- paste(z$arima$operator, z$arima$factor, 
                                z$arima$lag, sep = "-")

    arima.coef <- as.numeric(z$arima$estimate)
    if (is.null(z$arima$standard.error)){
      arima.se <- rep(0, length(arima.coef))
    } else {
      arima.se <- as.numeric(z$arima$standard.error)
    }
    names(arima.coef) <- arima.names
    names(arima.se) <- arima.names
  } else {
    arima.coef <- NULL
    arima.se <- NULL
  }

  # variance / modelspan section
  variance.start <- grep("\\$variance", est)
  modelspan.start <- grep("\\$modelspan", est)
  if (length(modelspan.start) > 0){
    variance.end <- dollars[which(dollars == variance.start) + 1] - 1
  } else {
    variance.end <- length(est)
  }
  
  # output
  z$variance <- read.table(text = est[(variance.start + 2):variance.end], 
                           stringsAsFactors = FALSE, sep = "\t")
  
  z$coefficients <- c(reg.coef, arima.coef)
  z$se <- c(reg.se, arima.se)
  
  z
}


