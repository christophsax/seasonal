
read_est <- function(file){
  
  est <-  readLines(paste0(file, ".est"))
  
  dollars <- which(str_detect(est, "\\$"))
  
  z <- list()
  
  reg.start <- which(str_detect(est, "\\$regression\\$estimates"))
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
  

  arima.start <- which(str_detect(est, "\\$arima\\$estimates"))
  if (length(arima.start) > 0){
    arima.end <- dollars[which(dollars==arima.start) + 1] - 1
    z$arima <- read.table(text = est[(arima.start + 1):arima.end], sep = "\t", 
                          header = TRUE, stringsAsFactors = FALSE)[-1,]

    arima.names <- paste(z$arima$operator, z$arima$factor, 
                                z$arima$period, sep = "-")
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
  
  variance.start <- which(str_detect(est, "\\$variance"))
  variance.end <- length(est)
  
  z$variance <- read.table(text = est[(variance.start + 2):variance.end], 
                           stringsAsFactors = FALSE, sep = "\t")
  z$coefficients <- c(reg.coef, arima.coef)
  z$se <- c(reg.se, arima.se)
  
  z
}



read_lks <- function(file){
  lks.raw <-  readLines(paste0(file, ".lks"))[-1]
  lks.split <- str_split(lks.raw, "\\s+")
  
  lks <- do.call(rbind, lks.split)
  z <- as.numeric(lks[,2])
  names(z) <- lks[,1]
  z
}