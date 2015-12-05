read_data <- function(method = "seats", file, frequency){
  # read and parse the main data series
  # 
  # method  "seats" or "x11", series are in different files, depending on mehtod
  # file  full path without file ending
  #
  # return "mts" object

  if (identical(method, "seats")){
    seasonal        <- read_series(paste0(file, ".s10"), frequency = frequency)
    seasonaladj     <- read_series(paste0(file, ".s11"), frequency = frequency)
    trend           <- read_series(paste0(file, ".s12"), frequency = frequency)
    irregular       <- read_series(paste0(file, ".s13"), frequency = frequency)
    adjustfac       <- read_series(paste0(file, ".s16"), frequency = frequency)
  } else if (identical(method, "x11")){
    seasonal        <- read_series(paste0(file, ".d10"), frequency = frequency)
    seasonaladj     <- read_series(paste0(file, ".d11"), frequency = frequency)
    trend           <- read_series(paste0(file, ".d12"), frequency = frequency)
    irregular       <- read_series(paste0(file, ".d13"), frequency = frequency)
    adjustfac       <- read_series(paste0(file, ".d16"), frequency = frequency)
  } else {
    stop("wrong method.")
  }

  if (file.exists(paste0(file, ".saa"))){
    final <- read_series(paste0(file, ".saa"), frequency = frequency)
  } else {
    final <- seasonaladj
  }
  
  # remove NULL elements, because cbind cannot handle them
  ll <- list(final = final, seasonal = seasonal, seasonaladj = seasonaladj, trend = trend, irregular = irregular, adjustfac = adjustfac)
  lll <- ll[!unlist(lapply(ll, is.null))]
  do.call(cbind, lll)
}


read_series <- function(file, frequency = NULL){
  # Read data from a particular X13-ARIMA-SEATS file
  # 
  # file  full path including file ending
  #
  # return a "ts" object, NULL if no file is present
  
  if (!file.exists(file)){
    return(NULL)
  }

  if (grepl("\\.ipc$|\\.iac$", file)){  # exception handling for ipc and iac files
    dta.raw <- read.table(file, stringsAsFactors = F, sep = "\t", header = TRUE, fill = TRUE, skip = 2)
  } else {
    dta.raw <- read.table(file, stringsAsFactors = F, sep = "\t", header = TRUE, fill = TRUE)
  }

  # if not numeric, return as it is
  if (grepl("[a-zA-Z]", dta.raw[2, 1])){
    z <- dta.raw[-1, ]
    rownames(z) <- NULL
    return(z)
  }

  dta <- do.call("cbind", lapply(dta.raw[-1, ], type.convert))
  tt <- as.numeric(dta[, 1])
  dd <- dta[, -1, drop = FALSE]
    
  rownames(dd) <- NULL

  if (nchar(tt[1]) == 6){  # time series
    year <- substr(tt, start = 1, stop = 4)
    per <- substr(tt, start = 5, stop = 6)

    if (is.null(frequency)){
      frequency <- length(unique(as.numeric(per)))
    }
    
    if (dim(dd)[2] == 1L){
      dd <- c(dd)
    }
    time <- as.numeric(year) + (as.numeric(per) - 1) / frequency
    z <- ts(dd, start = time[1], frequency = frequency)
    z[z==-999] <- NA   # usual NA code
    
  } else if (grepl("^[12]\\d\\d\\d$", tt[1])){  # annual time series
    z <- ts(dd, start = tt[1], frequency = 1)
    z[z==-999] <- NA   # usual NA code
  } else {
    z <- dd
  }
  z
}

