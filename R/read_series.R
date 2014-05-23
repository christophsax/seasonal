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

  cbind(final, seasonal, seasonaladj, trend, irregular, adjustfac)
}


read_series <- function(file, frequency){
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

  dta <- apply(dta.raw[-1, ], 2, type.convert)
  time.raw <- as.numeric(dta[, 1])
    
  if (nchar(time.raw[1]) == 6){  # time series
    year <- substr(time.raw, start = 1, stop = 4)
    per <- substr(time.raw, start = 5, stop = 6)
    
    time <- as.numeric(year) + (as.numeric(per) - 1) / frequency
    z <- ts(dta[, -1], start = time[1], frequency = frequency)
    
    z[z==-999] <- NA   # usual NA code
    
  } else {
    z <- dta
    rownames(z) <- NULL
  }
  z
}

