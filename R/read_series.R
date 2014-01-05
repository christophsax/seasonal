read_data <- function(method = "seats", file){
  # read and parse the main data series
  # 
  # method  "seats" or "x11", series are in different files, depending on mehtod
  # file  full path without file ending
  #
  # return "mts" object


  if (identical(method, "seats")){
    seasonal        <- read_series(paste0(file, ".s10"))
    seasonaladj     <- read_series(paste0(file, ".s11"))
    trend           <- read_series(paste0(file, ".s12"))
    irregular       <- read_series(paste0(file, ".s13"))
    adjustfac       <- read_series(paste0(file, ".s16"))
  } else if (identical(method, "x11")){
    seasonal        <- read_series(paste0(file, ".d10"))
    seasonaladj     <- read_series(paste0(file, ".d11"))
    trend           <- read_series(paste0(file, ".d12"))
    irregular       <- read_series(paste0(file, ".d13"))
    adjustfac       <- read_series(paste0(file, ".d16"))
  } else {
    stop("wrong method.")
  }
    
  residuals <- read_series(paste0(file, ".rsd"))

  if (file.exists(paste0(file, ".saa"))){
    final <- read_series(paste0(file, ".saa"))
  } else {
    final <- seasonaladj
  }

  cbind(final, seasonal, seasonaladj, trend, irregular, adjustfac,
        residuals)
}


read_series <- function(file){
  # Read data from a particular X13-ARIMA-SEATS file
  # 
  # file  full path including file ending
  #
  # return a "ts" object
  
  dta.raw <- read.table(file, stringsAsFactors = F, sep = "\t", header = TRUE)
  dta <- apply(dta.raw[-1, ], 2, as.numeric)

  time.raw <- as.numeric(dta[, 1])
  
  year <- substr(time.raw, start = 1, stop = 4)
  per <- substr(time.raw, start = 5, stop = 6)
  
  if (length(unique(per)) == 4){
    frequency <- 4
  } else if (length(unique(per)) >= 5){
    frequency <- 12
  } else {
    warning("Frequency determination is unclear. Assuming monthly data.")
    frequency <- 12
  }
  
  time <- as.numeric(year) + (as.numeric(per) - 1) / frequency
  z <- ts(dta[, -1], start = time[1], frequency = frequency)
  z
}
