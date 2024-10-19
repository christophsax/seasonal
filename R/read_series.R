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

  len <- nchar(file)
  suffix <- substr(file, start = len - 2, stop = len)

  if (suffix == "acm"){
    # exception to deal with #160
    return(read.table(file, stringsAsFactors = F, sep = "\t", header = TRUE, fill = TRUE)[-1, ])
  }

  if (suffix %in% c("ipc", "iac")){  # these files heave a header
    return(read_series_ipc_iac(file))
  } else {
    dta.raw <- read.table(file, stringsAsFactors = F, sep = "\t", header = TRUE, fill = TRUE)
  }

  # workaround for corrupted file #240
  if (suffix == "fts"){
    # only do if things seem corrupted
    if (all(is.na(dta.raw[, ncol(dta.raw)]))) {
      names(dta.raw) <- c(names(dta.raw)[-1], "drop")
      dta.raw <- subset(dta.raw, select = setdiff(names(dta.raw), "drop"))
      dta.raw <- cbind(Date = rownames(dta.raw), dta.raw)
      rownames(dta.raw) <- NULL
    }
  }

  # if not numeric, return as it is
  if (grepl("[a-zA-Z]", dta.raw[2, 1])){
    z <- dta.raw[-1, ]
    rownames(z) <- NULL
    return(z)
  }

  dta <- do.call("cbind", lapply(dta.raw[-1, ], type.convert, as.is = FALSE))
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
    if (unique(per)[1] == "00") {
      time_subperiod <- 0
    } else {
      time_subperiod <- (as.numeric(per) - 1) / frequency
    }
    stopifnot(time_subperiod >= 0)

    time <- as.numeric(year) + time_subperiod
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


# output of ipc and iac contains multiple tables, collect them in a long df
read_series_ipc_iac <- function(file) {
  raw0 <- read.table(file, stringsAsFactors = F, sep = "\t", header = FALSE, fill = TRUE)

  Lag <- NULL  # satisfy checks

  first_header <- raw0[which(grepl("Lag", raw0$V1, fixed = TRUE))[1], ]
  colnames(raw0) <- as.character(first_header)

  fill_down <- function(x) {
    ix <- seq_along(x)
    good <- !is.na(x)

    if (all(!good)) {
      return(x)
    }

    x_filled <- approx(
      x = ix[good],
      y = x[good],
      xout = ix,
      method = "constant",
      rule = 2,
      f = 0
    )$y

    if (is.factor(x)) {
      x_filled <- factor(x_filled, levels = levels(x))
    } else {
      mode(x_filled) <- mode(x)
    }

    return(x_filled)
  }

  extract_diff <- function(x, string) {
    z <- ifelse(
      grepl(string, x, fixed = TRUE),
      trimws(gsub(string, "", x, fixed = TRUE)),
      NA_real_
    )
    fill_down(z)
  }

  raw <- transform(raw0, diff = extract_diff(Lag, "$diff="))
  raw <- transform(raw, sdiff = extract_diff(Lag, "$sdiff="))

  to_drop <- grepl("Lag|\\-\\-\\-|\\$", raw$Lag)
  ans <- raw[!to_drop, ]
  ans[] <- lapply(ans, as.numeric)
  rownames(ans) <- NULL

  ans
}
