#' @export
ReadData <- function(method = "seats", file, output = FALSE){

  # currently extracted final series (SEATS / X11)
  
  # seasonal        s10 final SEATS seasonal component               d10 final seasonal factors
  # seasonaladj     s11 final SEATS seasonal adjustment component    d11 final seasonally adjusted series
  # trend           s12 final SEATS trend component                  d12 final trend-cycle
  # irregular       s13 final SEATS irregular component              d13 final irregular component
  # adjustfac       s16 final SEATS combined adjustment factors      d16 combined seasonal and trading day factors
  # adjustmentratio s18 final SEATS adjustment ratio                 e18 final adjustment ratios (original series / seasonally adjusted series)
  
  z <- list()
  if (identical(method, "seats")){
    z$seasonal        <- as.numeric(read.table(paste0(file, ".s10"), stringsAsFactors = F)[-c(1,2) ,2])
    z$seasonaladj     <- as.numeric(read.table(paste0(file, ".s11"), stringsAsFactors = F)[-c(1,2) ,2])
    z$trend           <- as.numeric(read.table(paste0(file, ".s12"), stringsAsFactors = F)[-c(1,2) ,2])
    z$irregular       <- as.numeric(read.table(paste0(file, ".s13"), stringsAsFactors = F)[-c(1,2) ,2])
    z$adjustfac       <- as.numeric(read.table(paste0(file, ".s16"), stringsAsFactors = F)[-c(1,2) ,2])
    z$adjustmentratio <- as.numeric(read.table(paste0(file, ".s18"), stringsAsFactors = F)[-c(1,2) ,2])
  } else if (identical(method, "x11")){
    z$seasonal        <- as.numeric(read.table(paste0(file, ".d10"), stringsAsFactors = F)[-c(1,2) ,2])
    z$seasonaladj     <- as.numeric(read.table(paste0(file, ".d11"), stringsAsFactors = F)[-c(1,2) ,2])
    z$trend           <- as.numeric(read.table(paste0(file, ".d12"), stringsAsFactors = F)[-c(1,2) ,2])
    z$irregular       <- as.numeric(read.table(paste0(file, ".d13"), stringsAsFactors = F)[-c(1,2) ,2])
    z$adjustfac       <- as.numeric(read.table(paste0(file, ".d16"), stringsAsFactors = F)[-c(1,2) ,2])
    z$adjustmentratio <- as.numeric(read.table(paste0(file, ".e18"), stringsAsFactors = F)[-c(1,2) ,2])
  } else {
    stop("wrong method.")
  }
  z <- as.data.frame(z)
  
  if (file.exists(paste0(file, ".saa"))){
    z$final <- as.numeric(read.table(paste0(file, ".saa"), stringsAsFactors = F)[-c(1,2) ,2])
  } else {
    z$final <- z$seasonaladj
  }
  
  z
}