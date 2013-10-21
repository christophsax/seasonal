#' @export
ReadX13 <- function(method = "seats", name = "test", 
                    path = "C:/Users/seco-sxh/github/seasonal/inst/"){

  # currently extracted final series (SEATS / X11)
  
  # seasonal        s10 final SEATS seasonal component               d10 final seasonal factors
  # seasonaladj     s11 final SEATS seasonal adjustment component    d11 final seasonally adjusted series
  # trend           s12 final SEATS trend component                  d12 final trend-cycle
  # irregular       s13 final SEATS irregular component              d13 final irregular component
  # adjustfac       s16 final SEATS combined adjustment factors      d16 combined seasonal and trading day factors
  # adjustmentratio s18 final SEATS adjustment ratio                 e18 final adjustment ratios (original series / seasonally adjusted series)
  
  z <- list()
  if (identical(method, "seats")){
    z$data$seasonal        <- as.numeric(read.table(paste0(path, "io/out/", name, ".s10"), stringsAsFactors = F)[-c(1,2) ,2])
    z$data$seasonaladj     <- as.numeric(read.table(paste0(path, "io/out/", name, ".s11"), stringsAsFactors = F)[-c(1,2) ,2])
    z$data$trend           <- as.numeric(read.table(paste0(path, "io/out/", name, ".s12"), stringsAsFactors = F)[-c(1,2) ,2])
    z$data$irregular       <- as.numeric(read.table(paste0(path, "io/out/", name, ".s13"), stringsAsFactors = F)[-c(1,2) ,2])
    z$data$adjustfac       <- as.numeric(read.table(paste0(path, "io/out/", name, ".s16"), stringsAsFactors = F)[-c(1,2) ,2])
    z$data$adjustmentratio <- as.numeric(read.table(paste0(path, "io/out/", name, ".s18"), stringsAsFactors = F)[-c(1,2) ,2])
  } else if (identical(method, "x11")){
    z$data$seasonal        <- as.numeric(read.table(paste0(path, "io/out/", name, ".d10"), stringsAsFactors = F)[-c(1,2) ,2])
    z$data$seasonaladj     <- as.numeric(read.table(paste0(path, "io/out/", name, ".d11"), stringsAsFactors = F)[-c(1,2) ,2])
    z$data$trend           <- as.numeric(read.table(paste0(path, "io/out/", name, ".d12"), stringsAsFactors = F)[-c(1,2) ,2])
    z$data$irregular       <- as.numeric(read.table(paste0(path, "io/out/", name, ".d13"), stringsAsFactors = F)[-c(1,2) ,2])
    z$data$adjustfac       <- as.numeric(read.table(paste0(path, "io/out/", name, ".d16"), stringsAsFactors = F)[-c(1,2) ,2])
    z$data$adjustmentratio <- as.numeric(read.table(paste0(path, "io/out/", name, ".e18"), stringsAsFactors = F)[-c(1,2) ,2])
  } else {
    stop("wrong method.")
  }
  z$data <- as.data.frame(z$data)
  
  if (file.exists(paste0(path, "io/out/", name, ".saa"))){
    z$data$final <- as.numeric(read.table(paste0(path, "io/out/", name, ".saa"), stringsAsFactors = F)[-c(1,2) ,2])
  } else {
    z$data$final <- z$data$seasonaladj
  }
  
  z$mdl <- ReadSPC(paste0(path, "io/out/", name, ".mdl"))
  
  # keep arima models as a single string, 
  # transform other SPC vectors to R vectors
  for (i in seq_along(z$mdl)){
    z$mdl[[i]] <- as.list(z$mdl[[i]])
    for (j in seq_along(z$mdl[[i]])){
      if (names(z$mdl[[i]])[j] != "model"){
        z$mdl[[i]][[j]] <- CleanElement(z$mdl[[i]][[j]])
      } else {
        z$mdl[[i]][[j]] <- str_trim(z$mdl[[i]][[j]])
      }
    }    
  }

  
  z
}