#' @export
seas <- function(x, method = "seats"){
  # if everything works:
  # path <- system.file(package = "seasonal")
  path <- "C:/Users/seco-sxh/github/seasonal/inst/"
  spcfile <- "spcfile"
  
  stopifnot(inherits(x, "ts"))
  
  # --- construct spc list -----------------------------------------------------
  spc <- list()
  
  spc$series <- SubSeries(x, name = deparse(substitute(x)))
  spc$transform$`function` <- "auto"
  spc$regression$aictest <- c("td", "easter")
  spc$automdl$savelog <- "automodel"
  spc$estimate$save <- "model"
  spc$outlier <- list()
  
  if (identical(method, "seats")){
    spc$seats <- list(save = c("s10", "s11", "s12", "s13", "s16", "s18"))
  } else if (identical(method, "x11")){
    spc$x11 <- list(save = c("d10", "d11", "d12", "d13", "d16", "e18"))
  } else {
    stop("wrong method.")
  }
  
  # --- run x13 ----------------------------------------------------------------
  unlink(paste0(path, "io/out"), recursive = TRUE)
  dir.create(paste0(path, "io/out"))
  WriteSPC(spc, paste0(path, "io/", spcfile, ".spc"))
  shell(paste0(path, "x13/x13as.exe ", path, "io/", spcfile, " ", path, "io/out/test"))
  
  # --- read data --------------------------------------------------------------
  z <- ReadX13(method = method, path = path)
  z$data$original <- x
  z$data <- ts(z$data, start = start(x), frequency = frequency(x))
  class(z) <- "seas"
  z
}




#' @export
SubSeries <- function(x, name = "series", start = NULL, end = NULL){
  stopifnot(inherits(x, "ts"))
  
  z <- list()
  z$title <- paste0("\"", name, "\"")
  z$start <- paste0(start(x)[1], ".", cycle(x)[1])
  z$data <- as.numeric(x)
  z
}