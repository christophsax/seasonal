#' @export
seas <- function(x, method = "seats", ...){
  # 1. Gen
  # 2. Run
  # 3. Read
  
  spc <- GenSPC(x, name = deparse(substitute(x)), method = method)
  spc <- ModSPC(spc, ...)
  spc <- EnsureConsistencySPC(spc)
  RunSPC(spc)

  z <- ReadX13(method = method)
  
  z$spc <- spc
  
  # last transformations...
  z$data$original <- x
  z$data <- ts(z$data, start = start(x), frequency = frequency(x))
  z$call <- match.call()
  class(z) <- "seas"
  z
}


#' @export
GenSPC <- function(x, name, method = "seats", 
              transform.function = "auto",
              regression.aictest = c("td", "easter")
  ){
    
    stopifnot(inherits(x, "ts"))
    
    # --- construct spc list -----------------------------------------------------
    z <- list()
    
    z$series <- GenSPCSeries(x, name = name)
    z$transform$`function` <- transform.function
    z$regression$aictest <- regression.aictest
    z$automdl$savelog <- "automodel"
    
    
    z$estimate$save <- "model"
    z$outlier <- list()
    
    if (identical(method, "seats")){
      z$seats <- list(save = c("s10", "s11", "s12", "s13", "s16", "s18"))
    } else if (identical(method, "x11")){
      z$x11 <- list(save = c("d10", "d11", "d12", "d13", "d16", "e18"))
    } else {
      stop("wrong method.")
    }
    
    class(z) <- c("SPC", "list")
    z
}


#' @export
ModSPC <- function(x, ...){
  stopifnot(inherits(x, "SPC"))
  
  mod.list <- list(...)
  
  for (i in seq_along(mod.list)){
    content.i <- mod.list[[i]]
    names.i <- names(mod.list)[i]
    
    split.names.i <- strsplit(names.i, "\\.")
    
    stopifnot(length(split.names.i) == 1)
    
    spc.name <- split.names.i[[1]][1]
    
    if (is.null(x[[spc.name]])){
      x[[spc.name]] <- list()
    }
    
    if (length(split.names.i[[1]]) == 1){
      x[[spc.name]] <- content.i
    } else if (length(split.names.i[[1]]) == 2){
      spc.arg <- split.names.i[[1]][2]
      if (is.null(x[[spc.name]][[spc.arg]])){
        x[[spc.name]][[spc.arg]] <- list()
      }
      x[[spc.name]][[spc.arg]] <- content.i
    } else {
      stop("Arguments should contain a Spc and an optional Argument after the dot.")
    }
  }
  x
}

#' @export
EnsureConsistencySPC <-function(x){
  stopifnot(inherits(x, "SPC"))
  if (!is.null(x$automdl) & !is.null(x$arima)){
    x$automdl <- NULL
  }
  if (!is.null(x$force$type)){
    x$force$save <- "saa"
  }
  x
}


#' @export
RunSPC <- function(x, method = "seats"){
  # if everything works:
  # path <- system.file(package = "seasonal")
  path <- "C:/Users/seco-sxh/github/seasonal/inst/"
  spcfile <- "spcfile"
  
  stopifnot(inherits(x, "SPC"))
  
  unlink(paste0(path, "io/out"), recursive = TRUE)
  dir.create(paste0(path, "io/out"))
  WriteSPC(x, paste0(path, "io/", spcfile, ".spc"))
  shell(paste0(path, "x13/x13as.exe ", path, "io/", spcfile, " ", path, "io/out/test"))
}




#' @export
GenSPCSeries <- function(x, name = "series", start = NULL, end = NULL){
  stopifnot(inherits(x, "ts"))
  
  z <- list()
  z$title <- paste0("\"", name, "\"")
  z$start <- paste0(start(x)[1], ".", cycle(x)[1])
  z$data <- as.numeric(x)
  z
}