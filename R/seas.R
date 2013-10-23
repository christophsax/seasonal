#' @export
seas <- function(x, seats = list(), transform.function = "auto", 
                 regression.aictest = c("td", "easter"), outlier = list(), 
                 automdl = list(), 
                 save.out = FALSE, ...){
  
  # temporary working dir, x13 spc file is saved here, input is read from here.
  wdir <- tempdir() 
  
  ### 1. Gen
  spc <- list()
  class(spc) <- c("SPC", "list")
  
  # add data series
  spc$series <- SPCSeries(x, name = deparse(substitute(x))) 
  
  # add the default options
  spc$transform$`function` <- transform.function
  spc$regression$aictest <- regression.aictest
  spc$outlier <- outlier
  spc$automdl <- automdl
  spc$seats <- seats

  # add user defined options
  spc <- ModSPC(spc, ...)

  # remove double entries, adjust outputs
  spc <- EnsureConsistencySPC(spc)

  ### 2. Run
  RunSPC(spc, wdir = wdir)

  ### 3. Read
  # Output tables have names that depend on the method, thus a separate call to
  # ReadX13 is needed
  if (!is.null(spc$seats)){
    z <- ReadX13(method = "seats", wdir = wdir, save.out = save.out)
  } else if (!is.null(spc$x11)){
    z <- ReadX13(method = "x11", wdir = wdir, save.out = save.out)
  } else {
    warning("dont know what to read if neater seats or x11 are specified.")
  }
  
  z$spc <- spc
  
  ### 4. final transformations
  z$data$original <- x
  z$data <- ts(z$data, start = start(x), frequency = frequency(x))
  z$call <- match.call()
  class(z) <- "seas"
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
  
  ### avoid mutually exclusive alternatives
  
  # priority: 1. arima, 2. pickmdl, 3. automdl (default)
  if (!is.null(x$automdl) & !is.null(x$arima)){
    x$automdl <- NULL
  }
  if (!is.null(x$pickmdl) & !is.null(x$arima)){
    x$pickmdl <- NULL
  }
  if (!is.null(x$pickmdl) & !is.null(x$automdl)){
    x$automdl <- NULL
  }
  
  # priority: 1. x11, 2. seats (default)
  if (!is.null(x$seats) & !is.null(x$x11)){
    x$seats <- NULL
  }
  
  # priority: 1. x11regression, 2. regression (default)
  if (!is.null(x$x11regression) & !is.null(x$regression)){
    x$regression <- NULL
  }
  
  
  ### ensure correct output
  
  # seats and x11 have different output tables
  if (!is.null(x$seats)){
    x$seats <- list(save = c("s10", "s11", "s12", "s13", "s16", "s18"))
  } else if (!is.null(x$x11)){
    x$x11 <- list(save = c("d10", "d11", "d12", "d13", "d16", "e18"))
  } else {
    stop("wrong method.")
  }
  
  # if force is present, return adjusted output
  if (!is.null(x$force$type)){
    x$force$save <- "saa"
  }
  
  # always return estimate model
  x$estimate$save <- "model"
  
  x
}


#' @export
RunSPC <- function(x, method = "seats", wdir = tempdir()){
  stopifnot(inherits(x, "SPC"))
  
  
  
  
  # ---------------------
  # if everything works, need to be platform independent
  
#   x13dir <- system.file(package = "seasonal")
  
#   x13dir <- "C:/Users/seco-sxh/github/seasonal/inst/"
  
  x13dir <- "~/seasonal/inst/"
  # ---------------------
  
  
  WriteSPC(x, paste0(wdir, "/spcfile.spc"))
  system(paste0(x13dir, "x13/x13as ", paste0(wdir, "/spcfile")))
  
#   shell(paste0(path, "x13/x13as.exe ", path, "io/", spcfile, " ", path, "io/out/test"))
}




#' @export
SPCSeries <- function(x, name = "series", start = NULL, end = NULL){
  stopifnot(inherits(x, "ts"))
  
  z <- list()
  z$title <- paste0("\"", name, "\"")
  z$start <- paste0(start(x)[1], ".", cycle(x)[1])
  z$data <- as.numeric(x)
  z
}