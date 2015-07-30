
mod_spclist <- function(x, list){
  # Add one or several X-13ARIMA-SEATS specs/arguments to a spclist
  #
  # x  "spclist" object
  # list named list with additional spec.argument combinations
  #
  # returns a "spclist"
  #
  # required by seas
  stopifnot(inherits(x, "spclist"))
  
  for (i in seq_along(list)){
    content.i <- list[[i]]
    names.i <- names(list)[i]
    
    split.names.i <- strsplit(names.i, "\\.")
    
    stopifnot(length(split.names.i) == 1)
    
    spc.name <- split.names.i[[1]][1]
    
    # empty list if spc is not present
    if (is.null(x[[spc.name]])){
      x[[spc.name]] <- list()
    }
    
    if (length(split.names.i[[1]]) == 1){
      if (length(content.i) == 1){
          if (content.i == ""){
            x[[spc.name]] <- list()
          } else {
            x[[spc.name]] <- content.i
          }
      } else {
        x[[spc.name]] <- content.i
      }
    } else if (length(split.names.i[[1]]) == 2){
      spc.arg <- split.names.i[[1]][2]
      if (!is.list(x[[spc.name]])){
        stop("Specification of ", spc.name, " without a dot is not allowed in this context.", call. = FALSE)
      }
      if (is.null(x[[spc.name]][[spc.arg]])){
        x[[spc.name]][[spc.arg]] <- list()
      }
      if (length(x[[spc.name]][[spc.arg]]) == 0){
        x[[spc.name]][[spc.arg]] <- content.i
      } else {
        x[[spc.name]][[spc.arg]] <- unique(c(x[[spc.name]][[spc.arg]], content.i))
      }
    } else {
      stop("X-13ARIMA-SEATS options should contain a spec and an optional argument after the dot.", call. = FALSE)
    }
  }
  x
}

consist_spclist <-function(x){
  # ensure consistency of a spclist
  # 
  # removes exclusive spec.arguments. See 'priority rules' on website..
  # ensures the necessary output
  #
  # x  "spclist" object
  #
  # returns a "spclist"
  #
  # required by seas
  
  stopifnot(inherits(x, "spclist"))

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

  ### general output modification
  
  x <- mod_spclist(x, list = list(estimate.save = c("model", "estimates", "lkstats", 
                                        "residuals")))
  
  
  x <- mod_spclist(x, list = list(spectrum.print = "qs"))
  
  x <- mod_spclist(x, list = list(transform.print = "aictransform"))
  


  ### ensure arima.model is character
  if (!is.null(x$arima$model)){
    x$arima$model <- as.character.arima(x$arima$model)
  }
  

  ### spec specific output modification
  
  if (!is.null(x$seats)){
    x <- mod_spclist(x, list = list(seats.save = c("s10", "s11", "s12", "s13", "s16", "s18")))    
  } 
  
  if (!is.null(x$x11)){

    x <- mod_spclist(x, list = list(x11.save = c("d10", "d11", "d12", "d13", "d16", "e18")))
  } 
  
  if (!is.null(x$automdl)){
    x <- mod_spclist(x, list = list(automdl.print = "bestfivemdl"))
  }
  
  if (!is.null(x$force)){
    x <- mod_spclist(x, list = list(force.save = "saa"))
  }
  x
}
