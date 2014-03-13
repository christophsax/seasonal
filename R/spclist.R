
mod_spclist <- function(x, overwrite = TRUE, ...){
  # Add one or several X-13ARIMA-SEATS specs/arguments to a spclist
  #
  # x  "spclist" object
  #
  # returns a "spclist"
  #
  # required by seas
  stopifnot(inherits(x, "spclist"))
  
  mod.list <- list(...)
  
  for (i in seq_along(mod.list)){
    content.i <- mod.list[[i]]
    names.i <- names(mod.list)[i]
    
    split.names.i <- strsplit(names.i, "\\.")
    
    stopifnot(length(split.names.i) == 1)
    
    spc.name <- split.names.i[[1]][1]
    
    # empty list if spc is not present
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
      if (overwrite){
        x[[spc.name]][[spc.arg]] <- content.i
      } else {
        x[[spc.name]][[spc.arg]] <- unique(c(x[[spc.name]][[spc.arg]], content.i))
      }
    } else {
      stop("X-13ARIMA-SEATA options should contain a spec and an optional argument after the dot.")
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
  
  #   # priority: 1. x11regression, 2. regression (default)
  #   if (!is.null(x$x11regression) & !is.null(x$regression)){
  #     x$regression <- NULL
  #   }
  #   
  
  ### ensure correct output
  
  # seats and x11 have different output tables
  if (!is.null(x$seats)){
    x <- mod_spclist(x, seats.save = c("s10", "s11", "s12", "s13", "s16", "s18"))
  } else if (!is.null(x$x11)){
    x <- mod_spclist(x, x11.save = c("d10", "d11", "d12", "d13", "d16", "e18"))
  } 
  
  if (!is.null(x$automdl)){
    x <- mod_spclist(x, automdl.print = "bestfivemdl")
  }
  
  if (!is.null(x$history)){
    x <- mod_spclist(x, history.save = c("sarevisions", "saestimates"))
  }
  
  if (!is.null(x$slidingspans)){
    x <- mod_spclist(x, slidingspans.save = c("sfspans"))
  }
  
  # if force is present, return adjusted output
  if (!is.null(x$force$type)){
    x$force$save = "saa"
  }
  
  # always return estimate model
  x <- mod_spclist(x, overwrite = FALSE, 
                   estimate.save = c("model", "estimates", "lkstats", 
                                     "regressioneffects"))
  
#   always.add <- c("model", "estimates", "residuals", "lkstats", "regressioneffects")
#   if (is.null(x$estimate$save)){
#     x$estimate$save <- always.add
#   } else {
#     to.add <- always.add[!(always.add %in% x$estimate$save)]
#     x$estimate$save <- c(x$estimate$save, to.add)
#   }
  
  x
}
