#' @export
recursive.seas <- function(x, start = NULL){
  require(vts.core)
  if (is.null(start)){  # default X-13
    start <- time(original(x))[1] + 6
  }
  
  
  lc <- as.list(x$call)
  sname <- as.character(lc$x)
  
  env <- new.env()
  
  periods <- seq(start, 
                 time(original(x))[length(original(x))], 
                 by = 1/frequency(original(x))
  )
  
  pb <- txtProgressBar(min=periods[1], max=periods[length(periods)], style=3)
  
  z <- NULL
  
  for (t in periods){
    assign(sname, window(original(x), end = t), envir = env)
    
    m <- eval(x$call, envir = env)    
    z <- cbind(z, final(m))
    setTxtProgressBar(pb, t)
  }
  
  vts(z, vstart = periods[1], vfrequency = frequency(original(x)))
} 


