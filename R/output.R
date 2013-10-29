#' @method print seas
#' @export
#' 
print.seas <- function(x){
  cat("X13-ARIMA-SEATS seasonal adjustment of ", x$spc$series$title, ":\n", sep = "")
  
  cat("\nAdjustment method      ")
  if (!is.null(x$spc$seats)){
    cat("SEATS")
  } else if (!is.null(x$spc$x11)){
    cat("X11")
  } else {
    cat("none")
  }
  
  cat("\nARIMA model search     ")
  if (!is.null(x$spc$automdl)){
    cat("automdl")
  } else if (!is.null(x$spc$pickmdl)){
    cat("'pickmdl'")
  } else {
    cat("none")
  }
  
  cat("\nOutlier detection      ")
  if (!is.null(x$spc$outlier)){
    cat("on")
  } else {
    cat("off")
  } 
  
  cat("\nAIC regressor testing  ")
  if (!is.null(x$spc$regression$aictest)){
    cat("on")
  } else {
    cat("off")
  } 

  cat("\n\nX13-ARIMA-SEATS messages:", x$err[-c(1:5)], sep = "\n")
  
  cat("\nUse:\n- final() to extract the final adjusted series.\n- summary() for details on the adjustment model.\n- plot() for diagnostical plots.\n- static() to extract the static call for the model.")
  
}



#' @export
print.spclist <- function(x){
  cat(parse_spclist(x))
}


#' @export
spc <- function(x){
  x$spc
}

#' @export
mdl <- function(x){
  x$mdl
}

#' @export
out <- function(x){
  if (is.null(x$out)){
    stop("Contains no 'out' data. Use 'seas' with the 'out = TRUE' option.")
  }
  # remove page breaks
#   x$output <- str_replace_all(x$output, '\\f', '')
  page(x$out)
  invisible(x$out)
}

#' @export
residuals.seas <- function(x){
  x$data[,'residuals']
}

