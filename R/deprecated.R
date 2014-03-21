#' Deprecated
#' 
#' These functions are deprecated and will be removed in the next version.
#' 
#' @param object a seas object
#'  
#' @export
regressioneffects <- function(object){
  message("function is deprecated and will be removed in the next version. \nuse instead the more general function:\n  series(x, \"estimate.regressioneffects\") ")   
  series(object, "estimate.regressioneffects") 
}
