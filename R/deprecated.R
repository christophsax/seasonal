#' Deprecated
#' 
#' These functions are deprecated and will be removed in the next version.
#' 
#' @param object a seas object
#'  
#' @export
regressioneffects <- function(object){
  .Deprecated("series", package=NULL, "Function is deprecated and will be removed soon. Use the more univeral series function. See examples in ?series.",
              old = as.character(sys.call(sys.parent()))[1L])
  
  series(object, "estimate.regressioneffects") 
}
