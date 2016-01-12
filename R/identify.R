#' Manually Identify Outliers
#' 
#' Select or deselect outliers by point and click. To quit and return the call, press
#' ESC. Click several times to loop through different outlier types.
#' 
#' @param x      an object of class \code{"seas"}.
#' @param type   character vector, types of outlier to loop through.
#' @param ...    unused, for compatibility with the generic function.
#'   
#' @return an object of class \code{"seas"}, containing the static call of the
#'   selected model.
#'   
#' @export
#' @examples
#' \dontrun{
#'  m <- seas(AirPassengers)
#'  identify(m)
#' }
identify.seas <- function(x, type = c("ao", "tc", "ls"), ...){
  f <- frequency(final(x))
  
  repeat{
    print(summary(x))
    plot(x, main = "click several times to loop through different outlier types")
    ol.ts <- outlier(x)
    sc <- static(x, test = FALSE)
    cat("\n")

    id.select <- identify(final(x), n = 1, plot = FALSE)
    if (length(id.select) == 0){
      return(x)
    }
    if (any(sc$regression.variables  %in% outlier(x, full = TRUE)[id.select])){
      ol.sel <- sc$regression.variables[sc$regression.variables  %in% outlier(x, full = TRUE)[id.select]]
      ol.pos <- which(type %in% substr(ol.sel, start = 1, stop = 2))
      if (ol.pos == length(type)){
        sc$regression.variables <- 
          sc$regression.variables[!sc$regression.variables  %in% outlier(x, full = TRUE)[id.select]]
      } else {
        ol.mod <- gsub(type[ol.pos], type[ol.pos + 1], ol.sel)
        sc$regression.variables[sc$regression.variables  %in% outlier(x, full = TRUE)[id.select]] <- ol.mod
      }
    } else {  # add new outlier
      time.ao <- month.abb[cycle(final(x))[id.select]]
      if (f == 12){
        cyc <- month.abb[cycle(final(x))[id.select]]
      } else {
        cyc <- cycle(final(x))[id.select]
      }
      ol.new <- paste0(type[1], floor(time(final(x))[id.select] + 1e-05), ".", cyc)
      sc$regression.variables <- c(sc$regression.variables, ol.new)
    }
    x <- eval(sc)
  }
}

