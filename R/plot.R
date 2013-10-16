#' @export
plot.seas <- function(x){
  ts.plot(cbind(z$data[,'original'], z$data[,'seasonaladj']), col = c("black", "red"))
}
