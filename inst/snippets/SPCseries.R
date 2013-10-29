# 
# #' @export
# SPCSeries <- function(x, name = "series", start = NULL, end = NULL){
#   stopifnot(inherits(x, "ts"))
#   
#   z <- list()
#   z$title <- paste0("\"", name, "\"")
#   z$start <- paste0(start(x)[1], ".", cycle(x)[1])
#   z$data <- as.numeric(x)
#   z
# }