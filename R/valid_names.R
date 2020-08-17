# valid_names(c("sd$fs&%@#:$%^&*äüsfdd/fsdfsds", "sd$fs&%@#:$%^&*äüsfdd/fsdfsds"))
valid_names <- function(x) {
  x <- gsub('[\'\\"]', '', x)
  x <- gsub('[:\\$/\\]', '_', x)
  x <- gsub("[^0-9A-Za-z_]","" , x ,ignore.case = TRUE)
  x <- substr(x, start = 0, stop = 20)
  make.unique(x,  sep = "_")
}

