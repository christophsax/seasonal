
#' @rdname out
#' @export
revisions <- function(x, view = FALSE, ...){
  ldots <- list(...)
  if (length(ldots) == 0){
    ldots$history = list()
  } else {
    if (!any(grepl("history", names(ldots)))){
      ldots$history = list()
    }
  }
  z <- reeval(x, ldots)
  z$revisions <- z$out[grep("  History analysis", z$out)[1]:length(z$out)]
  class(z$revisions) <- "out"
  class(z) <- "revisions"
  z
  z
}

#' @export
#' @method plot revisions
plot.revisions <- function(x){
  ts.plot(x$sae, col = c("black", "red"), main = "revisions")
  legend("topleft", colnames(x$sae), lty = 1, col = c("black", "red"), bty = "n", horiz = TRUE)
}

#' @export
#' @method print revisions
print.revisions <- function(x){
  print(x$revisions)
}



