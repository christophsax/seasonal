
#' @rdname out
#' @export
revisions <- function(x, ...){
  ldots <- list(...)
  if (length(ldots) == 0){
    ldots$history = list()
  } else {
    if (!any(grepl("history", names(ldots)))){
      ldots$history = list()
    }
  }
  z <- reeval(x, ldots)

  n.line <- grep("history analysis", z$out)
  if (length(n.line) == 0){
    warning("No history analysis found in the output. Returning the standard output.")
  } else {
    z$revisions <- z$out[grep("  History analysis", z$out)[1]:length(z$out)]
    class(z$revisions) <- "out"
  }
  class(z) <- "revisions"
  z
}

#' @rdname plot.seas
#' @method plot revisions
#' @export
plot.revisions <- function(x, main = "revisions", ...){
  ts.plot(x$sae, col = c("black", "red"), main = main, ...)

  legend("topleft", c("concurrent estimation", "final estimation"), lty = 1, col = c("black", "red"), bty = "n", horiz = TRUE)
}

#' @export
#' @method print revisions
print.revisions <- function(x, ...){
  print(x$revisions)
}



