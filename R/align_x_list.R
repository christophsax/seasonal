align_x_list <- function(x, list = NULL) {
  if (is.null(x)) {
    # x specified in lists
    n_series <- length(list)
    series.names <- paste0("ser_", seq(n_series))
    xs <- lapply(seq(n_series), function(i) NULL)
  } else {
    # x specified explicitly
    n_series <- ncol(x)
    series.names <- valid_names(colnames(x))
    xs <- lapply(seq(n_series), function(i) x[, i])
  }

  if (is.null(list)) list <- list()
  # expand lists
  if (all(sapply(list, inherits, "list")) && length(list) == n_series) {
    # one list for each series
    lists <- list
  } else {
    # one list for all series
    lists <- rep(list(list), n_series)
  }

  list(
    xs = xs,
    lists = lists,
    series.names = series.names
  )
}

