seas_multi <- function(x = NULL, xreg = NULL, xtrans = NULL,
         seats.noadmiss = "yes", transform.function = "auto",
         regression.aictest = c("td", "easter"), outlier = "",
         automdl = "", na.action = na.omit,
         out = FALSE, dir = NULL, list_dots, list = NULL){

  # x <- cbind(a = AirPassengers, b = mdeaths)
  # x <- NULL


  # list <- list(list(x11 = ""), list(x11 = ""))
  # list <- list(x11 = "")
  # list <- NULL

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

  lists_combined <- Map(
    function(list, x) {
      enrich_list(
        list = list,
        list_dots = list_dots,
        x = x,
        xreg = xreg,
        xtrans = xtrans,
        seats.noadmiss = seats.noadmiss,
        transform.function = transform.function,
        regression.aictest = regression.aictest,
        outlier = outlier,
        automdl = automdl
      )
    },
    list = lists,
    x = xs
  )

  wdir <- wdir_create()
  iofiles <- file.path(wdir, series.names)

  # write specs
  spcs <- Map(
    function(list, iofile) {
      x13_prepare(list = list, na.action = na.action, iofile = iofile)
    },
    list = lists_combined,
    iofile = iofiles
  )

  # batchmode = c("R")
  lapply(iofiles, x13_run, out = FALSE)

  zs <- Map(
    function(iofile, list) {
      x13_import(iofile = iofile, x = list$x, na.action = na.action, out = out)
    },
    iofile = iofiles,
    list = lists_combined
  )

  names(zs) <- NULL
  zs

}
