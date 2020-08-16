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

  series.names <- valid_names(colnames(x))

  # expand x
  xs <- lapply(seq(ncol(x)), function(i) x[, i])

  if (is.null(list)) list <- list()
  # expand lists
  if (all(sapply(list, inherits, "list")) && length(list) == length(xs)) {
    # one list for each series
    lists <- list
  } else {
    # one list for all series
    lists <- rep(list(list), length(xs))
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
    function(iofile, x) {
      x13_import(iofile = iofile, x = x, na.action = na.action, out = out)
    },
    iofile = iofiles,
    x = xs
  )

  zs

}
