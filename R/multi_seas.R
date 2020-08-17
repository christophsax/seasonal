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

  al <- align_x_list(x, list)
  series.names <- al$series.names
  xs <- al$xs
  lists <- al$lists

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

  # browseURL(wdir)

  # --- multimode = c("R")

  # lapply(iofiles, x13_run, out = FALSE)

  # --- multimode = c("x13")

  # meta data file
  writeLines(paste0(iofiles, ".dta"), file.path(wdir, "metafile.dta"))

  # meta file
  writeLines(iofiles, file.path(wdir, "metafile.mta"))

  x13_run(file = file.path(wdir, "metafile"), out = FALSE, meta = TRUE)




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
