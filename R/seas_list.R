seas_list <- function(list, na.action = na.omit, out = FALSE, dir = NULL,
                      call = "dummycall", series.name = "x"){

  # remove out from the list
  if ("out" %in% names(list)) {
    out <- list$out
    list <- list[setdiff(names(list), "out")]
  }

  # same for na.action, which is kept in the list so that update() and the
  # functions that re-evaluate a model do not fall back to na.omit (#295)
  if ("na.action" %in% names(list)) {
    na.action <- list$na.action
    list <- list[setdiff(names(list), "na.action")]
  }

  # wdir and file name
  id <- "iofile"
  wdir <- wdir_create()
  iofile <- file.path(wdir, id)  # wdir can be derived from it

  spc <- x13_prepare(list = list, na.action = na.action, iofile = iofile)

  # run X13, either with full output or not
  x13_run(file = iofile, out = out)

  z <- x13_import(iofile, x = list[['x']], na.action = na.action, out = out)

  z$call <- call
  # save list with evaluated arguments, so they can be used to rerun
  z$list <- rm_defaults(lapply(list, eval, envir = parent.frame()))
  if (!identical(na.action, stats::na.omit)) z$list$na.action <- na.action
  z$x <- list[['x']]
  z$spc <- spc

  # ### Save output files if 'dir' is specified
  if (!is.null(dir)){
    if (!file.exists(dir)){
      dir.create(dir)
    }
    file.copy(list.files(wdir, full.names = TRUE), dir, overwrite = TRUE)
    message("All X-13ARIMA-SEATS output files have been copied to '", dir, "'.")
  }
  if (!out) {
    wdir_clean_up(wdir)
  }

  z
}

rm_defaults <- function(ll) {
  defaults <- list(
    xreg = NULL,
    xtrans = NULL,
    seats.noadmiss = "yes",
    transform.function = "auto",
    regression.aictest = c("td", "easter"),
    outlier = "",
    automdl = ""
  )

  cn <- intersect(names(ll), names(defaults))
  common <- cn[unlist(Map(identical, ll[cn], defaults[cn]))]

  ll[setdiff(names(ll), common)]
}


