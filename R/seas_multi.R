seas_multi <- function(x = NULL, xreg = NULL, xtrans = NULL,
         seats.noadmiss = "yes", transform.function = "auto",
         regression.aictest = c("td", "easter"), outlier = "",
         automdl = "", composite = NULL,
         na.action = na.omit,
         out = FALSE, dir = NULL, multimode = c("x13", "R"), list_dots,
         list = NULL,
         call = NULL){

  multimode <- match.arg(multimode)

  if (!is.null(composite)) {
    composite <- enrich_list(
      list = composite,
      x = NULL,
      xreg = xreg,
      xtrans = xtrans,
      seats.noadmiss = seats.noadmiss,
      transform.function = transform.function,
      regression.aictest = regression.aictest,
      outlier = outlier,
      automdl = automdl
    )
  }

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
  iofiles <- file.path(wdir, paste0("ser", seq_along(series.names)))

  # write specs
  spcs <- Map(
    function(list, iofile) {
      x13_prepare(list = list, na.action = na.action, iofile = iofile)
    },
    list = lists_combined,
    iofile = iofiles
  )

  if (!is.null(composite)) {
    iofile_composite <- file.path(wdir, "composite")
    spc_composite <- x13_prepare(
      list = composite, na.action = na.action,
      iofile = file.path(wdir, "composite"), composite = TRUE
    )
  } else {
    iofile_composite <- NULL
  }

  if (multimode == "R") {
    if (!is.null(composite)) stop("composite requires: multimode = \"x13\"")
    lapply(iofiles, x13_run, out = FALSE)

  } else if (multimode == "x13") {
    writeLines(c(iofiles, iofile_composite), file.path(wdir, "metafile.mta"))
    x13_run(file = file.path(wdir, "metafile"), out = TRUE, meta = TRUE)

  } else {
    "invalid"
  }


  zs <- Map(
    function(iofile, list) {
      x13_import(iofile = iofile, x = list$x, na.action = na.action, out = out)
    },
    iofile = iofiles,
    list = lists_combined
  )

  names(zs) <- series.names

  # add missing elements to output

  add_element_to_each <- function(l, el, name) {
    lapply(l, function(li) {li[[name]] <- el; li})
  }

  add_elements_to_each <- function(l, els, name) {
    Map(function(li, el) {li[[name]] <- el; li}, li = l, el = els)
  }

  # z$call
  zs <- add_element_to_each(zs, call, "call")

  # z$list
  lists_combined_eval <- lapply(lists_combined, function(e) lapply(e, eval, envir = parent.frame()))
  zs <- add_elements_to_each(zs, lists_combined_eval, "list")

  # z$x
  zs <- add_elements_to_each(zs, xs, "x")

  # z$spc
  zs <- add_elements_to_each(zs, spcs, "spc")

  if (!is.null(composite)) {
    zs$composite <- x13_import(iofile = iofile_composite, x = lists_combined[[1]]$x, na.action = na.action, out = out)
    zs$composite$call <- call
    zs$composite$list <- composite
    zs$composite$spc <- spc_composite
  }

  # ### Save output files if 'dir' is specified
  if (!is.null(dir)){
    if (!file.exists(dir)){
      dir.create(dir)
    }
    file.copy(list.files(wdir, full.names = TRUE), dir, overwrite = TRUE)
    message("All X-13ARIMA-SEATS output files have been copied to '", dir, "'.")
  }

  if (!out) {
    unlink(wdir, recursive = TRUE)
  }

  zs$call <- zs[[1]]$call
  class(zs) <- c("seas_multi", class(zs))

  zs

}
