multi_seas <- function(x, xreg = NULL, xtrans = NULL,
         seats.noadmiss = "yes", transform.function = "auto",
         regression.aictest = c("td", "easter"), outlier = "",
         automdl = "", na.action = na.omit,
         out = FALSE, dir = NULL, ..., list = NULL){



  # x <- cbind(a = AirPassengers, b = AirPassengers)

  # # multiple series, one spec
  # seas(x, x11 = "")

  # # alternatively, using list =
  # seas(x, list = list(x11 = ""))


  # seas(x, list = list(list(x11 = ""), list(x11 = ""))


  # seas(list = list(list(x = AirPassengers = x11 = ""), list(x = AirPassengers, x11 = ""))


  # intial checks
  checkX13(fail = TRUE, fullcheck = FALSE, htmlcheck = FALSE)

  if (!is.null(list) && !inherits(list, "list")){
    stop("the 'list' argument mus be of class 'list'")
  }
  if (length(names(list)) != length(list)){
    stop("all spec.argument combinations in 'list' must be named")
  }

  # save series name
  series.name <- deparse(substitute(x))[1]
  series.name <- gsub('[\'\\"]', '', series.name)
  series.name <- gsub(':', '_', series.name)

  list_dots <- list(...)
  common_names <- intersect(names(list_dots), names(list))
  if (length(common_names) > 0) {
    stop(
      "some spec args are specified in '...' and in 'list': ",
      paste(common_names, collapse = ", "),
      call. = FALSE
    )
  }

  use_if_not_in_list <- c(
    "x", "xreg", "xtrans", "seats.noadmiss", "transform.function",
    "regression.aictest", "outlier", "automdl"
  )
  not_in_list <- setdiff(use_if_not_in_list, names(list))
  list_not_in_list <- mget(not_in_list)

  # order matters, e.g., list(outlier = "", outlier.critical = 3)
  list_combined <- c(list_not_in_list, list, list_dots)

  seas_list(
    list = list_combined,
    na.action = na.omit,
    out = out,
    dir = dir,
    call = match.call(),
    series.name = series.name
  )
}
