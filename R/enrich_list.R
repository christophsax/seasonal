enrich_list <- function(list,
                        list_dots = NULL,
                        x = NULL,
                        xreg = NULL,
                        xtrans = NULL,
                        seats.noadmiss = "yes",
                        transform.function = "auto",
                        regression.aictest = c("td", "easter"),
                        outlier = "",
                        automdl = ""
                        ) {

  if (!is.null(list) && !inherits(list, "list")){
    stop("the 'list' argument mus be of class 'list'")
  }
  if (length(names(list)) != length(list)){
    stop("all spec.argument combinations in 'list' must be named")
  }

  # FIXME stop if x and list(x = ) provided

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

}
