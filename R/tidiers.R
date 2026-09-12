# Tidier methods, for the generics of the 'generics' package.
#
# 'broom::tidy' is a re-export of 'generics::tidy', so registering on
# 'generics' covers broom as well as every other package that uses these
# generics. Nothing is imported: the methods are registered lazily, via
# `S3method(generics::tidy, seas)` in NAMESPACE (R >= 3.6.0), so they become
# available as soon as 'generics' is loaded, and cost nothing if it never is.
# The data.frames themselves are produced by `as.data.frame()` and
# `glance_df()`, which do not depend on any of this.

#' @exportS3Method generics::tidy
tidy.seas <- function(x, ...) {
  as.data.frame(summary(x, ...))
}

#' @exportS3Method generics::augment
augment.seas <- function(x, ...) {
  as.data.frame(x, ...)
}

#' @exportS3Method generics::glance
glance.seas <- function(x, ...) {
  glance_df(summary(x, ...))
}

# keep a missing element in the one-row data.frame, rather than dropping it
or_na <- function(x, mode = "numeric") {
  if (length(x) == 0) as.vector(NA, mode) else unname(x)
}

# one-row summary of a "summary.seas" object: the information that
# print.summary.seas shows below the coefficient matrix
glance_df <- function(x) {
  stopifnot(inherits(x, "summary.seas"))

  adjustment <- if (!is.null(x$spc$seats)) {
    "SEATS"
  } else if (!is.null(x$spc$x11)) {
    "X11"
  } else {
    "none"
  }

  z <- list(
    adjustment = adjustment,
    arima = or_na(x$model$arima$model, "character"),
    transform = or_na(x$transform.function, "character"),
    nobs = or_na(x$nobs),
    AICc = or_na(x$aicc),
    BIC = or_na(x$bic),
    qs = or_na(x$qsv["qs"]),
    qs.p.value = or_na(x$qsv["p-val"])
  )

  if (!is.null(x$resid)) {
    bltest <- Box.test(x$resid, lag = 24, type = "Ljung")
    z$box.ljung <- unname(bltest$statistic)
    z$box.ljung.p.value <- bltest$p.value

    # shapiro.test() is only defined for 3 to 5000 observations
    if (length(x$resid) >= 3 && length(x$resid) <= 5000) {
      swtest <- shapiro.test(x$resid)
      z$shapiro <- unname(swtest$statistic)
      z$shapiro.p.value <- swtest$p.value
    }
  }

  data.frame(z, stringsAsFactors = FALSE)
}
