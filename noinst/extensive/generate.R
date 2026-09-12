# Generate tests/testthat/test-extensive-cases.R
#
# The file contains one test_that() block per example call in ex_run.csv. Each
# block runs the call and checks four things:
#
#   1. seas() returns a model
#   2. final() matches the stored benchmark (numerical regression)
#   3. update() reproduces the model
#   4. writing an spc file and reading it back gives the same series
#   5. static() reproduces the model
#
# Cases that are known to fail one of these checks are pinned with
# expect_error() and a comment naming the reason, so that we notice when a
# known issue is fixed.
#
# Run from the package root:
#
#   Rscript noinst/extensive/generate.R
#
# To regenerate the benchmark as well (this overwrites the reference values,
# so only do it when you have decided that the current output is correct):
#
#   SEASONAL_REGENERATE_BENCHMARK=true Rscript noinst/extensive/generate.R

pkgload::load_all(".", quiet = TRUE)
library(seasonal)

csv <- "noinst/extensive/ex_run.csv"
out_file <- "tests/testthat/test-extensive-cases.R"
benchmark_file <- "tests/testthat/fixtures/extensive-benchmark.rds"

# --- case list ----------------------------------------------------------------

# These calls are not part of the test set. They are the "known issues" that
# do not run at all, kept in ex_run.csv for reference.
excluded <- c(87, 91, 96, 97, 98, 99, 100, 101, 102, 103)

cc <- read.csv(csv)
calls <- as.character(cc$r)[-excluded]

# --- known issues -------------------------------------------------------------

# Cases where static() does not reproduce the model. Verified against
# seasonal 1.10.0 and x13binary 1.1.61.2 on 2026-09-12.
static_issues <- list(
  `47` = list(
    why = "complicated outliers (qi1950.2-1950.4) are not read back from the mdl file",
    msg = "does not occur on a valid date"
  ),
  `53` = list(
    why = "seasonal regressors with a span (td//1952.dec/) drift beyond the static tolerance",
    msg = "Static series is different"
  ),
  `60` = list(
    why = "fixed regression.b coefficients are written back with the wrong length",
    msg = "Number of initial values is not the same"
  )
)

# --- benchmark ----------------------------------------------------------------

eval_case <- function(x) {
  exprs <- parse(text = x)
  set.seed(100) # some examples use runif()
  if (length(exprs) > 1) {
    for (i in seq_len(length(exprs) - 1)) eval(exprs[[i]], envir = globalenv())
  }
  eval(exprs[[length(exprs)]], envir = globalenv())
}

if (identical(Sys.getenv("SEASONAL_REGENERATE_BENCHMARK"), "true")) {
  message("regenerating ", benchmark_file)
  bench <- lapply(seq_along(calls), function(i) {
    message("  case ", i)
    final(eval_case(calls[i]))
  })
  names(bench) <- paste0("case", seq_along(calls))
  saveRDS(bench, benchmark_file, version = 2)
}

# --- code generation ----------------------------------------------------------

# split a case into its setup expressions and the final seas() call
split_case <- function(x) {
  exprs <- parse(text = x)
  list(
    setup = if (length(exprs) > 1) {
      vapply(
        exprs[seq_len(length(exprs) - 1)],
        function(e) paste(deparse(e), collapse = "\n"),
        character(1)
      )
    } else {
      character(0)
    },
    call = paste(deparse(exprs[[length(exprs)]]), collapse = "\n")
  )
}

indent <- function(x, n = 2) {
  paste0(strrep(" ", n), unlist(strsplit(x, "\n", fixed = TRUE)))
}

gen_case <- function(i) {
  parts <- split_case(calls[i])
  issue <- static_issues[[as.character(i)]]

  body <- c(
    "  skip_if_not_extensive()",
    ""
  )

  if (length(parts$setup) > 0) {
    # some examples build their regressors with runif(), so the seed has to
    # be fixed for the benchmark comparison to mean anything
    if (any(grepl("runif|rnorm|rbinom|sample\\(", parts$setup))) {
      body <- c(
        body,
        "  # the regressors below are random, the benchmark was generated with",
        "  # this seed",
        "  set.seed(100)",
        ""
      )
    }
    body <- c(body, unlist(lapply(parts$setup, indent)), "")
  }

  body <- c(
    body,
    indent(paste0("m <- ", parts$call)),
    "",
    '  expect_s3_class(m, "seas")',
    "",
    "  # numerical regression against the stored benchmark. The tolerance is",
    "  # loose enough to absorb the last-digit differences between the X-13",
    "  # builds on different platforms.",
    paste0(
      "  expect_equal(final(m), benchmark_final(", i,
      "), tolerance = 1e-6)"
    ),
    "",
    "  # update() reproduces the model",
    "  expect_equal(final(update(m)), final(m))",
    "",
    "  # writing the spc and reading it back gives the same series",
    "  expect_spc_roundtrip(m)",
    ""
  )

  if (is.null(issue)) {
    body <- c(
      body,
      "  # static() reproduces the model",
      "  expect_no_error(static(m, fail = TRUE))"
    )
  } else {
    body <- c(
      body,
      paste0("  # Known issue: ", issue$why),
      "  # Pinned so that we notice when it starts working.",
      paste0('  expect_error(static(m, fail = TRUE), "', issue$msg, '")')
    )
  }

  c(
    paste0('test_that("example case ', i, ' runs, matches the benchmark, and is reproducible", {'),
    body,
    "})",
    ""
  )
}

header <- c(
  "# Generated by noinst/extensive/generate.R -- do not edit by hand.",
  "#",
  "# One block per example call in noinst/extensive/ex_run.csv. See the",
  "# generator for what is checked and for the list of known issues.",
  ""
)

writeLines(
  c(header, unlist(lapply(seq_along(calls), gen_case))),
  out_file
)

message("wrote ", out_file, " (", length(calls), " cases)")
