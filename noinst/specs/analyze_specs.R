# This is a scratchpad for

devtools::load_all()
library(tidyverse)
library(rex)

td <- tempdir()

specs <- read_csv("noinst/specs/SPECS.csv")

specs_save <- specs |> filter(is.save)

analyze_output <- function(.x, .y, x11_seats) {
  message("doing ", .x$long)

  specdir_seats <- file.path(td, paste0(.x$spec, "_", .x$short, "_", x11_seats))
  dir.create(specdir_seats, showWarnings = FALSE, recursive = TRUE)

  # TODO: requires
  l <- list()
  l[[paste0(.x$spec, ".save")]] <- unlist(.x$short)

  if(x11_seats == "x11") {
    l[["x11"]] = ""
  }

  if(!is.na(.x$requires)) {
    message("Adding required argument ", .x$requires)

    required_args <- re_matches(
      .x$requires,
      rex(
        capture(anything, name = "name"),
        " = \"",
        capture(anything, name = "value"),
        "\"")
      )

    l[[required_args$name]] <- required_args$value
  }

  m <- try(seas(AirPassengers, list = l, dir = specdir_seats), silent = TRUE)

  if(inherits(m, "try-error")) {
    return(tibble(
      long = .x$long,
      short = .x$short,
      is_error = TRUE,
      x11_seats
    ))
  }

  readable_files <- .x |>
    filter(!short %in% c("est", "mdl")) |>
    mutate(
      file = file.path(specdir_seats, paste0("iofile.", short))
    )

  if(nrow(readable_files) > 0) {
      readable_files |>
        rowwise() |>
        mutate(
          has_file = file.exists(file),
          content = {message(file); list(read_series(file))},
          content_raw = if(has_file) { paste(readLines(file), collapse = "\n") } else { "" }
        ) |>
        ungroup() |>
        mutate(
          is_ts = sapply(content, is.ts),
          length = sapply(content, length),
          has_content = length > 0,
          is_series = short %in% names(m$series),
          is_error = FALSE,
          model = list(m)
        ) |>
        select(long, short, is_ts, has_content, is_series, has_file, is_error, content, content_raw, model)
  } else {
    tibble()
  }
}

res <- specs_save |>
  filter(spec != "composite") |>
  rowwise() |>
  group_map(~{
    bind_rows(
      analyze_output(.x, .y, "seats") |> mutate(x11_seats = "seats"),
      analyze_output(.x, .y, "x11") |> mutate(x11_seats = "x11")
    )
  }) |>
  bind_rows()

res |> nrow()

# Seems like a "they" problem (or AirPassengers is bad input.)
# [1] ""
# [2] " X-13ARIMA-SEATS Seasonal Adjustment Program"
# [3] " Version Number 1.1 Build 57"
# [4] " Execution began  Feb 16, 2023  09.37.30 "
# [5] ""
# [6] "  Reading input spec file from C:\\Users\\X\\AppData\\Local\\Temp\\RtmpAnUWr5\\x131a6810aa3764/iofile.spc"
# [7] "  Storing any program output into C:\\Users\\X\\AppData\\Local\\Temp\\RtmpAnUWr5\\x131a6810aa3764/iofile.html"
# [8] "  Storing any program error messages into C:\\Users\\X\\AppData\\Local\\Temp\\RtmpAnUWr5\\x131a6810aa3764/iofile_err.html"
# [9] "  Storing any diagnostics output into C:\\Users\\X\\AppData\\Local\\Temp\\RtmpAnUWr5\\x131a6810aa3764/iofile.udg"
# [10] ""
# [11] ""
# [12] "Program received signal SIGSEGV: Segmentation fault - invalid memory reference."
# [13] ""
# [14] "Backtrace for this error:"
# Author's note: this is the end of the output, no backtrace
res |> filter(is_error)

# Oddly, without the 'required' history.estimates = "seasonal" it seems to work.
m <- seas(AirPassengers, history.save = "sfr")
seas(AirPassengers, history.save = "sfr", history.estimates = "seasonal")

contentless <- res |>
  filter(!has_content) |>
  transmute(
    long,
    short,
    x11_seats,
    err = lapply(model, function(x){
      y <- unclass(x$err)
      tibble(
        error = ifelse(is.list(y$error), "", unlist(y$error)),
        warning = ifelse(is.list(y$warning), "", unlist(y$warning)),
        note = ifelse(is.list(y$note), "", unlist(y$note))
      )
    })
  ) |>
  unnest(err)

res |>
  filter(is_ts)

res |>
  filter(
    is_ts,
    !is_series
  )

contentless |>
  left_join(specs, by = "long") |>
  select(long, short = short.x, description, requires) |>
  distinct(long, .keep_all = TRUE)

contentless_with_file <- res |>
  filter(
    !has_content,
    nchar(content_raw) > 0
  )

res$has_content == (nchar(res$content_raw) > 0)
