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
    return(tibble(is_error = TRUE))
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
          content = {message(file); list(read_series(file))}
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
        select(long, short, is_ts, has_content, is_series, is_error, content, model)
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

res |> filter(is_error)

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
