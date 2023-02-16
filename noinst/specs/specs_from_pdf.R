devtools::load_all()
library(tidyverse)
library(rex)

txt_full <- pdftools::pdf_text(
  "https://www2.census.gov/software/x-13arima-seats/x13as/unix-linux/documentation/docx13ashtml.pdf"
)

# Named thus because this bit used to be extracted from the quickstart guido
quick_from_full <- tibble(
  page = txt_full
) |>
  filter(
    re_matches(
      page,
      rex("Table B.1: Print and Save")
    )
  ) |>
  # Convert to lines
  pull(page) |>
  paste(collapse = "\n") |>
  strsplit("\n") |>
  unlist() |>
  trimws() |>
  tibble() |>
  setNames("line") |>
  # Pull lines that are part of the desired tables
  mutate(
    starts_table = re_matches(line, rex("Name", anything, "Spec")),
    is_after_table = re_matches(line, rex("PRINT AND SAVE TABLES")),
    table_number = cumsum(starts_table)
  ) |>
  group_by(table_number) |>
  filter(cumsum(is_after_table) == 0) |>
  ungroup() |>
  filter(table_number > 0) |>
  filter(!starts_table) |>
  mutate(
    line = trimws(line)
  ) |>
  filter(nchar(line) > 0) |>
  pull(line) |>
  re_matches(
    rex(
      start,
      capture(alnums, name = "long"),
      spaces,
      capture(alnums, name = "short"),
      spaces,
      capture(alnums, name = "save"),
      spaces,
      capture(alnums, name = "brief"),
      spaces,
      capture(alnums, name = "default"),
      spaces,
      capture(alnums, name = "spec"),
      end
    )
  ) |>
  as_tibble() |>
  transmute(
    long = sprintf("%s.%s", spec, long),
    short,
    spec,
    is.save = save == "yes",
    is.brief = brief == "yes",
    is.default = default == "yes"
  )

full <- tibble(
  page = txt_full
) |>
  # Only keep pages in chapter 7 containing the desired table header
  filter(
    re_matches(
      page,
      rex(
        or("chapter 7", rex("7", dot, numbers)),
        anything,
        "name", anything, "description of table",
        anything,
        "Name gives the name of each"
      ),
      options = c("s", "i"))
  ) |>
  # Convert to lines
  pull(page) |>
  paste(collapse = "\n") |>
  strsplit("\n") |>
  unlist() |>
  trimws() |>
  tibble() |>
  setNames("line") |>
  # Pull lines that are part of the desired tables
  mutate(
    starts_table = re_matches(line, rex("name", anything, "description of table")),
    is_after_table = re_matches(line, rex("Name gives the name of each")),
    table_number = cumsum(starts_table)
  ) |>
  group_by(table_number) |>
  mutate(
    spec = re_matches(
      line,
      rex(
        # n.b. "Argument(s) for" is not sufficient here as there are also tables
        #      "XXX Available Using the YYY Argument for SPEC"
        #      those may reference the tables we want but actually document
        #      different parameters of the spec
        or(
          "Output Tables for",
          "Tables Saved As Percentages in the save Argument",
          "Available Output Tables in Both print and save Arguments",
          "Output Tables Available Only with save Argument"
        ),
        anything,
        space,
        capture(anything, name = "X"),
        end
      ),
      options = "i"
    )$X) |>
  mutate(spec = trimws(tolower(spec))) |>
  fill(spec, .direction = "downup") |>
  filter(cumsum(is_after_table) == 0) |>
  ungroup() |>
  filter(table_number > 0) |>
  filter(!starts_table) |>
  # Mark lines that are just the continuation of the description col of the previous
  # entry (they should contain only single spaces as they do not span multiple columns)
  mutate(
    is_part_of_prev = !re_matches(line, rex(between(spaces, 2, 5)))
  ) |>
  mutate(
    entry_number = cumsum(!is_part_of_prev)
  ) |>
  summarize(
    # gsub to remove no longer needed hyphenation
    line = gsub("- ", "", paste(line, collapse = " ")),
    .by = c("entry_number", "spec")
  ) |>
  mutate(
    data = re_matches(
      line,
      rex(
        start,
        capture(alnums, name = "long"),
        spaces,
        capture(alnums, name = "short"),
        capture(any_of(space, "·", "+"), name = "issave"),
        capture(anything, name = "description"),
        end
      )
    )
  ) |>
  unnest(data) |>
  transmute(
    long = sprintf("%s.%s", spec, long),
    short,
    is.save = re_matches(issave, "\\+"),
    spec,
    description
  )

jnd <- full_join(quick_from_full, full, by = "long")

jnd |> filter(is.na(short.x))

jnd |> filter(is.na(short.y))

jnd |> filter(!is.na(short.x) & !is.na(short.y)) |>
  filter(is.save.x != is.save.y)

tbl_all <- full_join(quick_from_full, full, by = c("long")) |>
  mutate(
    short = coalesce(short.x, short.y),
    spec = coalesce(spec.x, spec.y),
    # Be optimistic for starters
    is.save = is.save.x | is.save.y
  ) |>
  select(
    -short.x,
    -short.y,
    -spec.x,
    -spec.y,
    -is.save.x,
    -is.save.y
  )

manual_specs <- read_csv("noinst/specs/SPECS_MANUAL.csv")

tbl_final <- left_join(tbl_all, manual_specs, by = "long") |>
  select(long, short, spec, is.save, is.series, description, requires)


# Check validity of tbl_final and fix possible issues ---------------------

td <- tempdir()

analyze_output <- function(.x, .y, x11_seats) {
  message("doing ", .x$long)

  # Create a separate dir for each combination to allow inspection of the generated files
  specdir_seats <- file.path(td, paste0(.x$spec, "_", .x$short, "_", x11_seats))
  dir.create(specdir_seats, showWarnings = FALSE, recursive = TRUE)

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
      error_msg = as.character(m),
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

# Run every spec + save combination and see what sticks
res <- tbl_final |>
  # The author does not understand composite spec yet. ;)
  filter(spec != "composite") |>
  rowwise() |>
  group_map(~{
    bind_rows(
      analyze_output(.x, .y, "seats") |> mutate(x11_seats = "seats"),
      analyze_output(.x, .y, "x11") |> mutate(x11_seats = "x11")
    )
  }) |>
  bind_rows()


# Mark entries that x13 actually can't save
not_save <- res |>
  filter(
    !is.na(error_msg),
    re_matches(error_msg, rex(
        or(
          "Save argument is not defined",
          "Argument name \"save\" not found"
        )
      ),
      options = "i")
  ) |>
  select(long)

tbl_final <- tbl_final |>
  mutate(
    is.save.actual = case_when(
      spec == "composite" ~ is.save,
      TRUE ~ !(long %in% not_save$long)
    )
  )

# At the time of writing there was only one: slidingspans.yysummary which
# is marked as save in the manual (conflictingly) but gives a
# "Save argument is not defined" error
tbl_final |> filter(is.save != is.save.actual)

# Mark the specs which an actual run of x13 claims (should) have saveable output
# as is.save. Calling seas with non-saveable tables in save should have thrown
# one of the errors checked for above (e.g. seas(AirPassengers, regression.save = "ats"))
tbl_final <- tbl_final |>
  mutate(
    is.save = is.save.actual
  )

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
  unnest(err) |>
  mutate(
    works_with_one = n() == 1,
    .by = long
  )
#
# contentless |> pull(x11_seats) |> table()
#
# contentless |>
#   filter(works_with_one) |>
#   select(long, x11_seats) |>
#   left_join(tbl_final) |>
#   select(long, short, x11_seats, description) |>
#   View()
#
# tbl_final |>
#   filter(!long %in% contentless$long) |>
#   View()

tbl_final |>
  select(long, short, spec, is.save, is.series, description, requires) |>
  write_csv("noinst/specs/SPECS.csv")

