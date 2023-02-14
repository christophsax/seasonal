library(tidyverse)
library(rex)

# TODO: There is also a full table in appendix B of the full manual but eh.
txt_quick <- pdftools::pdf_text(
  "https://www2.census.gov/software/x-13arima-seats/x13as/windows/documentation/qrefx13ashtmlpc.pdf"
  ) |>
  paste(collapse = "\n") |>
  strsplit("\n") |>
  unlist()

quick <- tibble(
  line = trimws(txt_quick)
) |>
  mutate(
    starts_table = grepl("^Name.*Table\\?.*Spec$", line),
    table_number = cumsum(starts_table),
    is_empty = line == ""
  ) |>
  group_by(
    table_number
  ) |>
  filter(cumsum(is_empty) == 0) |>
  ungroup() |>
  filter(
    table_number > 0,
    !grepl("Table\\?", line)
  ) |>
  pull(line) |>
  re_matches(
    rex(
      start,
      capture(alnums, name = "long"),
      spaces,
      capture(alnums, name = "short"),
      capture(anything, name = "rest"),
      boundary,
      capture(alnums, name = "spec"),
      end
    )
  ) |>
  as_tibble() |>
  mutate(
    save = re_matches(rest, rex(start, between(space, 0, 15), "+"))
  ) |>
  transmute(
    long = sprintf("%s.%s", spec, long),
    short,
    spec,
    is.save = save
  )


txt_full <- pdftools::pdf_text(
  "https://www2.census.gov/software/x-13arima-seats/x13as/unix-linux/documentation/docx13ashtml.pdf"
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
        or("Output Tables for ", "Argument of ", "Argument for ", "Arguments for "),
        capture(anything, name = "X")
      )
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
        any_of(space, "·", "+"),
        capture(anything, name = "description"),
        end
      )
    )
  ) |>
  unnest(data) |>
  transmute(
    long = sprintf("%s.%s", spec, long),
    short,
    spec,
    description
  )

tbl_all <- full_join(quick, full, by = c("long")) |>
  mutate(
    short = coalesce(short.x, short.y),
    spec = coalesce(spec.x, spec.y)
  ) |>
  select(
    -short.x,
    -short.y,
    -spec.x,
    -spec.y
  )

manual_specs <- read_csv("noinst/specs/SPECS_MANUAL.csv")

tbl_final <- left_join(tbl_all, manual_specs, by = "long") |>
  select(long, short, spec, is.save, is.series, description, requires)

write_csv(tbl_final, "noinst/specs/SPECS.csv")
