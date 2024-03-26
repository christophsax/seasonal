library(tidyverse)

# Update SPECS_auto.csv --------------------------------------------------------

# download latest version of X-13 manual and write SPEC to SPECS_auto.csv
#
# source("noinst/specs/specs_from_pdf.R")


# load new specs data from csv --------------------------------------------


SPECS_description <- read_csv("noinst/specs/SPECS_auto.csv") |>
  # Fix non-ascii characters. R CMD CHECK only allows ascii in data
  mutate(
    description = stringi::stri_trans_general(description, "latin-ascii")
  ) |>
  mutate(
    # Fix for estimate.regressioneffects. The beta should have a circumflex
    # but pdftools::pdf_text seens unable to reflect that.
    description = gsub("Xβ,b", "Xb", description),
    description = gsub("≤", "<=", description)
  ) |>
  arrange(long) |>
  mutate(requires = coalesce(requires, ""))


# Update data file --------------------------------------------------------

SPECS_old <- readr::read_csv("noinst/specs/SPECS.csv", na = "NA")

# SPECS.rda and SPECS.csv must match!
stopifnot(all.equal(arrange(get_specs(), long), as.data.frame(SPECS_old)))

SPECS <-
  SPECS_description |>
  select(-description) |>
  arrange(long) |>
  mutate(is.save = coalesce(is.save, TRUE)) |>
  mutate(is.series = coalesce(is.series, TRUE)) |>
  mutate(requires = if_else(long == "history.sfrevisions", 'history.estimates = \"seasonal\"', requires)) |>
  as.data.frame()

# library(daff)
# render_diff(diff_data(
#   SPECS_old,
#   SPECS
#
# ))

# acutal update
# readr::write_csv(SPECS, "noinst/specs/SPECS.csv")
# usethis::use_data(SPECS, overwrite = TRUE)

# SPECS_new <- readr::read_csv("noinst/specs/SPECS.csv", na = "NA")

# SPECS.rda and SPECS.csv must match!
# stopifnot(all.equal(arrange(get_specs(), long), as.data.frame(SPECS_new)))


# update roxygen header (carfully review!) -------------------------------------

txt <- read_lines("R/series.R")

# Find start/end of spec description table
line0 <- which(txt == "#' **spec** \\tab **long name** \\tab **short name** \\tab **description** \\cr")

linen <- which(txt == "#' }")[1]

#' **spec** \tab **long name** \tab **short name** \tab **description** \cr
# why this? series, but not save
# # A tibble: 5 × 6
#   long                       short spec      is.save is.series requires
#   <chr>                      <chr> <chr>     <lgl>   <lgl>     <chr>
# 1 composite.indmovseasrat    ims   composite FALSE   TRUE      ""
# 2 composite.indreplacsi      id9   composite FALSE   TRUE      ""
# 3 composite.indresidualseasf irf   composite FALSE   TRUE      ""
# 4 composite.ratioplotindsa   ir2   composite FALSE   TRUE      ""
# 5 composite.ratioplotorig    ir1   composite FALSE   TRUE      ""


# Create updated spec description table
tbl_txt <-
  SPECS_description |>
  as_tibble() |>
  filter(
    is.series == TRUE,
    is.save == TRUE
  ) |>
  select(spec, long, short, description) |>
  transmute(new = paste("#'", spec, "\\tab", long, "\\tab", short, "\\tab", description, "\\cr")) |>
  pull(new)

# Patch together new contents of series.R
txt_new <-
  c(
    txt[1:line0],
    tbl_txt,
    txt[linen:length(txt)]
  )


write_lines(txt_new, "R/series.R")
