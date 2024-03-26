library(tidyverse)

# Update SPECS_auto.csv --------------------------------------------------------

# download latest version of X-13 manual and write SPEC to SPECS_auto.csv
#
# source("noinst/specs/specs_from_pdf.R")


# load new specs data from csv --------------------------------------------


SPECS <- read_csv("noinst/specs/SPECS.csv") |>
  # Fix non-ascii characters. R CMD CHECK only allows ascii in data
  mutate(
    description = stringi::stri_trans_general(description, "latin-ascii")
  ) |>
  mutate(
    # Fix for estimate.regressioneffects. The beta should have a circumflex
    # but pdftools::pdf_text seens unable to reflect that.
    description = gsub("Xβ,b", "Xb", description),
    description = gsub("≤", "<=", description)
  )

# find differences between updated and previous version (if any) ----------

if(file.exists("data/SPECS.rda")) {
  SPECS_old_env <- new.env()
  load("data/SPECS.rda", SPECS_old_env)
  SPECS_old <- SPECS_old_env$SPECS

  differences <- bind_rows(
    SPECS |>
      filter(!long %in% SPECS_old$long) |>
      mutate(is_new = TRUE),
    SPECS_old |>
      filter(!long %in% SPECS$long) |>
      mutate(is_removed = TRUE)
  )

  # For a detailed overview of changes within lines
  # use `git diff noinst/spec/SPECS.csv`

  write_csv(differences, "noinst/specs/SPECS_added_removed.csv")
}


# Update data file --------------------------------------------------------

usethis::use_data(SPECS, overwrite = TRUE)


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
  SPECS |>
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
