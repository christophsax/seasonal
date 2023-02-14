
# Update SPECS.csv --------------------------------------------------------

source("noinst/specs/specs_from_pdf.R")


# update internal data file ----------------------------------------------------


# load SPECS data from .csv and store as data/SPECS.RData
SPECS <- read.csv("noinst/specs/SPECS.csv", stringsAsFactors = FALSE)

save(SPECS, file = "data/specs.RData", version = 2)  # version 3 requires >= R3.5





# update roxygen header (carfully review!) -------------------------------------

# Would be nice if we could link or include the description from X-13 manual

library(tidyverse)

txt <- read_lines("R/series.R")
line0 <- which(txt == "#' **spec** \\tab **long name** \\tab **short name** \\tab **description** \\cr")

linen <- which(txt == "#' }")[1]
txt[line0:linen]


#' **spec** \tab **long name** \tab **short name** \tab **description** \cr
SPECS <- as_tibble(SPECS)

# why this? series, but not save
# # A tibble: 5 × 6
#   long                       short spec      is.save is.series requires
#   <chr>                      <chr> <chr>     <lgl>   <lgl>     <chr>
# 1 composite.indmovseasrat    ims   composite FALSE   TRUE      ""
# 2 composite.indreplacsi      id9   composite FALSE   TRUE      ""
# 3 composite.indresidualseasf irf   composite FALSE   TRUE      ""
# 4 composite.ratioplotindsa   ir2   composite FALSE   TRUE      ""
# 5 composite.ratioplotorig    ir1   composite FALSE   TRUE      ""

tbl_txt <-
  SPECS |>
  filter(
    is.series == TRUE,
    is.save == TRUE
  ) |>
  select(spec, long, short, description) |>
  transmute(new = paste("#'", spec, "\\tab", long, "\\tab", short, "\\tab", description, "\\cr")) |>
  pull(new)

txt_new <-
  c(
    txt[1:line0],
    tbl_txt,
    txt[linen:length(txt)]
  )


write_lines(txt_new, "R/series.R")
