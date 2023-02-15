library(tidyverse)

path_old_SPECS <- "noinst/specs/specs_old.csv"
path_new_SPECS <- "noinst/specs/SPECS.csv"

oldspecs <- read_csv(path_old_SPECS)
newspecs <- read_csv(path_new_SPECS)

setdiff(oldspecs$long, newspecs$long)

setdiff(newspecs$long, oldspecs$long)
