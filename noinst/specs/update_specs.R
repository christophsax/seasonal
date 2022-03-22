
# Corrections
# SPECS[1,1]  <- "automdl.autochoice"
# SPECS$spec <- as.character(SPECS$spec)
# SPECS$requires <- as.character(SPECS$requires)
# readr::write_csv(SPECS, "noinst/specs/SPECS.csv")

# load SPECS data from .csv and store as data/SPECS.RData
SPECS <- read.csv("noinst/specs/SPECS.csv", stringsAsFactors = FALSE)

save(SPECS, file = "data/specs.RData")
