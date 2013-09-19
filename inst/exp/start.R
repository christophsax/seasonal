devtools::load_all(".")
syspath <- paste0(system.file(package = "seasonal"), "/inst/io/")  # if load.all
infile <- paste0(syspath, "test.mdl")
outfile <- paste0(syspath, "test2.mdl")

y <- ReadX13(infile)
WriteX13(y, file = outfile)

