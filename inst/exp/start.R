devtools::load_all(".")
syspath <- paste0(system.file(package = "seasonal"), "/inst/io/")  # if load.all

infile <- paste0(syspath, "test.mdl")

txt = paste(readLines(infile), collapse=" ")
p <- ParseCurly(txt)


rr <- ParseRound(p['regression'])
as.numeric(rr$b)

aa <- ParseRound(p['arima'])
as.numeric(aa$ma)




