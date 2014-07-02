library(shiny)
library(seasonal)

m <- seas(AirPassengers)


library(seasonal)
m <- seas(AirPassengers)
# 
# inspect2(m)

txt <- out(m)
library(XML)
doc.html = htmlTreeParse(txt, useInternalNodes=T)
