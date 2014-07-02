library(shiny)
library(seasonal)

m <- seas(AirPassengers)
# 
# inspect2(m)

txt <- out(m)
library(XML)
doc = htmlTreeParse(txt, useInternalNodes=T)



rn = getNodeSet(doc, '//div[@id="rightnavigation"]')



html.code <- as(rn[[1]], "character")


bd <- getNodeSet(doc, '/html/body/div[1]')
html.code <- as(bd[[1]], "character")
