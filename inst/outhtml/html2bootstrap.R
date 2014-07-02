shinyUI(
  bootstrapPage(title = NULL, responsive = TRUE,
                includeCSS("www/docs.css"),
                HTML(html.code)
                )
)


out



body <- getNodeSet(doc, '/html/body/div[1]/a')



length(body[[1]])

x <- xmlNode("a", 
             xmlNode("b", "1"),
             xmlNode("c", "1"),
             "some basic text")
x


b = newXMLNode("bob",
               namespace = c(r = "http://www.r-project.org",
                             omg = "http://www.omegahat.org"))

cat(saveXML(b), "\n")

addAttributes(b, a = 1, b = "xyz", "r:version" = "2.4.1", "omg:len" = 3)
cat(saveXML(b), "\n")

removeAttributes(b, "a", "r:version")
cat(saveXML(b), "\n")


removeAttributes(b, .attrs = names(xmlAttrs(b)))


addChildren(b, newXMLNode("el", "Red", "Blue", "Green",
                          attrs = c(lang ="en")))









library(XML)
rr <- readLines("~/Desktop/tmp/iofile.html")
doc = htmlTreeParse(rr, useInternalNodes=T)


body <- as(getNodeSet(doc, '/html/body/div[1]')[[1]], "character")

sidebar <- as(getNodeSet(doc, '/html/body/div[2]')[[1]], "character")



sidebar <- as(getNodeSet(doc, '/html/body/div[2]/ul[1]')[[1]], "character")

rr <- readLines("~/Desktop/tmp/iofile.html")

which(rr == "<body>")
which(rr == "</body>")


body <- rr[(which(rr == "<body>")+1):(which(rr == "</body>")-1)]
  
header <- '<!DOCTYPE html><html lang="en"><head><title>X-13ARIMA-SEATS Output</title><meta name="viewport" content="width=device-width, initial-scale=1.0"><link href="assets/css/bootstrap.css" rel="stylesheet">'

h2 <- '
    <script src="http://cdnjs.cloudflare.com/ajax/libs/jquery/2.0.3/jquery.min.js"></script>
    <!-- Include all compiled plugins (below), or include individual files as needed -->
    <script src="http://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.0.2/js/bootstrap.min.js"></script>
</head>
<body data-spy="scroll" data-target="#affix-nav">
<div class = "container">

<div class="row">
<nav id="affix-nav" class="sidebar col-md-4">'



h4 <- '
</nav>
<div class="col-md-8" role="main">
'


footer <- '</div></div></div>
<script src="//code.jquery.com/jquery-1.10.2.min.js"></script>
<script src="//netdna.bootstrapcdn.com/bootstrap/3.0.3/js/bootstrap.min.js"></script>
<script src="//cdnjs.cloudflare.com/ajax/libs/holder/2.2.0/holder.min.js"></script>
<script src="public/js/application.js"></script>
</body></html>'
  
writeLines(c(header, h2, sidebar, h4, body, footer), file("~/Desktop/neu.html"))
browseURL("~/Desktop/neu.html")




readLines

as(bd[[1]], "character")


htmlmode system var


out function, depending on sys var








