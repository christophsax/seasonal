
shinyServer(function(input, output, session) {  
  output$info <- renderText({
    "hello"
    })
  output$html <- renderText({
#     txt <- out(m)[-153]
#     st <- grep('\\<body', txt, perl = TRUE) + 1
#     en <- grep('\\</body', txt, perl = TRUE) - 1
#     paste(txt[st:en], collapse = "\n")
    
    els = getNodeSet(doc.html, '//div[@id="rightnavigation"]')
    
    hh[[2]]
    
    hh <- getNodeSet(doc.html, '/html/body/div[1]/div')
    
    
#     
#     ee <- hh[[2]]
#     
    as(els[[1]], "character")
   
#     
#     txt <- out(m)[-153]
#     paste(txt, collapse = "\n")
  })
})


