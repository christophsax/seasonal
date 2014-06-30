
shinyServer(function(input, output, session) {  
  output$info <- renderText({
    "hello"
    })
  output$html <- renderText({
#     txt <- out(m)[-153]
#     st <- grep('\\<body', txt, perl = TRUE) + 1
#     en <- grep('\\</body', txt, perl = TRUE) - 1
#     paste(txt[st:en], collapse = "\n")
    
    txt <- out(m)[-153]
    paste(txt, collapse = "\n")
  })
})


