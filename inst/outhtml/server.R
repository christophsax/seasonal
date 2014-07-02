
shinyServer(function(input, output, session) {  

  output$nav <- renderText({
    rn = getNodeSet(doc, '//div[@id="rightnavigation"]')
    as(rn[[1]], "character")
  })
  
  output$main <- renderText({
    bd <- getNodeSet(doc, '/html/body/div[1]')
    as(bd[[1]], "character")
  })
})


