

library(shiny)

shinyServer(function(input, output) {
  
   
  output$table <- renderDataTable({
    
    input<-input$text
    out<-predictword(input=input)
    out
  })
  
})
