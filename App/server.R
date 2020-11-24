library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$text <- renderText(input$slider1) 
  dfb
})
