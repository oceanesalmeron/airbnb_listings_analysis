library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("AirBnB listings analysis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h1("Move the slider!"),
      sliderInput("slider1", "Slide Me!", 0, 100, 0)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3("Slider value: "),
      textOutput("text")
      
      #plotOutput("distPlot")
    )
  )
))
